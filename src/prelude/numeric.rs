use prelude::{define_built_in, apply_program};

use math::rich_number::RichNumber;

use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalError};
use eval::built_in_function::BuiltInFunction;

const BINARY_PARAMS_LENGTH: usize = 2;

pub fn define_math(context: &mut EvalContext) {
    context.set_boolean("true", true);
    context.set_boolean("false", false);

    define_numeric_binary_op(context, "pow", |a, b| a.pow(b), |_, _| false);
    define_numeric_binary_op(context, "mul", |a, b| a * b, |_, _| false);

    define_numeric_binary_op(context, "add", |a, b| a + b, |_, _| true);
    define_numeric_binary_op(context, "sub", |a, b| a - b, |_, _| true);
    define_numeric_binary_op(context, "div", |a, b| a / b,
        |a, b| a.has_units() && b.has_units());
    define_numeric_binary_op(context, "mod", |a, b| a % b,
        |a, b| a.has_units() && b.has_units());

    define_boolean_binary_op(context, "eq", |a, b| a == b, true);
    define_boolean_binary_op(context, "ne", |a, b| a != b, true);

    apply_program(context, include_str!("math.lion"));

    //TODO: ge, le, gt, lt, neg, not, sin, cos, tan, etc.
}

/// Defines an operator that can take either booleans or numbers
/// Optionally coerces numbers into the same unit
/// Ensures that both arguments are numbers or both are booleans
fn define_boolean_binary_op<F: 'static>(context: &mut EvalContext, name: &str, operator: F, coerce_first: bool)
    where F: Fn(ContextItem, ContextItem) -> bool {

    define_built_in(context, name, BINARY_PARAMS_LENGTH, 
        BuiltInFunction::new(move |context, mut params| {
            if params.len() != BINARY_PARAMS_LENGTH {
                return Err(EvalError::ExpectedParams(BINARY_PARAMS_LENGTH))
            }

            // params can be either number or boolean but not both
            if !((params[0].is_number() && params[1].is_number())
                || (params[0].is_boolean() && params[1].is_boolean())) {
                return Err(EvalError::InvalidParam("Expecting numeric or boolean argument, cannot compare numeric values to boolean values".to_owned()));
            }

            let mut rhs = params.remove(0);
            let lhs = params.remove(0);
            debug_assert!(params.is_empty(), "Not all parameters used");

            if coerce_first && lhs.is_number() && rhs.is_number() {
                // May possibly attempt to convert in the other
                // direction too in the future
                let lhs_unit = match lhs {
                    ContextItem::Number(RichNumber { ref unit, .. }) => unit,
                    _ => unreachable!(),
                };
                rhs = try!(context.convert(rhs.unwrap_number(), *lhs_unit));
            }

            Ok(ContextItem::Boolean(operator(lhs, rhs)))
        })
    );
}

/// Defines an operator that can only take numeric arguments
/// Optionally takes a function used to define whether the numeric arguments should be coerced into the same unit
fn define_numeric_binary_op<F: 'static, G: 'static>(context: &mut EvalContext, name: &str, operator: F, coerce_first: G)
    where F: Fn(RichNumber, RichNumber) -> RichNumber,
          G: Fn(RichNumber, RichNumber) -> bool {

    define_built_in(context, name, BINARY_PARAMS_LENGTH, 
        BuiltInFunction::new(move |context, mut params| {
            if params.len() != BINARY_PARAMS_LENGTH {
                return Err(EvalError::ExpectedParams(BINARY_PARAMS_LENGTH))
            }

            if !params[0].is_number() || !params[1].is_number() {
                return Err(EvalError::InvalidParam("Expecting numeric argument".to_owned()));
            }

            let mut rhs = params.pop().unwrap().unwrap_number();
            let lhs = params.pop().unwrap().unwrap_number();
            debug_assert!(params.is_empty(), "Not all parameters used");

            if coerce_first(lhs, rhs) {
                // May possibly attempt to convert in the other
                // direction too in the future
                rhs = try!(context.convert(rhs, lhs.unit)).unwrap_number();
            }

            Ok(ContextItem::Number(operator(lhs, rhs)))
        })
    );
}

#[cfg(test)]
mod tests {
    use ast::*;

    use eval::context_item::ContextItem;
    use eval::eval_context::EvalContext;
    use math::rich_number::RichNumber;

    #[test]
    fn math_operators() {
        let mut context = EvalContext::prelude();

        // add 1 (sub (mul 2 (pow 4 3)) (div (mod 7 3) 10))
        // 1 + ((2 * (4 ** 3)) - ((7 % 3) / 10))
        assert_eq!(context.apply(Statement::Expression(vec![
            ExprItem::SingleTerm(Term::Symbol("add".to_owned())),
            ExprItem::SingleTerm(Term::Number(1f64)),
            ExprItem::Group(vec![
                ExprItem::SingleTerm(Term::Symbol("sub".to_owned())),
                ExprItem::Group(vec![
                    ExprItem::SingleTerm(Term::Symbol("mul".to_owned())),
                    ExprItem::SingleTerm(Term::Number(2f64)),
                    ExprItem::Group(vec![
                        ExprItem::SingleTerm(Term::Symbol("pow".to_owned())),
                        ExprItem::SingleTerm(Term::Number(4f64)),
                        ExprItem::SingleTerm(Term::Number(3f64)),
                    ]),
                ]),
                ExprItem::Group(vec![
                    ExprItem::SingleTerm(Term::Symbol("div".to_owned())),
                    ExprItem::Group(vec![
                        ExprItem::SingleTerm(Term::Symbol("mod".to_owned())),
                        ExprItem::SingleTerm(Term::Number(7f64)),
                        ExprItem::SingleTerm(Term::Number(3f64)),
                    ]),
                    ExprItem::SingleTerm(Term::Number(10f64)),
                ]),
            ]),
        ])).unwrap().unwrap_number(), RichNumber::from(128.9f64));
    }

    #[test]
    fn division_with_or_without_units() {
        let mut context = EvalContext::prelude();

        let fake_unit = "foounit";
        context.call("defineUnit", vec![
            ContextItem::Constant(fake_unit.to_owned()),
        ]).unwrap().unwrap();

        let unit = context.lookup_unit_name(fake_unit).unwrap();

        // 3 foounit / 2 should equal 1.5 foounit
        assert_eq!(context.apply(Statement::Expression(vec![
            ExprItem::SingleTerm(Term::Number(3f64)),
            ExprItem::SingleTerm(Term::Symbol(fake_unit.to_owned())),
            ExprItem::SingleTerm(Term::Symbol("/".to_owned())),
            ExprItem::SingleTerm(Term::Number(2f64)),
        ])).unwrap().unwrap_number(), RichNumber::from_unit(1.5f64, unit));

        // 3 foounit / 2 foounit should equal 1.5
        assert_eq!(context.apply(Statement::Expression(vec![
            ExprItem::SingleTerm(Term::Number(3f64)),
            ExprItem::SingleTerm(Term::Symbol(fake_unit.to_owned())),
            ExprItem::SingleTerm(Term::Symbol("/".to_owned())),
            ExprItem::SingleTerm(Term::Number(2f64)),
            ExprItem::SingleTerm(Term::Symbol(fake_unit.to_owned())),
        ])).unwrap().unwrap_number(), RichNumber::from(1.5f64));
    }

    #[test]
    fn can_compare_booleans_to_booleans() {
        let mut context = EvalContext::prelude();

        // true == false should be false and should not error
        assert_eq!(context.apply(Statement::Expression(vec![
            ExprItem::SingleTerm(Term::Symbol("true".to_owned())),
            ExprItem::SingleTerm(Term::Symbol("==".to_owned())),
            ExprItem::SingleTerm(Term::Symbol("false".to_owned())),
        ])).unwrap().unwrap_boolean(), false);
    }

    #[test]
    fn can_compare_numbers_to_numbers() {
        let mut context = EvalContext::prelude();

        // 2 == 2 should be true and should not error
        assert_eq!(context.apply(Statement::Expression(vec![
            ExprItem::SingleTerm(Term::Number(2f64)),
            ExprItem::SingleTerm(Term::Symbol("==".to_owned())),
            ExprItem::SingleTerm(Term::Number(2f64)),
        ])).unwrap().unwrap_boolean(), true);
    }
}

