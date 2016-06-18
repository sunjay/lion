use prelude::define_built_in;

use math::rich_number::RichNumber;

use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalError};
use eval::built_in_function::BuiltInFunction;

pub fn define_math(context: &mut EvalContext) {
    define_numeric_binary_op(context, "pow", |a, b| a.pow(b), false);
    define_numeric_binary_op(context, "mul", |a, b| a * b, false);

    define_numeric_binary_op(context, "add", |a, b| a + b, true);
    define_numeric_binary_op(context, "sub", |a, b| a - b, true);
    define_numeric_binary_op(context, "div", |a, b| a / b, true);
    define_numeric_binary_op(context, "mod", |a, b| a % b, true);

    define_boolean_binary_op(context, "eq", |a, b| a == b, true);
    define_boolean_binary_op(context, "ne", |a, b| a != b, true);

    //TODO: ge, le, gt, lt, neg, not, sin, cos, tan, etc.
}

fn define_boolean_binary_op<F: 'static>(context: &mut EvalContext, name: &str, operator: F, coerce_first: bool)
    where F: Fn(RichNumber, RichNumber) -> bool {

    define_binary_op(context, name,
        move |a, b| ContextItem::Boolean(operator(a, b)), coerce_first);
}

fn define_numeric_binary_op<F: 'static>(context: &mut EvalContext, name: &str, operator: F, coerce_first: bool)
    where F: Fn(RichNumber, RichNumber) -> RichNumber {

    define_binary_op(context, name,
        move |a, b| ContextItem::Number(operator(a, b)), coerce_first);
}

fn define_binary_op<F: 'static>(context: &mut EvalContext, name: &str, operator: F, coerce_first: bool)
    where F: Fn(RichNumber, RichNumber) -> ContextItem {

    const PARAMS_LENGTH: usize = 2;
    define_built_in(context, name, PARAMS_LENGTH, 
        BuiltInFunction::new(move |context, mut params| {
            if params.len() != PARAMS_LENGTH {
                return Err(EvalError::ExpectedParams(PARAMS_LENGTH))
            }

            if !params[0].is_number() || !params[1].is_number() {
                return Err(EvalError::InvalidParams("Expected two numbers".to_owned()));
            }

            let mut rhs = params.pop().unwrap().unwrap_number();
            let lhs = params.pop().unwrap().unwrap_number();

            if coerce_first {
                // May possibly attempt to convert in the other
                // direction too in the future
                rhs = try!(context.convert(rhs, lhs.unit)).unwrap_number();
            }

            Ok(operator(lhs, rhs))
        })
    );
}

#[cfg(test)]
mod tests {
    use ast::*;
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
}

