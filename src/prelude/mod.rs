/// The definitions for every function used in prelude
/// "The built in functions"
/// These special functions allow for things like defining operators,
/// performing basic operations and special math functions

use math::rich_number::RichNumber;

use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalResult, EvalError};
use eval::built_in_function::BuiltInFunction;

pub fn setup_prelude(context: &mut EvalContext) {
    define_math(context);
}

fn define_math(context: &mut EvalContext) {
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

fn define_built_in(
    context: &mut EvalContext,
    name: &str,
    params: usize,
    function: BuiltInFunction,
) {
    let item = ContextItem::built_in_defaults(function, params);
    context.set(name, item);
}

