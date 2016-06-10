/// The definitions for every function used in prelude
/// "The built in functions"
/// These special functions allow for things like defining operators,
/// performing basic operations and special math functions

use rich_number::RichNumber;
use eval_context::{ContextItem, EvalContext, EvalResult, BuiltInFunction};

pub fn setup_prelude(mut context: &mut EvalContext) {
    define_math(context);
}

fn define_math(mut context: &mut EvalContext) {
}

fn define_built_in(
    mut context: &mut EvalContext,
    name: &str,
    function: BuiltInFunction,
    params: usize,
) {
    let item = ContextItem::built_in_defaults(function, params);
    context.set(name, item);
}

