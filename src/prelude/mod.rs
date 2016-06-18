/// The definitions for every function used in prelude
/// "The built in functions"
/// These special functions allow for things like defining operators,
/// performing basic operations and special math functions

mod numeric;

use std::io::prelude::*;

use api::parse;

use eval::context_item::ContextItem;
use eval::eval_context::EvalContext;
use eval::built_in_function::BuiltInFunction;

use prelude::numeric::define_math;

pub fn setup_prelude(context: &mut EvalContext) {
    define_math(context);
}

pub fn define_built_in(
    context: &mut EvalContext,
    name: &str,
    params: usize,
    function: BuiltInFunction,
) {
    let item = ContextItem::built_in_defaults(function, params);
    context.set(name, item);
}

pub fn apply_program(context: &mut EvalContext, string: &str) {
    let prog = parse(string).expect("Parse error");

    for statement in prog {
        context.apply(statement);
    }
}

