use prelude::apply_program;

use eval::eval_context::EvalContext;

pub fn define_units(context: &mut EvalContext) {
    apply_program(context, include_str!("units.lion"));
}

//TODO: Test unit & conversion definitions for prelude units

