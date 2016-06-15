use math::rich_number::RichNumber;

use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_context::{EvalContext, EvalResult, EvalError};

pub fn operator(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 4));
    
    //TODO: Unpack each argument form its context item and Err if it isn't the expected type
    unimplemented!();

    Ok(ContextItem::Nothing)
}

pub fn define_unit(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    //TODO: Unpack argument into string
    let unit_name: &str = unimplemented!();

    let unit = context.create_unit(unit_name);

    context.define_built_in_method_defaults(
        unit_name,
        1,
        BuiltInFunction::new(move |context, params| {
            try!(expect_params(&params, 1));

            //TODO: Unpack argument into RichNumber
            let value: RichNumber = unimplemented!();

            context.convert(value, Some(unit))
        }),
    );

    Ok(ContextItem::Nothing)
}

pub fn convert(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 2));

    unimplemented!();
}

pub fn unit_for(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    unimplemented!();
}

pub fn value_of(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    unimplemented!();
}

pub fn conversion(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 3));

    unimplemented!();
}

pub fn if_condition(context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 3));

    unimplemented!();
}

fn expect_params(params: &Vec<ContextItem>, nparams: usize) -> Result<(), EvalError> {
    if params.len() != nparams {
        Err(EvalError::ExpectedParams(nparams))
    }
    else {
        Ok(())
    }
}

