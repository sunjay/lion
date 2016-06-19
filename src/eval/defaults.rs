use math::rich_number::RichNumber;

use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_context::{EvalContext, EvalResult, EvalError};

pub fn setup_defaults(context: &mut EvalContext) {
    setup_fixity_constants(context);

    context.define_built_in_method_defaults(
        "operator",
        4,
        BuiltInFunction::new(operator),
    );

    context.define_built_in_method_defaults(
        "defineUnit",
        1,
        BuiltInFunction::new(define_unit),
    );

    context.define_built_in_method_defaults(
        "convert",
        2,
        BuiltInFunction::new(convert),
    );

    context.define_built_in_method_defaults(
        "unitFor",
        1,
        BuiltInFunction::new(unit_for),
    );

    context.define_built_in_method_defaults(
        "valueOf",
        1,
        BuiltInFunction::new(value_of),
    );

    context.define_built_in_method_defaults(
        "conversion",
        3,
        BuiltInFunction::new(conversion),
    );
}

pub fn setup_fixity_constants(context: &mut EvalContext) {
    context.set_constant("PREFIX", Fixity::Prefix.to_string());
    context.set_constant("INFIX", Fixity::Infix.to_string());
    context.set_constant("POSTFIX", Fixity::Postfix.to_string());
}

pub fn operator(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 4));
    
    try!(expect_param_is(params[0].is_constant(),
        "Fixity must be one of the constants PREFIX, INFIX, or POSTFIX"));
    try!(expect_param_is(params[1].is_number(),
        "Precedence must be a numeric value"));
    try!(expect_param_is(params[2].is_constant(),
        "Name of the operator must be a string literal"));
    try!(expect_param_is(params[3].is_function(),
        "Function definition must be either an existing function or an anonymous function"));

    let fixity = {
        let fx = Fixity::from_str(&params.remove(0).unwrap_constant());
        try!(expect_param_is(fx.is_some(),
            "Fixity must be one of the constants PREFIX, INFIX, or POSTFIX"));
        fx.unwrap()
    };

    let precedence = {
        let p = params.remove(0).unwrap_number();
        try!(expect_param_is(p.is_dimensionless(),
            "Precedence cannot have a unit"));
        p.value as u8
    };

    let name = &params.remove(0).unwrap_constant();
    let function = params.remove(0);

    debug_assert!(params.is_empty(), "Not parameters used");
    
    match function {
        ContextItem::Definition { function, .. } => {
            context.define(name, fixity, precedence, function);
        },
        ContextItem::BuiltInMethod { function, params, .. } => {
            context.define_built_in_method(name, fixity, precedence, params, function)
        },
        _ => unreachable!(),
    };

    Ok(ContextItem::Nothing)
}

pub fn define_unit(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    try!(expect_param_is(params[0].is_constant(),
        "Name of the unit must be a string literal"));

    let unit_name = &params.remove(0).unwrap_constant();
    debug_assert!(params.is_empty(), "Not parameters used");

    let unit = context.create_unit(unit_name);

    context.define_built_in_method_defaults(
        unit_name,
        1,
        BuiltInFunction::new(move |context, mut params| {
            try!(expect_params(&params, 1));
            try!(expect_param_is(params[0].is_number(),
                "Value to convert must be numeric"));

            let value = params.remove(0).unwrap_number();
            debug_assert!(params.is_empty(), "Not parameters used");

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

fn expect_params(params: &Vec<ContextItem>, nparams: usize) -> Result<(), EvalError> {
    if params.len() != nparams {
        Err(EvalError::ExpectedParams(nparams))
    }
    else {
        Ok(())
    }
}

fn expect_param_is(cond: bool, message: &str) -> Result<(), EvalError> {
    if cond {
        Ok(())
    }
    else {
        Err(EvalError::InvalidParam(message.to_owned()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    use eval::fixity::Fixity;
    use eval::eval_context::EvalContext;

    #[test]
    fn fixity_constants() {
        let mut context = EvalContext::new();
        setup_fixity_constants(&mut context);

        let lookup_fixity = |name| Fixity::from_str(&context.get(name).unwrap().unwrap_constant()).unwrap();

        assert_eq!(lookup_fixity("PREFIX"), Fixity::Prefix);
        assert_eq!(lookup_fixity("INFIX"), Fixity::Infix);
        assert_eq!(lookup_fixity("POSTFIX"), Fixity::Postfix);
    }
}

