use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_context::{EvalContext, EvalResult, EvalError};

const UNIT_PRECEDENCE: u8 = 9;

pub fn setup_defaults(context: &mut EvalContext) {
    setup_fixity_constants(context);

    context.define_built_in_method_defaults(
        "operator",
        4,
        BuiltInFunction::new(operator),
    );

    context.define_built_in_method(
        "units",
        Fixity::Postfix,
        UNIT_PRECEDENCE,
        1,
        BuiltInFunction::new(remove_unit),
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
        BuiltInFunction::new(remove_unit),
    );

    context.define_built_in_method_defaults(
        "conversion",
        3,
        BuiltInFunction::new(conversion),
    );
}

fn setup_fixity_constants(context: &mut EvalContext) {
    context.set_constant("PREFIX", Fixity::Prefix.to_string());
    context.set_constant("INFIX", Fixity::Infix.to_string());
    context.set_constant("POSTFIX", Fixity::Postfix.to_string());
}

fn operator(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
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

    debug_assert!(params.is_empty(), "Not all parameters used");
    
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

fn remove_unit(_: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    try!(expect_param_is(params[0].is_number(),
        "Value to convert must be numeric"));

    let value = params.remove(0).unwrap_number();
    debug_assert!(params.is_empty(), "Not all parameters used");

    Ok(ContextItem::Number(value.without_units()))
}

fn define_unit(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    try!(expect_param_is(params[0].is_constant(),
        "Name of the unit must be a string literal"));

    let unit_name = &params.remove(0).unwrap_constant();
    debug_assert!(params.is_empty(), "Not all parameters used");

    let unit = context.create_unit(unit_name);

    context.define_built_in_method(
        unit_name,
        Fixity::Postfix,
        UNIT_PRECEDENCE,
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

fn convert(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 2));

    try!(expect_param_is(params[0].is_number(),
        "Value to convert must be numeric"));
    try!(expect_param_is(params[1].is_constant(),
        "Unit to convert to must be a string literal"));

    let value = params.remove(0).unwrap_number();
    let target_unit_name = params.remove(0).unwrap_constant();
    debug_assert!(params.is_empty(), "Not all parameters used");

    let unit = context.lookup_unit_name(&target_unit_name).unwrap();

    context.convert(value, Some(unit))
}

fn unit_for(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 1));

    try!(expect_param_is(params[0].is_number(),
        "Value to convert must be numeric"));

    let value = params.remove(0).unwrap_number();
    debug_assert!(params.is_empty(), "Not all parameters used");

    Ok(ContextItem::Constant(
        value.unit.map_or_else(
            || "units".to_owned(),
            // since all units are defined by the eval context itself,
            // we can trust unit constants to exist and use unwrap here
            |v| context.lookup_unit(v).unwrap()
        )
    ))
}

fn conversion(context: &mut EvalContext, mut params: Vec<ContextItem>) -> EvalResult {
    try!(expect_params(&params, 3));

    try!(expect_param_is(params[0].is_constant(),
        "Conversion base unit must be a string literal"));
    try!(expect_param_is(params[1].is_constant(),
        "Conversion target unit must be a string literal"));
    try!(expect_param_is(params[2].is_definition(),
        "Conversion definition must be either an existing function or an anonymous function, built-in functions are not currently supported"));
    //TODO: Support built-in functions

    let base_unit_name = params.remove(0).unwrap_constant();
    let target_unit_name = params.remove(0).unwrap_constant();
    let function = match params.remove(0) {
        ContextItem::Definition { function, .. } => function,
        _ => unreachable!(),
    };

    debug_assert!(params.is_empty(), "Not all parameters used");

    let base_unit = context.lookup_unit_name(&base_unit_name);
    let target_unit = context.lookup_unit_name(&target_unit_name);

    if base_unit.is_none() {
        Err(EvalError::UndefinedUnit(base_unit_name))
    }
    else if target_unit.is_none() {
        Err(EvalError::UndefinedUnit(target_unit_name))
    }
    else {
        context.define_conversion(base_unit.unwrap(), target_unit.unwrap(), function);

        Ok(ContextItem::Nothing)
    }
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

    use ast::*;

    use math::rich_number::RichNumber;
    
    use eval::fixity::Fixity;
    use eval::context_item::ContextItem;
    use eval::eval_context::EvalContext;

    #[test]
    fn fixity_constants() {
        let context = context_with_defaults();

        let lookup_fixity = |name| Fixity::from_str(&context.get(name).unwrap().unwrap_constant()).unwrap();

        assert_eq!(lookup_fixity("PREFIX"), Fixity::Prefix);
        assert_eq!(lookup_fixity("INFIX"), Fixity::Infix);
        assert_eq!(lookup_fixity("POSTFIX"), Fixity::Postfix);
    }

    #[test]
    fn can_define_operators_with_different_fixities() {
        let mut context = context_with_defaults();

        let fixity = Fixity::Prefix;
        let precedence: u8 = 0;
        let name = "foo".to_owned();

        let params = vec!["x".to_owned()];
        let function_body: Expr = vec![
            ExprItem::SingleTerm(Term::Symbol("x".to_owned())),
        ];

        assert_eq!(context.call("operator", vec![
            ContextItem::Constant(fixity.to_string()),
            ContextItem::Number(RichNumber::from(precedence as f64)),
            ContextItem::Constant(name.clone()),
            ContextItem::new_definition(
                // must be different from the other values for these
                Fixity::Postfix,
                precedence + 1,
                Function {
                    params: params.clone(),
                    body: function_body.clone(),
                },
            ),
        ]).unwrap().unwrap(), ContextItem::Nothing);

        let defined = context.get(&name).unwrap();
        
        match defined {
            ContextItem::Definition {
                precedence: dprecedence,
                fixity: dfixity,
                function,
            } => {
                assert_eq!(dprecedence, precedence);
                assert_eq!(dfixity, fixity);
                assert_eq!(function.params, params);
                assert_eq!(function.body, function_body);
            },
            _ => panic!("operator did not define a function"),
        }
    }

    #[test]
    fn can_get_units() {
        let mut context = context_with_defaults();

        let unit_name = "foo";
        let unit = context.create_unit(unit_name);

        assert_eq!(context.call("unitFor", vec![
            ContextItem::Number(RichNumber::from_unit(23f64, unit)),
        ]).unwrap().unwrap().unwrap_constant(), unit_name);

        assert_eq!(context.call("unitFor", vec![
            ContextItem::Number(RichNumber::from(23f64)),
        ]).unwrap().unwrap().unwrap_constant(), "units");
    }

    #[test]
    fn can_define_conversions() {
        let mut context = context_with_defaults();

        let foo = context.create_unit("foo");
        let bar = context.create_unit("bar");

        assert_eq!(context.call("conversion", vec![
            ContextItem::Constant("foo".to_owned()),
            ContextItem::Constant("bar".to_owned()),
            ContextItem::function_defaults(Function {
                params: vec!["x".to_owned()],
                body: vec![
                    ExprItem::SingleTerm(Term::Symbol("x".to_owned())),
                ],
            }),
        ]).unwrap().unwrap(), ContextItem::Nothing);

        assert!(context.convert(RichNumber::from_unit(64f64, foo), Some(bar)).is_ok());
        assert!(context.convert(RichNumber::from_unit(64f64, bar), Some(foo)).is_err());
    }

    fn context_with_defaults() -> EvalContext {
        let mut context = EvalContext::new();
        setup_defaults(&mut context);
        context
    }
}

