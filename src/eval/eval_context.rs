use std::rc::Rc;
use std::collections::HashMap;

use parser::ast::{Function, Expr, Statement};
use math::rich_number::RichNumber;
use math::conversion_table::ConversionTable;

use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_tree_node::EvalTreeNode;

use prelude::setup_prelude;

#[derive(PartialEq, Debug)]
pub enum EvalError {
    UndefinedSymbol(String),
    UnexpectedSymbols,
    ExpectedParams(usize),
    InvalidSymbolDefinition {
        expected_params: usize,
        actual_params: usize,
    },
}

pub type EvalResult = Result<ContextItem, EvalError>;

pub struct EvalContext {
    symbol_table: HashMap<String, ContextItem>,
    conversion_table: ConversionTable,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            symbol_table: HashMap::new(),
            conversion_table: ConversionTable::new(),
        }
    }

    /// Defines a reasonble set of default built_in methods
    pub fn defaults() -> EvalContext {
        let mut context = EvalContext::new();

        context.set_constant("PREFIX", Fixity::Prefix.to_string());
        context.set_constant("INFIX", Fixity::Infix.to_string());
        context.set_constant("POSTFIX", Fixity::Postfix.to_string());

        context.define_built_in_method_defaults(
            "operator",
            4,
            BuiltInFunction::new(defaults::define_operator),
        );

        context
    }

    /// Defines a context with all the defaults as well as definitions
    /// for common operators and units
    pub fn prelude() -> EvalContext {
        let mut context = EvalContext::defaults();
        setup_prelude(&mut context);
        context
    }

    /// Gets a single value without evaluating it from the current context
    pub fn get(&self, name: &str) -> Option<ContextItem> {
        self.symbol_table.get(name).map(|item| (*item).clone())
    }

    /// Convenience method for setting a number in the context
    pub fn set_number(&mut self, name: &str, value: RichNumber) {
        self.set(name, ContextItem::Number(value))
    }

    /// Defines a function in the context
    pub fn define(
        &mut self,
        name: &str,
        fixity: Fixity,
        precedence: u8,
        function: Function,
    ) {
        self.set(name, ContextItem::new_definition(fixity, precedence, function));
    }

    /// Defines a function in the context with defaults for precedence and fixity
    pub fn define_defaults(
        &mut self,
        name: &str,
        function: Function,
    ) {
        self.set(name, ContextItem::function_defaults(function));
    }

    /// Defines a function linked to actual code rather than an expression
    pub fn define_built_in_method(
        &mut self,
        name: &str,
        fixity: Fixity,
        precedence: u8,
        params: usize,
        function: BuiltInFunction,
    ) {

        self.set(name, ContextItem::BuiltInMethod {
            fixity: fixity,
            precedence: precedence,
            params: params,
            function: function,
        });
    }

    /// Defines a built-in function in the context with defaults for precedence and fixity
    pub fn define_built_in_method_defaults(
        &mut self,
        name: &str,
        params: usize,
        function: BuiltInFunction,
    ) {
        self.set(name, ContextItem::built_in_defaults(function, params));
    }

    /// Creates a constant value
    pub fn set_constant(&mut self, name: &str, value: String) {
        self.set(name, ContextItem::Constant(value));
    }

    /// Creates a boolean value
    pub fn set_boolean(&mut self, name: &str, value: bool) {
        self.set(name, ContextItem::Boolean(value));
    }

    /// Adds to the current context (silently replaces if already present)
    pub fn set(&mut self, name: &str, value: ContextItem) {
        self.symbol_table.insert(name.to_owned(), value);
    }

    pub fn apply(&mut self, statement: Statement) -> EvalResult {
        match statement {
            Statement::NamedFunction {name, definition} => {
                self.define_defaults(&name, definition);
                Ok(ContextItem::Nothing)
            },
            Statement::AnonymousFunction(_) => Ok(ContextItem::Nothing),
            Statement::Assignment {name, value} => {
                let value = try!(self.evaluate(value));
                self.set(&name, value);
                Ok(ContextItem::Nothing)
            },
            Statement::Expression(value) => self.evaluate(value),
        }
    }

    fn evaluate(&mut self, expr: Expr) -> EvalResult {
        let root = try!(EvalTreeNode::from_expr(self, expr));

        println!("{:#?}", root);

        unimplemented!();
    }
}

mod defaults {
    use std::rc::Rc;

    use super::{EvalContext, EvalResult, EvalError};
    use eval::context_item::ContextItem;
    use eval::built_in_function::BuiltInFunction;

    pub fn define_operator(context: &mut EvalContext, args: Vec<ContextItem>) -> EvalResult {
        try!(expect_args(&args, 4));
        
        //TODO: Unpack each argument form its context item and Err if it isn't the expected type

        Ok(ContextItem::Nothing)
    }
    
    fn expect_args(args: &Vec<ContextItem>, nargs: usize) -> Result<(), EvalError> {
        if args.len() != nargs {
            Err(EvalError::ExpectedParams(nargs))
        }
        else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use api::parse;

    use ast::*;
    use math::rich_number::RichNumber;

    #[test]
    fn get_set_in_context() {
        let mut context = EvalContext::new();
        assert_eq!(context.get("a"), None);

        let value = RichNumber::from(2f64);
        context.set_number("a", value.clone());
        assert_eq!(context.get("a").map(|n| n.unwrap_number()), Some(value));
    }

    #[test]
    fn define_variable() {
        let mut context = EvalContext::new();
        apply_string(&mut context, "x = 3");

        assert_eq!(context.get("x").map(|n| n.unwrap_number()), Some(RichNumber::from(3f64)));
    }

    #[test]
    fn basic_operators_and_precedence() {
        let result = apply_single("2 * 3 ** 2 - (100e-1 + -77.2) / 13").unwrap();
        assert_eq!(result.unwrap_number(), RichNumber::from(23.1692307692f64));
    }

    fn test_single_boolean(string: &str, expected: bool) {
        let result = apply_single(string).unwrap();
        assert_eq!(result.unwrap_boolean(), expected);
    }

    fn apply_single(string: &str) -> EvalResult {
        let mut context = EvalContext::new();
        let statement = parse_statement(string);

        context.apply(statement)
    }

    fn parse_statement(string: &str) -> Statement {
        let mut parsed = parse(string).unwrap();
        assert!(parsed.len() == 1, "parse_statement() was passed more than one statement");

        parsed.pop().unwrap()
    }

    fn apply_string(mut context: &mut EvalContext, string: &str) {
        apply_program(context, parse(string).unwrap());
    }

    fn apply_program(mut context: &mut EvalContext, program: Program) {
        for statement in program {
            context.apply(statement).expect("Error while applying statement");
        }
    }
}

