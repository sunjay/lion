use std::collections::HashMap;

use ast::{Function, Expr, Statement};
use rich_number::RichNumber;
use prelude::setup_prelude;

const LOWEST_PRECEDENCE: u8 = 0;
const HIGHEST_PRECEDENCE: u8 = 9;
const FUNCTION_PRECEDENCE: u8 = HIGHEST_PRECEDENCE;

#[derive(PartialEq, Debug)]
pub enum EvalError {
    UnknownSymbol(String),
}

pub type EvalResult = Result<ContextItem, EvalError>;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ContextItem {
    Number(RichNumber),
    Definition {
        // number from 0 to 9 where 9 is the highest precedence
        precedence: u8,
        fixity: Fixity,
        function: Function,
    },
    BuiltInMethod(fn(Vec<ContextItem>) -> EvalResult),
    Constant(String),
    Boolean(bool),
    Nothing,
}

impl ContextItem {
    pub fn unwrap_number(self) -> RichNumber {
        match self {
            ContextItem::Number(num) => num,
            _ => panic!("Expected to unwrap a Number"),
        }
    }

    pub fn unwrap_boolean(self) -> bool {
        match self {
            ContextItem::Boolean(value) => value,
            _ => panic!("Expected to unwrap a Boolean"),
        }
    }
}

pub struct EvalContext {
    symbol_table: HashMap<String, ContextItem>,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            symbol_table: HashMap::new(),
        }
    }

    pub fn prelude() -> EvalContext {
        let mut context = EvalContext::new();
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
    pub fn define(&mut self, name: &str, fixity: Fixity, precedence: u8, function: Function) {
        debug_assert!(precedence >= LOWEST_PRECEDENCE && precedence <= HIGHEST_PRECEDENCE);

        self.set(name, ContextItem::Definition {
            fixity: fixity,
            precedence: precedence,
            function: function,
        })
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
                self.define(&name, Fixity::Prefix, FUNCTION_PRECEDENCE, definition);
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
        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use api::parse;
    use rich_number::RichNumber;

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

