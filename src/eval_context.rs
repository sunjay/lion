use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;

use ast::{Function, Expr, Statement};
use rich_number::RichNumber;
use eval_tree_node::EvalTreeNode;
use prelude::setup_prelude;

const LOWEST_PRECEDENCE: u8 = 0;
const HIGHEST_PRECEDENCE: u8 = 9;
const FUNCTION_PRECEDENCE: u8 = HIGHEST_PRECEDENCE;

const FUNCTION_FIXITY: Fixity = Fixity::Prefix;

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

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct BuiltInFunction(Rc<Fn(Vec<ContextItem>) -> EvalResult>);

impl fmt::Debug for BuiltInFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<built-in function>")
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
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
    BuiltInMethod {
        precedence: u8,
        fixity: Fixity,
        params: usize,
        function: BuiltInFunction,
    },
    Constant(String),
    Boolean(bool),
    Nothing,
}

impl ContextItem {
    /// Creates a Definition ContextItem from a function
    /// with the defaults assumed for all functions
    pub fn function_defaults(function: Function) -> ContextItem {
        ContextItem::Definition {
            precedence: FUNCTION_PRECEDENCE,
            fixity: FUNCTION_FIXITY,
            function: function,
        }
    }

    /// Creates a BuiltInMethod ContextItem from a function
    /// with the defaults assumed for all functions
    pub fn built_in_defaults(function: BuiltInFunction, params: usize) -> ContextItem {
        ContextItem::BuiltInMethod {
            precedence: FUNCTION_PRECEDENCE,
            fixity: FUNCTION_FIXITY,
            params: params,
            function: function,
        }
    }

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

    /// The most reliable way to get the precedence of any ContextItem
    /// ContextItems with no defined precedence return None
    pub fn resolve_precedence(&self) -> Option<&u8> {
        match *self {
            ContextItem::Definition { ref precedence, .. } => Some(precedence),
            ContextItem::BuiltInMethod { ref precedence, .. } => Some(precedence),
            _ => None,
        }
    }

    /// The most reliable way to get the fixity of any ContextItem
    /// ContextItems with no defined fixity return None
    pub fn resolve_fixity(&self) -> Option<Fixity> {
        match *self {
            ContextItem::Definition { fixity, .. } => Some(fixity),
            ContextItem::BuiltInMethod { fixity, .. } => Some(fixity),
            _ => None,
        }
    }

    /// The most reliable way to get the number of parameters of any ContextItem
    /// ContextItems with no defined number of parameters return None
    pub fn resolve_params(&self) -> Option<usize> {
        match *self {
            ContextItem::Definition {
                function: Function {
                    ref params,
                    ..
                },
                ..
            } => Some(params.len()),
            ContextItem::BuiltInMethod { ref params, .. } => Some(*params),
            _ => None,
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
    pub fn define(
        &mut self,
        name: &str,
        fixity: Fixity,
        precedence: u8,
        function: Function,
    ) {
        debug_assert!(precedence >= LOWEST_PRECEDENCE && precedence <= HIGHEST_PRECEDENCE);

        self.set(name, ContextItem::Definition {
            fixity: fixity,
            precedence: precedence,
            function: function,
        });
    }

    /// Defines a function linked to actual code rather than an expression
    pub fn define_builtin_method(
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
                self.define(&name, FUNCTION_FIXITY, FUNCTION_PRECEDENCE, definition);
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

