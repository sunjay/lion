use std::collections::HashMap;

use ast::*;
use rich_number::RichNumber;

pub struct EvalContext {
    symbol_table: HashMap<String, ContextItem>,
}

#[derive(PartialEq, Debug)]
pub enum ContextItem {
    Number(RichNumber),
    Definition {
        // number from 0 to 9 where 9 is the highest precedence
        precedence: u8,
        fixity: Fixity,
        function: Function,
    },
    Constant(String),
}

#[derive(Eq, PartialEq, Debug)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix,
}

impl ContextItem {
    pub fn unwrap_number(self) -> RichNumber {
        match self {
            ContextItem::Number(num) => num,
            _ => panic!("Expected to unwrap a number"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum EvalError {
    NotFound(String),
}

pub type EvalResult = Result<ContextItem, EvalError>;

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            symbol_table: HashMap::new(),
        }
    }

    /// Gets a single value without evaluating it from the current context
    pub fn get(&self, name: &str) -> Option<ContextItem> {
        unimplemented!();
    }

    /// Convenience method for setting a number in the context
    pub fn set_number(&mut self, name: &str, value: RichNumber) {
        self.set(name, ContextItem::Number(value))
    }

    /// Adds to the current context (silently replaces if already present)
    pub fn set(&mut self, name: &str, value: ContextItem) {
        unimplemented!();
    }

    pub fn apply(&mut self, statement: Statement) -> EvalResult {
        // Builds symbol tree
        // Performs beta reduction
        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
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

    fn apply_string(mut context: &mut EvalContext, string: &str) {
        apply_program(context, parse(string).unwrap());
    }

    fn apply_program(mut context: &mut EvalContext, program: Program) {
        for statement in program {
            context.apply(statement);
        }
    }

    fn parse(string: &str) -> ::parser::ParseResult<Program> {
        unimplemented!();
    }
}

