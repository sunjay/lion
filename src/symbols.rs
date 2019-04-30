use std::collections::HashMap;

use crate::ast::{Ident, Span};
use crate::ir::{Number, Function};

pub struct DuplicateSymbol<'a>(pub Ident<'a>);

#[derive(Debug, Clone)]
enum SymType<'a> {
    Constant {
        value: Number,
        span: Span<'a>,
    },
    Function(Function<'a>, Span<'a>),
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    //TODO: This representation will need to be refactored outside of the interpreter
    table: HashMap<Ident<'a>, SymType<'a>>,
}

impl<'a> SymbolTable<'a> {
    /// Returns the value of the constant with the given name if and only if a constant with that
    /// name is declared
    pub fn get_const(&self, name: Ident<'a>) -> Option<&Number> {
        self.table.get(&name).and_then(|sym| match sym {
            SymType::Constant {value, ..} => Some(value),
            _ => None,
        }).or(None)
    }

    pub fn insert_const(&mut self, name: Ident<'a>, value: Number, span: Span<'a>) -> Result<(), DuplicateSymbol> {
        if self.table.contains_key(&name) {
            return Err(DuplicateSymbol(name));
        }

        self.table.insert(name, SymType::Constant {value, span});
        Ok(())
    }

    pub fn insert_function(&mut self, name: Ident<'a>, function: Function<'a>, span: Span<'a>) -> Result<(), DuplicateSymbol> {
        if self.table.contains_key(&name) {
            return Err(DuplicateSymbol(name));
        }

        self.table.insert(name, SymType::Function(function, span));
        Ok(())
    }
}
