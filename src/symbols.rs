use std::collections::HashMap;

use rust_decimal::Decimal;

use ast::*;
use ir;
use canonical::CanonicalUnit;

pub struct DuplicateSymbol<'a>(pub Ident<'a>);

#[derive(Debug, Clone)]
enum SymType<'a> {
    Constant {
        value: Decimal,
        unit: CanonicalUnit,
        span: Span<'a>,
    },
    Function(ir::Function<'a>, Span<'a>),
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    //TODO: This representation will need to be refactored outside of the interpreter
    table: HashMap<Ident<'a>, SymType<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub fn insert_const(&mut self, name: Ident<'a>, value: Decimal, unit: CanonicalUnit, span: Span<'a>) -> Result<(), DuplicateSymbol> {
        if self.table.contains_key(&name) {
            return Err(DuplicateSymbol(name));
        }

        self.table.insert(name, SymType::Constant {value, unit, span});
        Ok(())
    }

    pub fn insert_function(&mut self, name: Ident<'a>, function: ir::Function<'a>, span: Span<'a>) -> Result<(), DuplicateSymbol> {
        if self.table.contains_key(&name) {
            return Err(DuplicateSymbol(name));
        }

        self.table.insert(name, SymType::Function(function, span));
        Ok(())
    }
}
