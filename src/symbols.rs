use std::collections::HashMap;

use rust_decimal::Decimal;

use ast::*;
use ir;
use canonical::CanonicalUnit;

#[derive(Debug, Clone)]
pub enum SymType<'a> {
    Variable {
        unit: CanonicalUnit,
        /// Only stored so it can be reprinted in the same notation as the user
        printable: UnitExpr<'a>,
    },
    Constant {
        value: Decimal,
        unit: UnitExpr<'a>,
    },
    Function {
        body: ir::Function<'a>,
    },
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    //TODO: This representation will need to be refactored outside of the interpreter
    symbol_types: HashMap<Ident<'a>, SymType<'a>>,
}
