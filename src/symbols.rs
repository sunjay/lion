use std::collections::HashMap;

use ast::*;

pub type IdentID = usize;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    symbols: HashMap<IdentID, Ident<'a>>,
}
