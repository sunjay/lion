use unit_graph::UnitGraph;
use symbols::SymbolTable;

#[derive(Debug, Clone, Default)]
pub struct Context<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}
