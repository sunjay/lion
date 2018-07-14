use std::collections::HashMap;

use nom::types::CompleteStr;

use canonical::CanonicalUnit;
use ast::*;

pub type UnitID = usize;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UndeclaredUnit<'a>(UnitName<'a>);

/// Represents a set of conversion functions
pub type Conversions<'a> = HashMap<(CanonicalUnit, CanonicalUnit), (FnArgs<'a>, Block<'a>)>;

#[derive(Debug, Clone)]
pub struct UnitGraph<'a> {
    next_id: UnitID,
    unit_ids: HashMap<UnitName<'a>, UnitID>,
    unit_symbols: HashMap<UnitID, (UnitName<'a>, Span<'a>)>,
    conversions: Conversions<'a>,
}

impl<'a> Default for UnitGraph<'a> {
    fn default() -> Self {
        let mut graph = UnitGraph {
            next_id: Default::default(),
            unit_ids: Default::default(),
            unit_symbols: Default::default(),
            conversions: Default::default(),
        };
        graph.insert_unit(UnitName::unitless(), Span::new(CompleteStr("")));

        graph
    }
}

impl<'a> UnitGraph<'a> {
    /// Returns a unit representing when a quantity is unitless
    ///
    /// In lion programs, this is denoted '_
    pub fn unitless(&self) -> UnitID {
        self.unit_id(UnitName::unitless())
            .expect("bug: '_ was not defined")
    }

    /// Returns the unique unit ID for the given unit name.
    /// Returns an Err if the name was not already a defined unit
    ///
    /// **Note:** name must be given without "'", i.e. to lookup 'foo, use unit_id("foo")
    pub fn unit_id(&self, name: UnitName<'a>) -> Result<UnitID, UndeclaredUnit> {
        self.unit_ids.get(&name).map(|id| *id).ok_or_else(|| UndeclaredUnit(name))
    }

    /// Returns the unit name for the given unit ID
    ///
    /// # Panics
    ///
    /// Panics if the unit ID was not declared since that should not be possible
    pub fn unit_name(&self, unit: UnitID) -> UnitName<'a> {
        match self.unit_symbols.get(&unit) {
            Some(&(name, _)) => name,
            None => unreachable!("Looked up an ID that did not exist"),
        }
    }

    /// Creates the given unit if it does not exist yet
    ///
    /// If it does exist, this does nothing
    pub fn insert_unit(&mut self, name: UnitName<'a>, span: Span<'a>) {
        if !self.unit_ids.contains_key(&name) {
            self.next_id += 1;
        }

        let id = self.next_id - 1;
        let id = *self.unit_ids.entry(name).or_insert(id);
        self.unit_symbols.entry(id).or_insert((name, span));
    }

    /// Adds the given conversion function to the graph
    ///
    /// If the conversion is already defined but overwrite is false, this will return an error
    pub fn add_conversion(&mut self, from: UnitExpr, to: UnitExpr, overwrite: bool) -> Result<(), ()> {
        //TODO: Check if all units in CanonicalUnits are declared and return an Err if not
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unitless_exists() {
        let units = UnitGraph::default();
        // Test to make sure that if we lookup unitless from elsewhere in the AST we get the same
        // value back as calling units.unitless()
        assert_eq!(units.unitless(), units.unit_id(UnitName::unitless()).unwrap());
        // Check that the unit name of the return value of units.unitless() is unitless
        assert_eq!(units.unit_name(units.unitless()), UnitName::unitless());
    }
}
