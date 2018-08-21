use std::collections::{HashMap, VecDeque, HashSet};

use nom::types::CompleteStr;
use petgraph::graph::{UnGraph, NodeIndex, DefaultIx};
use rust_decimal::Decimal;

use canonical::CanonicalUnit;
use ast::*;
use ir::ConversionRatio;

pub type UnitID = usize;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UndeclaredUnit<'a>(UnitName<'a>);

/// Attempt to insert a unit twice
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DuplicateUnit<'a>(UnitName<'a>);

#[derive(Debug, Clone)]
pub struct ConversionPath {
    start: CanonicalUnit,
    end: CanonicalUnit,
    ratio_path: Vec<ConversionRatio>,
}

impl ConversionPath {
    /// Reduces the path into the factor that transforms start to end
    pub fn conversion_factor(self) -> Decimal {
        let mut factor = Decimal::from(1);

        let mut current = self.start;
        for ratio in self.ratio_path {
            if ratio.left.unit == current {
                factor *= ratio.right.value / ratio.left.value;
                current = ratio.right.unit;
            }
            else if ratio.right.unit == current {
                factor *= ratio.left.value / ratio.right.value;
                current = ratio.left.unit;
            }
            else {
                unreachable!();
            }
        }

        factor
    }
}

#[derive(Debug, Clone)]
pub struct UnitGraph<'a> {
    unit_ids: HashMap<UnitName<'a>, UnitID>,
    units: Vec<(UnitName<'a>, Span<'a>)>,
    graph_ids: HashMap<CanonicalUnit, NodeIndex<DefaultIx>>,
    conversions: UnGraph<CanonicalUnit, ConversionRatio>,
}

impl<'a> Default for UnitGraph<'a> {
    fn default() -> Self {
        let mut graph = UnitGraph {
            unit_ids: Default::default(),
            units: Default::default(),
            graph_ids: Default::default(),
            conversions: Default::default(),
        };
        graph.insert_unit(UnitName::unitless(), Span::new(CompleteStr("")))
            .unwrap_or_else(|_| unreachable!("'_ was already declared"));

        graph
    }
}

impl<'a> UnitGraph<'a> {
    /// Returns a unit representing when a quantity is unitless
    ///
    /// In lion programs, this is denoted '_
    pub fn unitless(&self) -> UnitID {
        self.unit_id(UnitName::unitless())
            .unwrap_or_else(|_| unreachable!("bug: '_ was not defined"))
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
        match self.units.get(unit) {
            Some(&(name, _)) => name,
            None => unreachable!("Looked up an ID that did not exist"),
        }
    }

    /// Creates the given unit if it does not exist yet
    ///
    /// If it does exist, this function will return an error
    pub fn insert_unit(&mut self, name: UnitName<'a>, span: Span<'a>) -> Result<(), DuplicateUnit<'a>> {
        if self.unit_ids.contains_key(&name) {
            return Err(DuplicateUnit(name));
        }

        let id = self.unit_ids.len();
        assert!(self.unit_ids.insert(name, id).is_none(),
            "bug: failed to detect duplicate declaration");
        self.units.push((name, span));

        let node = CanonicalUnit::from(id);
        let graph_id = self.conversions.add_node(node.clone());
        assert!(self.graph_ids.insert(node, graph_id).is_none(),
            "bug: failed to detect duplicate declaration");

        Ok(())
    }

    /// Adds the given conversion ratio to the graph
    pub fn add_conversion(&mut self, ratio: ConversionRatio) {
        let left_id = if !self.graph_ids.contains_key(&ratio.left.unit) {
            self.conversions.add_node(ratio.left.unit.clone())
        } else { self.graph_ids[&ratio.left.unit] };
        let right_id = if !self.graph_ids.contains_key(&ratio.right.unit) {
            self.conversions.add_node(ratio.right.unit.clone())
        } else { self.graph_ids[&ratio.right.unit] };

        self.conversions.update_edge(left_id, right_id, ratio);
    }

    pub fn conversion_path(&self, start_unit: &CanonicalUnit, end_unit: &CanonicalUnit) -> Option<ConversionPath> {
        let start = match self.graph_ids.get(start_unit) {
            Some(node) => *node,
            None => return None,
        };
        let end = match self.graph_ids.get(end_unit) {
            Some(node) => *node,
            None => return None,
        };

        let mut queue = VecDeque::new();
        queue.push_back(vec![start]);
        let mut seen = HashSet::new();

        let node_path = loop {
            let path = match queue.pop_front() {
                Some(path) => path,
                None => return None, // Ran out of nodes to search
            };
            let node = path.last().cloned().unwrap();
            if seen.contains(&node) {
                continue;
            }
            seen.insert(node);

            if node == end {
                break path;
            }

            for adj in self.conversions.neighbors(node) {
                let mut adj_path = path.clone();
                adj_path.push(adj);
                queue.push_back(adj_path);
            }
        };

        let mut ratio_path = Vec::new();
        let mut last = node_path[0];
        for &node in &node_path[1..] {
            let edge_id = self.conversions.find_edge(last, node).unwrap();
            let ratio = self.conversions.edge_weight(edge_id).cloned().unwrap();
            ratio_path.push(ratio);
            last = node;
        }

        Some(ConversionPath {start: start_unit.clone(), end: end_unit.clone(), ratio_path})
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
