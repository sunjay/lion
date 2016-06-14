use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;

use math::rich_number::Unit;

use parser::ast::Function;

pub struct ConversionTable {
    // (from unit, to unit) : Function definition
    converters: HashMap<(Unit, Unit), Function>,
    // from unit : all other associated units
    associations: HashMap<Unit, Vec<Unit>>,
}

impl ConversionTable {
    pub fn new() -> ConversionTable {
        ConversionTable {
            converters: HashMap::new(),
            associations: HashMap::new(),
        }
    }

    pub fn get_converter(&self, from: Unit, to: Unit) -> Option<Function> {
        self.converters.get(&(from, to)).map(|v| v.clone())
    }

    pub fn define_conversion(&mut self, from: Unit, to: Unit, converter: Function) {
        assert!(from != to, "Cannot convert from the same unit to the same unit");

        self.converters.insert((from, to), converter);

        let assoc = self.associations.entry(from).or_insert(Vec::new());
        if !assoc.contains(&to) {
            assoc.push(to);
        }
    }

    pub fn conversion_steps(&mut self, start: Unit, target: Unit) -> Option<Vec<Unit>> {
        // Technically this is a valid conversion, there are just no steps to take
        if start == target {
            return Some(vec![]);
        }

        let mut shortest: Option<Vec<Unit>> = None;

        let mut open: LinkedList<(Unit, Vec<Unit>)> = LinkedList::new();
        open.push_back((start, Vec::new()));

        let mut seen = HashSet::new();

        while !open.is_empty() {
            let (unit, path) = open.pop_front().unwrap();
            if seen.contains(&unit) {
                continue;
            }
            seen.insert(unit);

            // Dead end
            if !self.associations.contains_key(&unit) {
                continue;
            }

            for next_unit in self.associations.get(&unit).unwrap() {
                let mut next_path = path.clone();
                next_path.push(*next_unit);

                // no path is always shorter than the current path
                let shorter = shortest.as_ref().map_or(true,
                    |ref p| next_path.len() < p.len());

                if *next_unit == target {
                    if shorter {
                        shortest = Some(next_path);
                    }
                }
                // Haven't found target, only continue if this
                // will result in a shorter path than the current
                // shortest path
                else if shorter {
                    // continue searching at the next unit
                    open.push_back((*next_unit, next_path));
                }
            }
        }

        shortest
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::ast::{Function, Expr};
    use math::rich_number::Unit;

    #[test]
    #[should_panic(expected = "same unit")]
    fn reject_defining_conversion_to_same_unit() {
        let unit: Unit = Default::default();

        let fake_func: Function = Function {
            params: Vec::new(),
            body: Expr::new(),
        };

        ConversionTable::new().define_conversion(unit, unit, fake_func);
    }

    #[test]
    fn conversion_to_same_unit() {
        let unit: Unit = Default::default();

        let mut table = ConversionTable::new();
        assert_eq!(table.conversion_steps(unit, unit).unwrap(), vec![]);
    }

    #[test]
    fn varying_conversion_steps() {
        // These numbers are arbitrary
        const mm: Unit = 3;
        const cm: Unit = 1;
        const m: Unit = 2;
        const km: Unit = 5;

        let fake_func: Function = Function {
            params: Vec::new(),
            body: Expr::new(),
        };

        let mut table = ConversionTable::new();

        table.define_conversion(mm, cm, fake_func.clone());
        table.define_conversion(cm, m, fake_func.clone());
        table.define_conversion(m, km, fake_func.clone());

        // Basic forward conversions
        assert_eq!(table.conversion_steps(mm, cm).unwrap(), vec![cm]);
        assert_eq!(table.conversion_steps(mm, m).unwrap(), vec![cm, m]);
        assert_eq!(table.conversion_steps(mm, km).unwrap(), vec![cm, m, km]);

        assert_eq!(table.conversion_steps(cm, mm), None);
        assert_eq!(table.conversion_steps(cm, m).unwrap(), vec![m]);
        assert_eq!(table.conversion_steps(cm, km).unwrap(), vec![m, km]);

        assert_eq!(table.conversion_steps(m, mm), None);
        assert_eq!(table.conversion_steps(m, cm), None);
        assert_eq!(table.conversion_steps(m, km).unwrap(), vec![km]);

        assert_eq!(table.conversion_steps(km, mm), None);
        assert_eq!(table.conversion_steps(km, cm), None);
        assert_eq!(table.conversion_steps(km, m), None);

        // Did not define the backwards conversions yet
        assert_eq!(table.conversion_steps(km, mm), None);

        // These are being added in addition to the previous forward conversions
        table.define_conversion(km, m, fake_func.clone());
        table.define_conversion(m, cm, fake_func.clone());
        table.define_conversion(cm, mm, fake_func.clone());

        // Test forward once again to make sure this doesn't mess that up
        assert_eq!(table.conversion_steps(mm, cm).unwrap(), vec![cm]);
        assert_eq!(table.conversion_steps(mm, m).unwrap(), vec![cm, m]);
        assert_eq!(table.conversion_steps(mm, km).unwrap(), vec![cm, m, km]);

        assert_eq!(table.conversion_steps(cm, m).unwrap(), vec![m]);
        assert_eq!(table.conversion_steps(cm, km).unwrap(), vec![m, km]);

        assert_eq!(table.conversion_steps(m, km).unwrap(), vec![km]);

        // Test backward conversions
        assert_eq!(table.conversion_steps(km, m).unwrap(), vec![m]);
        assert_eq!(table.conversion_steps(km, cm).unwrap(), vec![m, cm]);
        assert_eq!(table.conversion_steps(km, mm).unwrap(), vec![m, cm, mm]);

        assert_eq!(table.conversion_steps(m, cm).unwrap(), vec![cm]);
        assert_eq!(table.conversion_steps(m, mm).unwrap(), vec![cm, mm]);

        assert_eq!(table.conversion_steps(cm, mm).unwrap(), vec![mm]);

        // Short circuit the conversions
        table.define_conversion(m, mm, fake_func.clone());

        // Ensure the shortest path is always taken
        assert_eq!(table.conversion_steps(m, mm).unwrap(), vec![mm]);
        assert_eq!(table.conversion_steps(km, mm).unwrap(), vec![m, mm]);
    }
}
