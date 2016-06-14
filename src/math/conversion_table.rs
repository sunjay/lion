use std::collections::HashMap;

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

    pub fn define_conversion(&mut self, from: Unit, to: Unit, converter: Function) {
        self.converters.insert((from, to), converter);

        let assoc = self.associations.entry(from).or_insert(Vec::new());
        assoc.push(to);
    }

    pub fn conversion_steps(&mut self, from: Unit, to: Unit) -> Option<Vec<Unit>> {
        unimplemented!();
    }
}

