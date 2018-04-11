pub type Unit = usize;

#[derive(Debug, Clone)]
pub struct UnitGraph {
}

impl UnitGraph {
    pub fn new() -> Self {
        UnitGraph {
        }
    }

    /// Returns a unit representing when a quantity is unitless
    ///
    /// In lion programs, this is denoted '_
    pub fn unitless(&self) -> Unit {
        0 //TODO: Return a unit for the '_ unit
    }

    /// Returns the unique unit ID for the given unit name.
    /// New ID will be created and associated with the unit name if necessary
    ///
    /// Note: name must be given without "'", i.e. to lookup 'foo, use lookup("foo")
    pub fn lookup(&mut self, name: &str) -> Unit {
        0 //TODO
    }
}
