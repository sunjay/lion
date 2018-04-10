pub type Unit = usize;

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
}
