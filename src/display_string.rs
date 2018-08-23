use unit_graph::UnitGraph;

pub trait DisplayString {
    fn display<'a>(&self, units: &UnitGraph<'a>) -> String;
}
