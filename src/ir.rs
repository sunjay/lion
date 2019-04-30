//! Intermediate representation for type checked items

use bigdecimal::BigDecimal;
use crate::canonical::CanonicalUnit;
use crate::unit_graph::UnitGraph;
use crate::display_string::DisplayString;

use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'i> {
    pub attrs: Vec<ast::Attribute<'i>>,
    pub name: ast::Ident<'i>,
    pub args: ast::FnArgs<'i>,
    // None means that the function returns ()
    // To return a unitless number, use `-> '_` since '_ represents unitless
    //FIXME: In the future this Option type will be replaced with a real way to represent
    // types in the AST
    pub ret: Option<ast::UnitExpr<'i>>,
    pub body: ast::Block<'i>,
    pub span: ast::Span<'i>,
}

/// Represents a conversion ratio between two units
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConversionRatio {
    pub left: Number,
    pub right: Number,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Number {
    pub value: BigDecimal,
    pub unit: CanonicalUnit,
}

impl DisplayString for Number {
    fn display<'a>(&self, units: &UnitGraph) -> String {
        let mut out = format!("{}", self.value);
        if !self.unit.is_unitless() {
            out += " ";
            out += &self.unit.display(units);
        }
        out
    }
}

impl Number {
    /// Tests whether abs(self - other) <= threshold
    ///
    /// Does not perform any conversions, so the units must be the same. Will panic if this
    /// condition is not met.
    pub fn approx_eq(&self, other: &Self, threshold: &BigDecimal) -> bool {
        assert_eq!(self.unit, other.unit, "Cannot compare two quantities with different units");

        (&self.value - &other.value).abs() <= *threshold
    }
}
