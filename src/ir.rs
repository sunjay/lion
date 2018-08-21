//! Intermediate representation for type checked items

use rust_decimal::Decimal;
use canonical::CanonicalUnit;

use ast;

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
    pub value: Decimal,
    pub unit: CanonicalUnit,
}
