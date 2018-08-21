use std::fmt;

use rust_decimal::Decimal;

pub use parser::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'i> {
    pub decls: Vec<Decl<'i>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl<'i> {
    MacroInvoke(MacroInvoke<'i>),
    Function(Function<'i>),
    Constant(Constant<'i>),
    UnitDecl(UnitDecl<'i>),
    ConversionDecl(ConversionDecl<'i>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroInvoke<'i> {
    pub name: IdentPath<'i>,
    pub tokens: Vec<Token<'i>>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'i> {
    pub attrs: Vec<Attribute<'i>>,
    pub name: Ident<'i>,
    pub args: FnArgs<'i>,
    // None means that the function returns ()
    // To return a unitless number, use `-> '_` since '_ represents unitless
    //FIXME: In the future this Option type will be replaced with a real way to represent
    // types in the AST
    pub ret: Option<UnitExpr<'i>>,
    pub body: Block<'i>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant<'i> {
    pub name: Ident<'i>,
    pub unit: UnitExpr<'i>,
    pub value: Expr<'i>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnitDecl<'i> {
    pub attrs: Vec<Attribute<'i>>,
    pub unit_name: UnitName<'i>,
    pub alias_for: Option<UnitExpr<'i>>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConversionDecl<'i> {
    pub left: Expr<'i>,
    pub right: Expr<'i>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute<'i> {
    pub name: Ident<'i>,
    pub tokens: Vec<Token<'i>>,
    pub span: Span<'i>,
}

pub type FnArgs<'i> = Vec<IdentUnit<'i>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'i> {
    pub body: Vec<Statement<'i>>,
    pub ret: Expr<'i>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'i> {
    Function(Function<'i>),
    Let(LetDecl<'i>),
    Expr(Expr<'i>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl<'i> {
    pub label: Ident<'i>,
    // If this is None, the unit will be inferred
    pub expected_unit: Option<UnitExpr<'i>>,
    pub value: Expr<'i>,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'i> {
    Add(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Sub(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Mul(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Div(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Mod(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Pow(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Call(IdentPath<'i>, Vec<Expr<'i>>, Span<'i>),
    MacroCall(MacroInvoke<'i>),
    Number(NumericLiteral<'i>, UnitExpr<'i>),
    // Used for both "as" and for "() 'newunit" syntax
    ConvertTo(Box<Expr<'i>>, UnitExpr<'i>, Span<'i>),
    Ident(IdentPath<'i>, Span<'i>),
    Return(Box<Expr<'i>>, Span<'i>),
    Block(Box<Block<'i>>),
    UnitValue(Span<'i>), // ()
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentUnit<'i> {
    pub name: Ident<'i>,
    pub unit: UnitExpr<'i>,
}

pub type IdentPath<'i> = Vec<Ident<'i>>;
pub type Ident<'i> = &'i str;

#[derive(Debug, Clone, PartialEq)]
pub struct NumericLiteral<'i> {
    pub value: Decimal,
    pub span: Span<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnitExpr<'i> {
    Unit(UnitName<'i>, Span<'i>),
    // For both 'a * 'b and 'a 'b
    Mul(Box<UnitExpr<'i>>, Box<UnitExpr<'i>>, Span<'i>),
    Div(Box<UnitExpr<'i>>, Box<UnitExpr<'i>>, Span<'i>),
    Pow(Box<UnitExpr<'i>>, Decimal, Span<'i>),
}

// If the quantity is unitless, then we return None
// Unitless is represented in the syntax as '_
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnitName<'i>(Option<&'i str>);

impl<'i> From<&'i str> for UnitName<'i> {
    fn from(name: &'i str) -> Self {
        assert!(!name.is_empty(), "unit name cannot be empty");
        assert_ne!(name, "_", "bug: to get '_, use UnitName::unitless()");
        UnitName(Some(name))
    }
}

impl<'i> fmt::Display for UnitName<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}", match self.0 {
            None => "_",
            Some(name) => name,
        })
    }
}

impl<'i> UnitName<'i> {
    pub fn unitless() -> Self {
        UnitName(None)
    }

    pub fn is_unitless(&self) -> bool {
        self.0.is_none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'i> {
    Parens(Vec<Token<'i>>),
    Brackets(Vec<Token<'i>>),
    Braces(Vec<Token<'i>>),
    MacroInvoke(MacroInvoke<'i>),
    Function(Function<'i>),
    Attribute(Attribute<'i>),
    Block(Block<'i>),
    Expr(Expr<'i>),
    UnitExpr(UnitExpr<'i>),
    IdentPath(IdentPath<'i>),
    NumericLiteral(NumericLiteral<'i>),
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    LessThan,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Comma,
    Hash,
    Semi,
    Return,
    Unit,
    For,
    As,
    Arrow,
    Alias,
    Let,
    Const,
    Becomes,
    UnitKeyword,
    Conversion,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "unit name cannot be empty")]
    fn unit_name_empty() {
        UnitName::from("");
    }

    #[test]
    #[should_panic]
    fn unit_name_unitless() {
        UnitName::from("_");
    }

    #[test]
    fn format_unit_name() {
        assert_eq!(UnitName::unitless().to_string(), "'_");
        assert_eq!(UnitName::from("km").to_string(), "'km");
    }
}
