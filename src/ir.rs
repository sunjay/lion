//! Intermediate representation for type checked items

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
