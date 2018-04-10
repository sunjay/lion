use parser::Span;
use unit_graph::{UnitGraph, Unit};

pub struct Program<'i> {
    pub decls: Vec<Decl<'i>>,
    //TODO: Replace with a real symbol table (e.g. symbol-map crate)
    pub units: UnitGraph,
}

pub enum Decl<'i> {
    MacroInvoke(MacroInvoke<'i>),
    Function(Function<'i>),
}

pub struct MacroInvoke<'i> {
    pub name: IdentPath<'i>,
    pub tokens: Vec<Token>,
    pub span: Span<'i>,
}

pub struct Function<'i> {
    pub attrs: Vec<Attribute<'i>>,
    pub name: &'i str,
    pub args: FnArgs<'i>,
    // None means that the function returns ()
    // To return unitless, use `-> '_` since '_ represents unitless
    //FIXME: In the future this will be replaced with a real way to represent
    // types in the AST
    pub ret: Option<UnitExpr<'i>>,
    pub body: Block<'i>,
    pub span: Span<'i>,
}

pub struct Attribute<'i> {
    pub name: Ident<'i>,
    pub tokens: Vec<Token>,
    pub span: Span<'i>,
}

pub type FnArgs<'i> = Vec<IdentUnit<'i>>;

pub struct Block<'i> {
    pub body: Vec<Statement<'i>>,
    pub ret: Expr<'i>,
    pub span: Span<'i>,
}

pub enum Statement<'i> {
    Function(Function<'i>),
    Let(IdentUnit<'i>, Expr<'i>),
    Expr(Expr<'i>),
}

pub enum Expr<'i> {
    Add(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Sub(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Mul(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Div(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Mod(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Pow(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Call(IdentPath<'i>, Vec<Expr<'i>>, Span<'i>),
    Number(NumericLiteral<'i>, UnitExpr<'i>),
    // Used for both "as" and for "() 'newunit" syntax
    ConvertTo(Box<Expr<'i>>, UnitExpr<'i>, Span<'i>),
    Ident(IdentPath<'i>, Span<'i>),
    Return(Box<Expr<'i>>, Span<'i>),
    Block(Box<Block<'i>>),
    UnitValue, // ()
}

pub struct IdentUnit<'i> {
    pub name: Ident<'i>,
    pub unit: UnitExpr<'i>,
}

pub type IdentPath<'i> = Vec<Ident<'i>>;
pub type Ident<'i> = &'i str;

pub enum NumericLiteral<'i> {
    Float(f64, Span<'i>),
    Int(i64, Span<'i>),
}

pub enum UnitExpr<'i> {
    Unit(Unit, Span<'i>),
    Mul(Box<UnitExpr<'i>>, Box<UnitExpr<'i>>, Span<'i>),
    Div(Box<UnitExpr<'i>>, Box<UnitExpr<'i>>, Span<'i>),
    Pow(Box<UnitExpr<'i>>, i64, Span<'i>),
}

pub type Token = (); //TODO: tt
