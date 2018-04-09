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
    pub name: IdentPath,
    pub tokens: Vec<Token>,
    pub span: Span<'i>,
}

pub struct Function<'i> {
    pub attrs: Vec<Attribute<'i>>,
    pub name: &'i str,
    pub args: FnArgs<'i>,
    pub ret: UnitExpr<'i>,
    pub body: Block<'i>,
    pub span: Span<'i>,
}

pub struct Attribute<'i> {
    pub name: String,
    pub tokens: Vec<Token>,
    pub span: Span<'i>,
}

pub type FnArgs<'i> = Vec<(String, UnitExpr<'i>)>;

pub struct Block<'i> {
    pub body: Vec<Statement<'i>>,
    pub ret: Expr<'i>,
    pub span: Span<'i>,
}

pub enum Statement<'i> {
    Function(Function<'i>, Span<'i>),
    Expr(Expr<'i>, Span<'i>),
}

pub enum Expr<'i> {
    Add(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Sub(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Mul(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Div(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Rem(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Pow(Box<Expr<'i>>, Box<Expr<'i>>, Span<'i>),
    Call(IdentPath, Vec<Expr<'i>>, Span<'i>),
    Number(NumericLiteral<'i>, UnitExpr<'i>, Span<'i>),
    ConvertTo(Box<Expr<'i>>, UnitExpr<'i>, Span<'i>),
    Ident(IdentPath, Span<'i>),
    Return(Box<Expr<'i>>, Span<'i>),
}
pub type IdentPath = Vec<String>;

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
