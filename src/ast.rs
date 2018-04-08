use std::collections::HashMap;

pub struct Program {
    pub decls: Vec<Decl>,
    //TODO: Replace with a real symbol table (e.g. symbol-map crate)
    pub units: HashMap<String, Unit>,
    pub conversion_graph: (), //TODO
}

pub enum Decl {
    MacroInvoke(MacroInvoke),
    Function(Function),
}

pub struct MacroInvoke {
    pub name: IdentPath,
    pub tokens: Vec<Token>,
}

pub struct Function {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub args: FnArgs,
    pub body: Block,
}
pub struct Attribute {
    pub name: String,
    pub tokens: Vec<Token>,
}
pub type FnArgs = Vec<(String, UnitExpr)>;

pub struct Block {
    pub body: Vec<Statement>,
    pub ret: Expr,
}

pub enum Statement {
    Function(Function),
    Expr(Expr),
}

pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Call(IdentPath, Vec<Expr>),
    Number(NumericLiteral, UnitExpr),
    ConvertTo(Box<Expr>, UnitExpr),
    Ident(IdentPath),
    Return(Box<Expr>),
}
pub type IdentPath = Vec<String>;

pub enum NumericLiteral {
    Float(f64),
    Int(i64),
}

pub enum UnitExpr {
    Unit(Unit),
    Mul(Box<UnitExpr>, Box<UnitExpr>),
    Div(Box<UnitExpr>, Box<UnitExpr>),
    Pow(Box<UnitExpr>, i64),
}
pub type Unit = u64;

pub type Token = (); //TODO: tt
