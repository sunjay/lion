pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub enum Statement {
    NamedFunction {
        name: String,
        definition: Function,
    },
    AnonymousFunction(Function),
    Assignment {
        name: String,
        value: Expr,
    },
    Expression(Expr),
}

pub type Expr = Vec<ExprItem>;

#[derive(PartialEq, Debug)]
pub enum ExprItem {
    AnonymousFunction(Function),
    SingleTerm(Term),
    Group(Expr),
}

#[derive(PartialEq, Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Expr,
}

#[derive(PartialEq, Debug)]
pub enum Term {
    Symbol(String),
    Number(f64),
    StringLiteral(String),
}

