use pest::{iterators::Pairs, Error, Parser as ParserTrait};
pub use pest::Span;

use ast::*;
use unit_graph::UnitGraph;

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Parser;

pub type ParseResult<'s, T> = Result<T, Error<'s, Rule>>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    let pairs = Parser::parse(Rule::program, input)?;

    let mut units = UnitGraph::new();

    let decls = pairs.map(|pair| match pair.as_rule() {
        Rule::macro_invoke => unimplemented!(),
        Rule::function => Decl::Function(consume_function(pair.into_inner(), &mut units)),
        _ => unreachable!(),
    }).collect();

    Ok(Program { decls, units })
}

fn consume_function<'i>(pairs: Pairs<'i, Rule>, units: &mut UnitGraph) -> Function<'i> {
    let mut attrs = Vec::new();
    let mut name = None;
    let mut span = None;
    let mut args = None;
    let mut ret = None;
    let mut body = None;

    for pair in pairs {
        match pair.as_rule() {
            // This code is written such that if something expected isn't provided by the parser,
            // a panic will occur below

            Rule::attribute => attrs.push(consume_attribute(pair.into_inner())),
            Rule::ident if name.is_none() && span.is_none() => {
                name = Some(pair.as_str());
                span = Some(pair.into_span());
            },
            Rule::fnargs if args.is_none() => args = Some(consume_fnargs(pair.into_inner(), units)),
            Rule::compound_unit if ret.is_none() => ret = Some(consume_unit_expr(pair.into_inner(), units)),
            Rule::block if body.is_none() => body = Some(consume_block(pair.into_inner(), units)),
            _ => unreachable!("grammar did not produce expected tokens"),
        }
    }

    let name = name.expect("grammar did not provide a name");
    let span = span.expect("grammar did not provide a span");
    let args = args.expect("grammar did not provide fnargs");
    let ret = ret.unwrap_or_else(|| UnitExpr::Unit(units.unitless(), span.clone()));
    let body = body.expect("grammar did not provide a block");
    Function {attrs, name, args, ret, body, span}
}

fn consume_attribute<'i>(pairs: Pairs<'i, Rule>) -> Attribute<'i> {
    unimplemented!();
}

fn consume_fnargs<'i>(pairs: Pairs<'i, Rule>, units: &mut UnitGraph) -> FnArgs<'i> {
    unimplemented!();
}

fn consume_block<'i>(pairs: Pairs<'i, Rule>, units: &mut UnitGraph) -> Block<'i> {
    unimplemented!();
}

fn consume_unit_expr<'i>(pairs: Pairs<'i, Rule>, units: &mut UnitGraph) -> UnitExpr<'i> {
    unimplemented!();
}
