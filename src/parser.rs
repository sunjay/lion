pub use nom::Err as Error;
use nom::InputLength;
use nom_locate::LocatedSpan;

use ast::*;
use unit_graph::UnitGraph;

pub type Span<'a> = LocatedSpan<&'a str>;

pub type ParseResult<'a, T> = Result<T, Error<Span<'a>>>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    let input = Span::new(input);
    let mut units = UnitGraph::new();
    match program(input, &mut units) {
        Ok((remaining, decls)) => {
            assert!(remaining.fragment.is_empty(), "bug: parser did not completely read input");
            Ok(Program {decls, units})
        },
        Err(err) => Err(err),
    }
}

named_args!(program<'a>(units: &mut UnitGraph)<Span<'a>, Vec<Decl<'a>>>,
    complete!(many0!(apply!(decl, units)))
);

named_args!(decl<'a>(units: &mut UnitGraph)<Span<'a>, Decl<'a>>, alt_complete!(
    apply!(macro_invoke, units) => { |mi| Decl::MacroInvoke(mi) } |
    apply!(function, units) => { |func| Decl::Function(func) }
));

named_args!(macro_invoke<'a>(units: &mut UnitGraph)<Span<'a>, MacroInvoke<'a>>, do_parse!(
    span: position!() >>
    name: ident_path >>
    (MacroInvoke {
        name,
        tokens: Vec::new(), //TODO
        span,
    })
));

named_args!(function<'a>(units: &mut UnitGraph)<Span<'a>, Function<'a>>, do_parse!(
    attrs: many0!(attribute) >>
    span: position!() >>
    t_fn >>
    name: opt!(ident) >>
    args: apply!(fnargs, units) >>
    ret: apply!(return_unit, units) >>
    body: apply!(block, units) >>
    (Function {attrs, name: name.unwrap_or_default(), args, ret, body, span})
));

named_args!(fnargs<'a>(units: &mut UnitGraph)<Span<'a>, FnArgs<'a>>, do_parse!(
    (unimplemented!())
));

named_args!(return_unit<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, do_parse!(
    (unimplemented!())
));

named!(attribute(Span) -> Attribute, do_parse!(
    (unimplemented!())
));

named_args!(block<'a>(units: &mut UnitGraph)<Span<'a>, Block<'a>>, do_parse!(
    (unimplemented!())
));

named!(ident_path(Span) -> IdentPath, do_parse!(
    (unimplemented!())
));

named!(ident(Span) -> Ident, do_parse!(
    (unimplemented!())
));

named!(t_left_brace(Span) -> Span, tag!("{"));
named!(t_right_brace(Span) -> Span, tag!("}"));
named!(t_left_paren(Span) -> Span, tag!("("));
named!(t_right_paren(Span) -> Span, tag!(")"));
named!(t_left_bracket(Span) -> Span, tag!("["));
named!(t_right_bracket(Span) -> Span, tag!("]"));
named!(t_plus(Span) -> Span, tag!("+"));
named!(t_minus(Span) -> Span, tag!("-"));
named!(t_star(Span) -> Span, tag!("*"));
named!(t_slash(Span) -> Span, tag!("/"));
named!(t_percent(Span) -> Span, tag!("%"));
named!(t_caret(Span) -> Span, tag!("^"));
named!(t_comma(Span) -> Span, tag!(","));
named!(t_hash(Span) -> Span, tag!("#"));
named!(t_semi(Span) -> Span, tag!(";"));
named!(t_fn(Span) -> Span, tag!("fn"));
named!(t_return(Span) -> Span, tag!("return"));
named!(t_unit(Span) -> Span, tag!("()"));
named!(t_as(Span) -> Span, tag!("as"));
named!(t_arrow(Span) -> Span, tag!("->"));
named!(t_let(Span) -> Span, tag!("let"));
named!(t_becomes(Span) -> Span, tag!("="));
