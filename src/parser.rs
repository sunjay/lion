pub use nom::Err as Error;
use nom::{alpha, digit, recognize_float, sp, types::CompleteStr};
use nom_locate::LocatedSpan;

use ast::*;
use unit_graph::{UnitGraph, Unit};

pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

pub type ParseResult<'a, T> = Result<T, Error<Span<'a>>>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    let input = Span::new(CompleteStr(input));
    let mut units = UnitGraph::new();
    match program(input, &mut units) {
        Ok((remaining, decls)) => {
            assert!(remaining.fragment.0.is_empty(), "bug: parser did not completely read input");
            Ok(Program {decls, units})
        },
        Err(err) => Err(err),
    }
}

pub fn parse_expr<'a>(input: &'a str, units: &'a mut UnitGraph) -> ParseResult<'a, Expr<'a>> {
    let input = Span::new(CompleteStr(input));
    match expr(input, units) {
        Ok((remaining, expr)) => {
            assert!(remaining.fragment.0.is_empty(), "bug: parser did not completely read input");
            Ok(expr)
        },
        Err(err) => Err(err),
    }
}

macro_rules! default_unitless {
    ($id:ident, $units:ident, $span:ident) => {
        $id.unwrap_or_else(|| UnitExpr::Unit($units.unitless(), $span))
    };
}

macro_rules! ws_comments {
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, whitespace_comment, $($args)*)
    }
  )
}

named_args!(program<'a>(units: &mut UnitGraph)<Span<'a>, Vec<Decl<'a>>>,
    exact!(complete!(ws_comments!(many0!(apply!(decl, units)))))
);

named_args!(decl<'a>(units: &mut UnitGraph)<Span<'a>, Decl<'a>>, alt_complete!(
    apply!(function, units) => { |func| Decl::Function(func) } |
    macro_invoke => { |mi| Decl::MacroInvoke(mi) }
));

named!(macro_invoke(Span) -> MacroInvoke, ws_comments!(do_parse!(
    span: position!() >>
    name: ident_path >>
    t_semi >>
    (MacroInvoke {
        name,
        tokens: Vec::new(), //TODO
        span,
    })
)));

named_args!(function<'a>(units: &mut UnitGraph)<Span<'a>, Function<'a>>, ws_comments!(do_parse!(
    attrs: many0!(attribute) >>
    span: position!() >>
    t_fn >>
    name: opt!(ident) >>
    args: apply!(fnargs, units) >>
    ret: opt!(apply!(return_unit, units)) >>
    body: apply!(block, units) >>
    (Function {
        attrs,
        name: name.unwrap_or_default(),
        args,
        ret,
        body,
        span,
    })
)));

named_args!(fnargs<'a>(units: &mut UnitGraph)<Span<'a>, FnArgs<'a>>,
    ws_comments!(delimited!(
        t_left_paren,
        separated_list_complete!(t_comma, apply!(fnarg, units)),
        t_right_paren
    ))
);

named_args!(fnarg<'a>(units: &mut UnitGraph)<Span<'a>, IdentUnit<'a>>, ws_comments!(do_parse!(
    name: ident >>
    span: position!() >>
    unit: opt!(apply!(compound_unit, units)) >>
    (IdentUnit {name, unit: default_unitless!(unit, units, span)})
)));

named_args!(return_unit<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(do_parse!(
    t_arrow >>
    unit: apply!(compound_unit, units) >>
    (unit)
)));

named!(attribute(Span) -> Attribute, ws_comments!(do_parse!(
    t_hash >>
    span: position!() >>
    //TODO: tt*
    name: delimited!(t_left_bracket, ident, t_right_bracket) >>
    (Attribute {name, tokens: Vec::new(), span})
)));

named_args!(block<'a>(units: &mut UnitGraph)<Span<'a>, Block<'a>>, ws_comments!(do_parse!(
    t_left_brace >>
    span: position!() >>
    body: many0!(apply!(statement, units)) >>
    ret: opt!(apply!(expr, units)) >>
    t_right_brace >>
    (Block {body, ret: ret.unwrap_or(Expr::UnitValue), span})
)));

named_args!(statement<'a>(units: &mut UnitGraph)<Span<'a>, Statement<'a>>, ws_comments!(alt_complete!(
    apply!(function, units) => { |func| Statement::Function(func) } |
    apply!(var_decl, units) => { |(name, expr)| Statement::Let(name, expr) } |
    do_parse!(e: apply!(expr, units) >> t_semi >> (e)) => { |expr| Statement::Expr(expr) }
)));

named_args!(var_decl<'a>(units: &mut UnitGraph)<Span<'a>, (IdentUnit<'a>, Expr<'a>)>, ws_comments!(do_parse!(
    t_let >>
    name: ident >>
    unit_span: position!() >>
    unit: opt!(apply!(compound_unit, units)) >>
    t_becomes >>
    rhs: apply!(expr, units) >>
    t_semi >>
    ((IdentUnit {name, unit: default_unitless!(unit, units, unit_span)}, rhs))
)));

named_args!(expr<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, complete!(ws_comments!(do_parse!(
    first: apply!(term, units) >>
    result: fold_many0!(
        tuple!(position!(), alt!(t_plus | t_minus), apply!(term, units)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "+" => Expr::Add(Box::new(acc), Box::new(rhs), span),
            "-" => Expr::Sub(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
))));

named_args!(term<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, ws_comments!(do_parse!(
    first: apply!(pow, units) >>
    result: fold_many0!(
        tuple!(position!(), alt!(t_star | t_slash | t_percent), apply!(pow, units)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "*" => Expr::Mul(Box::new(acc), Box::new(rhs), span),
            "/" => Expr::Div(Box::new(acc), Box::new(rhs), span),
            "%" => Expr::Mod(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named_args!(pow<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, ws_comments!(do_parse!(
    first: apply!(factor_as, units) >>
    result: fold_many0!(
        tuple!(position!(), t_caret, apply!(factor_as, units)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "^" => Expr::Pow(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

// Not technically in the grammar, but needs to be done separately from factor in order to
// avoid left recursion problems
named_args!(factor_as<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, ws_comments!(do_parse!(
    first: apply!(factor, units) >>
    result: fold_many0!(
        tuple!(position!(), t_as, apply!(compound_unit, units)),
        first,
        |acc, (span, op, unit): (_, Span, _)| match op.fragment.0 {
            "as" => Expr::ConvertTo(Box::new(acc), unit, span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

macro_rules! maybe_convert_unit {
    ($expr:expr, $unit:expr, $unit_span:expr) => {
        match $unit {
            None => $expr,
            Some(unit) => Expr::ConvertTo(
                Box::new($expr),
                unit,
                $unit_span,
            ),
        }
    };
}

named_args!(factor<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, ws_comments!(alt_complete!(
    tuple!(numeric_literal, position!(), opt!(apply!(compound_unit, units))) => {
        |(num, unit_span, unit): (_, _, Option<_>)| Expr::Number(num, default_unitless!(unit, units, unit_span))
    } |
    tuple!(position!(), apply!(fncall, units), position!(), opt!(apply!(compound_unit, units))) => {
        |(span, (path, args), unit_span, unit)| {
            maybe_convert_unit!(Expr::Call(path, args, span), unit, unit_span)
        }
    } |
    tuple!(position!(), ident_path, position!(), opt!(apply!(compound_unit, units))) => {
        |(span, path, unit_span, unit)| {
            maybe_convert_unit!(Expr::Ident(path, span), unit, unit_span)
        }
    } |
    tuple!(delimited!(t_left_paren, apply!(expr, units), t_right_paren), position!(), opt!(apply!(compound_unit, units))) => {
        |(expr, unit_span, unit)| {
            maybe_convert_unit!(expr, unit, unit_span)
        }
    } |
    tuple!(apply!(block, units), position!(), opt!(apply!(compound_unit, units))) => {
        |(block, unit_span, unit)| {
            maybe_convert_unit!(Expr::Block(Box::new(block)), unit, unit_span)
        }
    } |
    tuple!(position!(), t_return, apply!(expr, units)) => {
        |(span, _, return_expr)| Expr::Return(Box::new(return_expr), span)
    } |
    t_unit => { |_| Expr::UnitValue }
)));

named_args!(fncall<'a>(units: &mut UnitGraph)<Span<'a>, (IdentPath<'a>, Vec<Expr<'a>>)>, ws_comments!(do_parse!(
    name: ident_path >>
    args: delimited!(
        t_left_paren,
        separated_list_complete!(t_comma, apply!(expr, units)),
        t_right_paren
    ) >>
    (name, args)
)));

named_args!(compound_unit<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(do_parse!(
    first: apply!(unitterm, units) >>
    result: fold_many0!(
        tuple!(position!(), apply!(unitterm, units)),
        first,
        |acc, (span, rhs)| UnitExpr::Mul(Box::new(acc), Box::new(rhs), span)
    ) >>
    (result)
)));

named_args!(unitterm<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(do_parse!(
    first: apply!(unitpow, units) >>
    result: fold_many0!(
        tuple!(position!(), alt!(t_star | t_slash), apply!(unitpow, units)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "*" => UnitExpr::Mul(Box::new(acc), Box::new(rhs), span),
            "/" => UnitExpr::Div(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named_args!(unitpow<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(do_parse!(
    first: apply!(unitfactor, units) >>
    result: fold_many0!(
        tuple!(position!(), t_caret, integer_literal),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "^" => UnitExpr::Pow(Box::new(acc), rhs, span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named_args!(unitfactor<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(alt_complete!(
    apply!(unit, units) => { |(u, span)| UnitExpr::Unit(u, span) } |
    delimited!(t_left_paren, apply!(compound_unit, units), t_right_paren)
)));

named_args!(unit<'a>(units: &mut UnitGraph)<Span<'a>, (Unit, Span<'a>)>, do_parse!(
    span: position!() >>
    char!('\'') >>
    name: alt_complete!(
        recognize!(tuple!(alpha, many0!(alt!(alpha | digit | tag!("_"))))) |
        // '_ on its own without anything following it
        do_parse!(
            result: tag!("_") >>
            // This crazy not(not(eof)) is brought to you by the not!() function returning ()
            // That means that alt would return incompatible types...
            alt!(not!(not!(eof!())) | not!(alt!(alpha | digit | tag!("_")))) >>
            (result)
        )
    ) >>
    (units.lookup(name.fragment.0), span)
));

named!(ident_path(Span) -> IdentPath,
    separated_nonempty_list_complete!(tag!("::"), ident)
);

named!(ident(Span) -> Ident,
    map!(
        recognize!(tuple!(alt!(alpha | tag!("_")), alt!(alpha | digit | tag!("_")))),
        |id| id.fragment.0
    )
);

named!(numeric_literal(Span) -> NumericLiteral, alt_complete!(
    tuple!(position!(), float_literal) => { |(span, fl)| NumericLiteral::Float(fl, span) } |
    tuple!(position!(), integer_literal) => { |(span, i)| NumericLiteral::Int(i, span) }
));

named!(integer_literal(Span) -> i64,
    flat_map!(recognize!(
        tuple!(
            opt!(alt!(t_plus | t_minus)),
            many1!(digit)
        )
    ), parse_to!(i64))
);

named!(float_literal(Span) -> f64,
    flat_map!(call!(recognize_float), parse_to!(f64))
);

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

named!(whitespace_comment(Span) -> Span, alt!(whitespace | comment));

named!(whitespace(Span) -> Span, call!(sp));
named!(comment(Span) -> Span, recognize!(tuple!(tag!("//"), take_until_and_consume!("\n"))));

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parser {
        ($parser:ident ( $input:expr ) -> ok) => {
            let input = Span::new(CompleteStr($input));
            let mut units = UnitGraph::new();
            match $parser(input, &mut units) {
                Ok((remaining, output)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`", $input);
                },
                Err(err) => panic!("parse of `{}` failed. Error: {:?}", $input, err),
            }
        };
        ($parser:ident ( $input:expr ) -> err) => {
            let input = Span::new(CompleteStr($input));
            let mut units = UnitGraph::new();
            match $parser(input, &mut units) {
                Ok((remaining, output)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`", $input);
                    panic!("parse of `{}` succeeded (when it should have failed). Result: {:?}", $input, output);
                },
                Err(_) => {}, // Expected
            }
        };
        ($parser:ident ( $input:expr ) -> ok, $expected:expr) => {
            let input = Span::new(CompleteStr($input));
            let mut units = UnitGraph::new();
            match $parser(input, &mut units) {
                Ok((remaining, output)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`", $input);
                    assert_eq!(output, $expected);
                },
                Err(err) => panic!("parse of `{}` failed. Error: {:?}", $input, err),
            }
        };
    }

    #[test]
    fn unit_parser() {
        test_parser!(unit("") -> err);
        test_parser!(unit("'a") -> ok);
        test_parser!(unit("'km") -> ok);
        test_parser!(unit("'_") -> ok);
        test_parser!(unit("'_a") -> err);
        test_parser!(unit("'a_b") -> ok);
        test_parser!(unit("'kph") -> ok);
    }
}
