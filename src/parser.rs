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

named_args!(decl<'a>(units: &mut UnitGraph)<Span<'a>, Decl<'a>>, alt!(
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

named_args!(statement<'a>(units: &mut UnitGraph)<Span<'a>, Statement<'a>>, ws_comments!(alt!(
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

named_args!(factor<'a>(units: &mut UnitGraph)<Span<'a>, Expr<'a>>, ws_comments!(alt!(
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

named_args!(unitfactor<'a>(units: &mut UnitGraph)<Span<'a>, UnitExpr<'a>>, ws_comments!(alt!(
    apply!(unit, units) => { |(u, span)| UnitExpr::Unit(u, span) } |
    delimited!(t_left_paren, apply!(compound_unit, units), t_right_paren)
)));

named_args!(unit<'a>(units: &mut UnitGraph)<Span<'a>, (Unit, Span<'a>)>, do_parse!(
    span: position!() >>
    char!('\'') >>
    name: alt!(
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

named!(ident_path(Span) -> IdentPath, do_parse!(
    path: separated_nonempty_list_complete!(tag!("::"), ident) >>
    // Cannot be followed by another path separator
    not!(tag!("::")) >>
    (path)
));

named!(ident(Span) -> Ident,
    map!(
        recognize!(tuple!(not!(eof!()), alt!(alpha | tag!("_")), many0!(alt!(alpha | digit | tag!("_"))))),
        |id| id.fragment.0
    )
);

named!(numeric_literal(Span) -> NumericLiteral, alt!(
    tuple!(position!(), integer_literal) => { |(span, i)| NumericLiteral::Int(i, span) } |
    tuple!(position!(), float_literal) => { |(span, fl)| NumericLiteral::Float(fl, span) }
));

named!(integer_literal(Span) -> i64, do_parse!(
    literal: flat_map!(recognize!(
        tuple!(
            opt!(alt!(t_plus | t_minus)),
            many1!(digit)
        )
    ), parse_to!(i64)) >>
    // Cannot be followed by any part of a float
    alt!(not!(not!(eof!())) | not!(alt!(char!('.') | char!('e') | char!('E')))) >>
    (literal)
));

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
            match $parser(input) {
                Ok((remaining, _)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`", $input);
                },
                Err(err) => panic!("parse of `{}` failed. Error: {:?}", $input, err),
            }
        };
        ($parser:ident ( $input:expr ) -> err) => {
            let input = Span::new(CompleteStr($input));
            match $parser(input) {
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
            match $parser(input) {
                Ok((remaining, output)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`", $input);
                    assert_eq!(output, $expected);
                },
                Err(err) => panic!("parse of `{}` failed. Error: {:?}", $input, err),
            }
        };
    }

    macro_rules! test_parser2 {
        ($parser:ident ( $input:expr ) -> ok) => {
            let input = Span::new(CompleteStr($input));
            let mut units = UnitGraph::new();
            match $parser(input, &mut units) {
                Ok((remaining, _)) => {
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
    fn unitfactor_parser() {
        test_parser2!(unitfactor("") -> err);
        test_parser2!(unitfactor("'a") -> ok);
        test_parser2!(unitfactor("'km") -> ok);
        test_parser2!(unitfactor("'_") -> ok);
        test_parser2!(unitfactor("'_a") -> err);
        test_parser2!(unitfactor("'a_b") -> ok);
        test_parser2!(unitfactor("'kph") -> ok);

        test_parser2!(unitfactor("('a)") -> ok);
        test_parser2!(unitfactor("('km)") -> ok);
        test_parser2!(unitfactor("('_)") -> ok);
        test_parser2!(unitfactor("('a_b)") -> ok);
        test_parser2!(unitfactor("('kph)") -> ok);

        test_parser2!(unitfactor("( 'a)") -> ok);
        test_parser2!(unitfactor("( 'km)") -> ok);
        test_parser2!(unitfactor("( '_)") -> ok);
        test_parser2!(unitfactor("( 'a_b)") -> ok);
        test_parser2!(unitfactor("( 'kph)") -> ok);

        test_parser2!(unitfactor("('a )") -> ok);
        test_parser2!(unitfactor("('km )") -> ok);
        test_parser2!(unitfactor("('_ )") -> ok);
        test_parser2!(unitfactor("('a_b )") -> ok);
        test_parser2!(unitfactor("('kph )") -> ok);

        test_parser2!(unitfactor("(      'a )") -> ok);
        test_parser2!(unitfactor("(      'km )") -> ok);
        test_parser2!(unitfactor("(      '_    )") -> ok);
        test_parser2!(unitfactor("(      'a_b )") -> ok);
        test_parser2!(unitfactor("(      'kph )") -> ok);
    }

    #[test]
    fn unit_parser() {
        test_parser2!(unit("") -> err);
        test_parser2!(unit("'a") -> ok);
        test_parser2!(unit("'km") -> ok);
        test_parser2!(unit("'_") -> ok);
        test_parser2!(unit("'_a") -> err);
        test_parser2!(unit("'a_b") -> ok);
        test_parser2!(unit("'kph") -> ok);
    }

    #[test]
    fn ident_path_parser() {
        test_parser!(ident_path("") -> err);
        test_parser!(ident_path("abc") -> ok, vec!["abc"]);
        test_parser!(ident_path("abc::bcd") -> ok, vec!["abc", "bcd"]);
        test_parser!(ident_path("abc:: bcd") -> err);
        test_parser!(ident_path("abc::") -> err);
    }

    #[test]
    fn ident_parser() {
        test_parser!(ident("") -> err);
        test_parser!(ident("a") -> ok, "a");
        test_parser!(ident("km") -> ok, "km");
        test_parser!(ident("foooo") -> ok, "foooo");
        test_parser!(ident("_") -> ok, "_");
        test_parser!(ident("_a") -> ok, "_a");
        test_parser!(ident("_ab") -> ok, "_ab");
        test_parser!(ident("a_b") -> ok, "a_b");
        test_parser!(ident("tree_height") -> ok, "tree_height");
        test_parser!(ident("kph") -> ok, "kph");
        test_parser!(ident("0") -> err);
        test_parser!(ident("0") -> err);
        test_parser!(ident("1") -> err);
        test_parser!(ident("123") -> err);
        test_parser!(ident("-123") -> err);
        test_parser!(ident(".123") -> err);
        test_parser!(ident("-.123") -> err);
        test_parser!(ident("123.") -> err);
        test_parser!(ident("-123.") -> err);
        test_parser!(ident("123.e1") -> err);
        test_parser!(ident("123.e-1") -> err);
        test_parser!(ident("123.456e10") -> err);
        test_parser!(ident("123.456e-10") -> err);
        test_parser!(ident("123.456E10") -> err);
        test_parser!(ident("123.456E-10") -> err);
        test_parser!(ident("0.456E-10") -> err);
        test_parser!(ident("123.0E-10") -> err);
        test_parser!(ident("0.456E10") -> err);
        test_parser!(ident("123.0E10") -> err);
        test_parser!(ident("+123.456e10") -> err);
        test_parser!(ident("+123.456e-10") -> err);
        test_parser!(ident("+123.456E10") -> err);
        test_parser!(ident("+123.456E-10") -> err);
        test_parser!(ident("-123.456e10") -> err);
        test_parser!(ident("-123.456e-10") -> err);
        test_parser!(ident("-123.456E10") -> err);
        test_parser!(ident("-123.456E-10") -> err);
    }

    #[test]
    fn numeric_literal_parser() {
        let span = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        test_parser!(numeric_literal("") -> err);
        test_parser!(numeric_literal("a") -> err);
        test_parser!(numeric_literal("'km") -> err);
        test_parser!(numeric_literal("0") -> ok, NumericLiteral::Int(0, span));
        test_parser!(numeric_literal("0") -> ok, NumericLiteral::Int(0, span));
        test_parser!(numeric_literal("1") -> ok, NumericLiteral::Int(1, span));
        test_parser!(numeric_literal("123") -> ok, NumericLiteral::Int(123, span));
        test_parser!(numeric_literal("-123") -> ok, NumericLiteral::Int(-123, span));
        test_parser!(numeric_literal(".123") -> ok, NumericLiteral::Float(0.123, span));
        test_parser!(numeric_literal("-.123") -> ok, NumericLiteral::Float(-0.123, span));
        test_parser!(numeric_literal("123.") -> ok, NumericLiteral::Float(123., span));
        test_parser!(numeric_literal("-123.") -> ok, NumericLiteral::Float(-123., span));
        test_parser!(numeric_literal("123.e1") -> ok, NumericLiteral::Float(123.0e1, span));
        test_parser!(numeric_literal("123.e-1") -> ok, NumericLiteral::Float(123.0e-1, span));
        test_parser!(numeric_literal("123.456e10") -> ok, NumericLiteral::Float(123.456e10, span));
        test_parser!(numeric_literal("123.456e-10") -> ok, NumericLiteral::Float(123.456e-10, span));
        test_parser!(numeric_literal("123.456E10") -> ok, NumericLiteral::Float(123.456E10, span));
        test_parser!(numeric_literal("123.456E-10") -> ok, NumericLiteral::Float(123.456E-10, span));
        test_parser!(numeric_literal("0.456E-10") -> ok, NumericLiteral::Float(0.456E-10, span));
        test_parser!(numeric_literal("123.0E-10") -> ok, NumericLiteral::Float(123.0E-10, span));
        test_parser!(numeric_literal("0.456E10") -> ok, NumericLiteral::Float(0.456E10, span));
        test_parser!(numeric_literal("123.0E10") -> ok, NumericLiteral::Float(123.0E10, span));
        test_parser!(numeric_literal("+123.456e10") -> ok, NumericLiteral::Float(123.456e10, span));
        test_parser!(numeric_literal("+123.456e-10") -> ok, NumericLiteral::Float(123.456e-10, span));
        test_parser!(numeric_literal("+123.456E10") -> ok, NumericLiteral::Float(123.456E10, span));
        test_parser!(numeric_literal("+123.456E-10") -> ok, NumericLiteral::Float(123.456E-10, span));
        test_parser!(numeric_literal("-123.456e10") -> ok, NumericLiteral::Float(-123.456e10, span));
        test_parser!(numeric_literal("-123.456e-10") -> ok, NumericLiteral::Float(-123.456e-10, span));
        test_parser!(numeric_literal("-123.456E10") -> ok, NumericLiteral::Float(-123.456E10, span));
        test_parser!(numeric_literal("-123.456E-10") -> ok, NumericLiteral::Float(-123.456E-10, span));
    }

    #[test]
    fn integer_literal_parser() {
        test_parser!(integer_literal("") -> err);
        test_parser!(integer_literal("a") -> err);
        test_parser!(integer_literal("'km") -> err);
        test_parser!(integer_literal("0") -> ok, 0);
        test_parser!(integer_literal("1") -> ok, 1);
        test_parser!(integer_literal("123") -> ok, 123);
        test_parser!(integer_literal("-123") -> ok, -123);
        test_parser!(integer_literal(".123") -> err);
        test_parser!(integer_literal("-.123") -> err);
        test_parser!(integer_literal("123.") -> err);
        test_parser!(integer_literal("-123.") -> err);
        test_parser!(integer_literal("123.e1") -> err);
        test_parser!(integer_literal("123.e-1") -> err);
        test_parser!(integer_literal("123.456e10") -> err);
        test_parser!(integer_literal("123.456e-10") -> err);
        test_parser!(integer_literal("123.456E10") -> err);
        test_parser!(integer_literal("123.456E-10") -> err);
        test_parser!(integer_literal("0.456E-10") -> err);
        test_parser!(integer_literal("123.0E-10") -> err);
        test_parser!(integer_literal("0.456E10") -> err);
        test_parser!(integer_literal("123.0E10") -> err);
        test_parser!(integer_literal("+123.456e10") -> err);
        test_parser!(integer_literal("+123.456e-10") -> err);
        test_parser!(integer_literal("+123.456E10") -> err);
        test_parser!(integer_literal("+123.456E-10") -> err);
        test_parser!(integer_literal("-123.456e10") -> err);
        test_parser!(integer_literal("-123.456e-10") -> err);
        test_parser!(integer_literal("-123.456E10") -> err);
        test_parser!(integer_literal("-123.456E-10") -> err);
    }

    #[test]
    fn float_literal_parser() {
        test_parser!(float_literal("") -> err);
        test_parser!(float_literal("a") -> err);
        test_parser!(float_literal("'km") -> err);
        test_parser!(float_literal("0") -> ok, 0.0);
        test_parser!(float_literal("1") -> ok, 1.0);
        test_parser!(float_literal("123") -> ok, 123.0);
        test_parser!(float_literal("-123") -> ok, -123.0);
        test_parser!(float_literal(".123") -> ok, 0.123);
        test_parser!(float_literal("-.123") -> ok, -0.123);
        test_parser!(float_literal("123.") -> ok, 123.);
        test_parser!(float_literal("-123.") -> ok, -123.);
        test_parser!(float_literal("123.e1") -> ok, 123.0e1);
        test_parser!(float_literal("123.e-1") -> ok, 123.0e-1);
        test_parser!(float_literal("123.456e10") -> ok, 123.456e10);
        test_parser!(float_literal("123.456e-10") -> ok, 123.456e-10);
        test_parser!(float_literal("123.456E10") -> ok, 123.456E10);
        test_parser!(float_literal("123.456E-10") -> ok, 123.456E-10);
        test_parser!(float_literal("0.456E-10") -> ok, 0.456E-10);
        test_parser!(float_literal("123.0E-10") -> ok, 123.0E-10);
        test_parser!(float_literal("0.456E10") -> ok, 0.456E10);
        test_parser!(float_literal("123.0E10") -> ok, 123.0E10);
        test_parser!(float_literal("+123.456e10") -> ok, 123.456e10);
        test_parser!(float_literal("+123.456e-10") -> ok, 123.456e-10);
        test_parser!(float_literal("+123.456E10") -> ok, 123.456E10);
        test_parser!(float_literal("+123.456E-10") -> ok, 123.456E-10);
        test_parser!(float_literal("-123.456e10") -> ok, -123.456e10);
        test_parser!(float_literal("-123.456e-10") -> ok, -123.456e-10);
        test_parser!(float_literal("-123.456E10") -> ok, -123.456E10);
        test_parser!(float_literal("-123.456E-10") -> ok, -123.456E-10);
    }
}
