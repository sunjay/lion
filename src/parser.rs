pub use nom::Err as Error;
use nom::{alpha, digit, recognize_float, sp, types::CompleteStr};
use nom_locate::LocatedSpan;

use ast::*;

pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

pub type ParseResult<'a, T> = Result<T, Error<Span<'a>>>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    let input = Span::new(CompleteStr(input));
    match program(input) {
        Ok((remaining, decls)) => {
            assert!(remaining.fragment.0.is_empty(), "bug: parser did not completely read input");
            Ok(Program {decls})
        },
        Err(err) => Err(err),
    }
}

pub fn parse_expr<'a>(input: &'a str) -> ParseResult<'a, Expr<'a>> {
    let input = Span::new(CompleteStr(input));
    match expr(input) {
        Ok((remaining, expr)) => {
            assert!(remaining.fragment.0.is_empty(), "bug: parser did not completely read input");
            Ok(expr)
        },
        Err(err) => Err(err),
    }
}

macro_rules! default_unitless {
    ($id:ident, $span:ident) => {
        $id.unwrap_or_else(|| UnitExpr::Unit(None, $span))
    };
}

macro_rules! ws_comments {
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, whitespace_comment, $($args)*)
    }
  )
}

named!(program(Span) -> Vec<Decl>,
    exact!(complete!(ws_comments!(many0!(decl))))
);

named!(decl(Span) -> Decl, alt!(
    function => { |func| Decl::Function(func) } |
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

named!(function(Span) -> Function, ws_comments!(do_parse!(
    attrs: many0!(attribute) >>
    span: position!() >>
    t_fn >>
    name: opt!(ident) >>
    args: fnargs >>
    ret: opt!(return_unit) >>
    body: block >>
    (Function {
        attrs,
        name: name.unwrap_or_default(),
        args,
        ret,
        body,
        span,
    })
)));

named!(fnargs(Span) -> FnArgs,
    ws_comments!(delimited!(
        t_left_paren,
        separated_list_complete!(t_comma, fnarg),
        t_right_paren
    ))
);

named!(fnarg(Span) -> IdentUnit, ws_comments!(do_parse!(
    name: ident >>
    span: position!() >>
    unit: opt!(compound_unit) >>
    (IdentUnit {name, unit: default_unitless!(unit, span)})
)));

named!(return_unit(Span) -> UnitExpr, ws_comments!(do_parse!(
    t_arrow >>
    unit: compound_unit >>
    (unit)
)));

named!(attribute(Span) -> Attribute, ws_comments!(do_parse!(
    t_hash >>
    span: position!() >>
    //TODO: tt*
    name: delimited!(t_left_bracket, ident, t_right_bracket) >>
    (Attribute {name, tokens: Vec::new(), span})
)));

named!(block(Span) -> Block, ws_comments!(do_parse!(
    t_left_brace >>
    span: position!() >>
    body: many0!(statement) >>
    ret: opt!(expr) >>
    t_right_brace >>
    (Block {body, ret: ret.unwrap_or(Expr::UnitValue), span})
)));

named!(statement(Span) -> Statement, ws_comments!(alt!(
    function => { |func| Statement::Function(func) } |
    var_decl => { |(name, expr)| Statement::Let(name, expr) } |
    do_parse!(e: expr >> t_semi >> (e)) => { |expr| Statement::Expr(expr) }
)));

named!(var_decl(Span) -> (IdentUnit, Expr), ws_comments!(do_parse!(
    t_let >>
    name: ident >>
    unit_span: position!() >>
    unit: opt!(compound_unit) >>
    t_becomes >>
    rhs: expr >>
    t_semi >>
    ((IdentUnit {name, unit: default_unitless!(unit, unit_span)}, rhs))
)));

named!(expr(Span) -> Expr, complete!(ws_comments!(do_parse!(
    first: term >>
    result: fold_many0!(
        tuple!(position!(), alt!(t_plus | t_minus), term),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "+" => Expr::Add(Box::new(acc), Box::new(rhs), span),
            "-" => Expr::Sub(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
))));

named!(term(Span) -> Expr, ws_comments!(do_parse!(
    first: pow >>
    result: fold_many0!(
        tuple!(position!(), alt!(t_star | t_slash | t_percent), pow),
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

named!(pow(Span) -> Expr, ws_comments!(do_parse!(
    first: factor_as >>
    result: fold_many0!(
        tuple!(position!(), t_caret, factor_as),
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
named!(factor_as(Span) -> Expr, ws_comments!(do_parse!(
    first: factor >>
    result: fold_many0!(
        tuple!(position!(), t_as, compound_unit),
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

named!(factor(Span) -> Expr, ws_comments!(alt!(
    tuple!(numeric_literal, position!(), opt!(compound_unit)) => {
        |(num, unit_span, unit): (_, _, Option<_>)| Expr::Number(num, default_unitless!(unit, unit_span))
    } |
    tuple!(position!(), fncall, position!(), opt!(compound_unit)) => {
        |(span, (path, args), unit_span, unit)| {
            maybe_convert_unit!(Expr::Call(path, args, span), unit, unit_span)
        }
    } |
    tuple!(position!(), ident_path, position!(), opt!(compound_unit)) => {
        |(span, path, unit_span, unit)| {
            maybe_convert_unit!(Expr::Ident(path, span), unit, unit_span)
        }
    } |
    tuple!(delimited!(t_left_paren, expr, t_right_paren), position!(), opt!(compound_unit)) => {
        |(expr, unit_span, unit)| {
            maybe_convert_unit!(expr, unit, unit_span)
        }
    } |
    tuple!(block, position!(), opt!(compound_unit)) => {
        |(block, unit_span, unit)| {
            maybe_convert_unit!(Expr::Block(Box::new(block)), unit, unit_span)
        }
    } |
    tuple!(position!(), t_return, expr) => {
        |(span, _, return_expr)| Expr::Return(Box::new(return_expr), span)
    } |
    t_unit => { |_| Expr::UnitValue }
)));

named!(fncall(Span) -> (IdentPath, Vec<Expr>), ws_comments!(do_parse!(
    name: ident_path >>
    args: delimited!(
        t_left_paren,
        separated_list_complete!(t_comma, expr),
        t_right_paren
    ) >>
    (name, args)
)));

named!(compound_unit(Span) -> UnitExpr, ws_comments!(do_parse!(
    first: unitterm >>
    result: fold_many0!(
        tuple!(position!(), unitterm),
        first,
        |acc, (span, rhs)| UnitExpr::Mul(Box::new(acc), Box::new(rhs), span)
    ) >>
    (result)
)));

named!(unitterm(Span) -> UnitExpr, ws_comments!(do_parse!(
    first: unitpow >>
    result: fold_many0!(
        ws_comments!(tuple!(position!(), alt!(t_star | t_slash), unitpow)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "*" => UnitExpr::Mul(Box::new(acc), Box::new(rhs), span),
            "/" => UnitExpr::Div(Box::new(acc), Box::new(rhs), span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named!(unitpow(Span) -> UnitExpr, ws_comments!(do_parse!(
    first: unitfactor >>
    result: fold_many0!(
        ws_comments!(tuple!(position!(), t_caret, integer_literal)),
        first,
        |acc, (span, op, rhs): (_, Span, _)| match op.fragment.0 {
            "^" => UnitExpr::Pow(Box::new(acc), rhs, span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named!(unitfactor(Span) -> UnitExpr, ws_comments!(alt!(
    unit => { |(u, span)| UnitExpr::Unit(u, span) } |
    delimited!(t_left_paren, compound_unit, t_right_paren)
)));

named!(unit(Span) -> (UnitName, Span), do_parse!(
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
    (match name.fragment.0 {
        "_" => None,
        name => Some(name),
    }, span)
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
                Ok((ref remaining, ref output)) if remaining.fragment.0.is_empty() => {
                    panic!("parse of `{}` succeeded (when it should have failed). Result: {:?}", $input, output);
                },
                _ => {}, // Expected
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
                Ok((ref remaining, ref output)) if remaining.fragment.0.is_empty() => {
                    panic!("parse of `{}` succeeded (when it should have failed). Result: {:?}", $input, output);
                },
                _ => {}, // Expected
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

    #[test]
    fn unitpow_parser() {
        test_parser2!(unitpow("") -> err);
        test_parser2!(unitpow("'a") -> ok);
        test_parser2!(unitpow("'km") -> ok);
        test_parser2!(unitpow("'_") -> ok);
        test_parser2!(unitpow("'_a") -> err);
        test_parser2!(unitpow("'a_b") -> ok);
        test_parser2!(unitpow("'kph") -> ok);

        // Disallow non-integer exponents
        test_parser2!(unitpow("'a^i") -> err);
        test_parser2!(unitpow("'km^i") -> err);
        test_parser2!(unitpow("'_^i") -> err);
        test_parser2!(unitpow("'a_b^i") -> err);
        test_parser2!(unitpow("'kph^i") -> err);

        test_parser2!(unitpow("'a^^2") -> err);
        test_parser2!(unitpow("'km^^2") -> err);
        test_parser2!(unitpow("'_^^2") -> err);
        test_parser2!(unitpow("'a_b^^2") -> err);
        test_parser2!(unitpow("'kph^^2") -> err);

        test_parser2!(unitpow("'a^2") -> ok);
        test_parser2!(unitpow("'km^2") -> ok);
        test_parser2!(unitpow("'_^2") -> ok);
        test_parser2!(unitpow("'a_b^2") -> ok);
        test_parser2!(unitpow("'kph^2") -> ok);

        test_parser2!(unitpow("'a ^ 2") -> ok);
        test_parser2!(unitpow("'km ^ 2") -> ok);
        test_parser2!(unitpow("'_ ^ 2") -> ok);
        test_parser2!(unitpow("'a_b ^ 2") -> ok);
        test_parser2!(unitpow("'kph ^ 2") -> ok);

        test_parser2!(unitpow("'a ^ 2   ^   3") -> ok);
        test_parser2!(unitpow("'km ^ 2  ^   3") -> ok);
        test_parser2!(unitpow("'_ ^ 2   ^   3") -> ok);
        test_parser2!(unitpow("'a_b ^ 2     ^   3") -> ok);
        test_parser2!(unitpow("'kph ^ 2     ^   3") -> ok);

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 7, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 15, line: 1, fragment: CompleteStr("") };
        test_parser2!(unitpow("'a ^ 2 ^ 3 ^ 4 ^ 5") -> ok,
            UnitExpr::Pow(
                Box::new(UnitExpr::Pow(
                    Box::new(UnitExpr::Pow(
                        Box::new(UnitExpr::Pow(
                            Box::new(UnitExpr::Unit(Some("a"), span1)),
                            2,
                            span2
                        )),
                        3,
                        span3,
                    )),
                    4,
                    span4,
                )),
                5,
                span5,
            )
        );
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

        test_parser2!(unitfactor("(( (  ( ('a))   )  )  )") -> ok);
        test_parser2!(unitfactor("(( (  ( ('km))   )  )  )") -> ok);
        test_parser2!(unitfactor("(( (  ( ('_))   )  )  )") -> ok);
        test_parser2!(unitfactor("(( (  ( ('a_b))   )  )  )") -> ok);
        test_parser2!(unitfactor("(( (  ( ('kph))   )  )  )") -> ok);

        // unbalanced
        test_parser2!(unitfactor("(( (  ( ('a)   )  )  )") -> err);
        test_parser2!(unitfactor("((   ( ('kph))   )  )  )") -> err);
    }

    #[test]
    fn unit_parser() {
        let span = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        test_parser2!(unit("") -> err);
        test_parser2!(unit("'a") -> ok, (Some("a"), span));
        test_parser2!(unit("'km") -> ok, (Some("km"), span));
        test_parser2!(unit("'_") -> ok, (None, span));
        test_parser2!(unit("'a_b") -> ok, (Some("a_b"), span));
        test_parser2!(unit("'kph") -> ok, (Some("kph"), span));
        test_parser2!(unit("'_a") -> err);
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
