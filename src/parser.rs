pub use nom::Err as Error;
use nom::{alpha, digit, recognize_float, sp, types::CompleteStr};
use nom_locate::LocatedSpan;
use num_traits::cast::ToPrimitive;
use rust_decimal::Decimal;

use ast::*;

pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

pub type ParseResult<'a, T> = Result<T, Error<Span<'a>>>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    let input = Span::new(CompleteStr(input));
    match program(input) {
        Ok((remaining, program)) => {
            assert!(remaining.fragment.0.is_empty(), "bug: parser did not completely read input");
            Ok(program)
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
        $id.unwrap_or_else(|| UnitExpr::Unit(UnitName::unitless(), $span))
    };
}

macro_rules! ws_comments {
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, whitespace_comment, $($args)*)
    }
  )
}

named!(program(Span) -> Program,
    map!(exact!(complete!(
        delimited!(many0!(whitespace_comment), many0!(decl), many0!(whitespace_comment))
    )), |decls| Program {decls})
);

named!(decl(Span) -> Decl, ws_comments!(alt!(
    constant => { Decl::Constant } |
    unit_decl => { Decl::UnitDecl } |
    conversion_factor => { Decl::ConversionDecl } |
    function => { Decl::Function } |
    terminated!(macro_invoke, t_semi) => { Decl::MacroInvoke }
)));

named!(constant(Span) -> Constant, ws_comments!(do_parse!(
    span: position!() >>
    tag!("pub") >>
    t_const >>
    name: ident >>
    unit_span: position!() >>
    unit: opt!(compound_unit) >>
    t_becomes >>
    value: expr >>
    t_semi >>
    (Constant {name, unit: default_unitless!(unit, unit_span), value, span})
)));

named!(unit_decl(Span) -> UnitDecl, ws_comments!(do_parse!(
    attrs: many0!(attribute) >>
    span: position!() >>
    t_unit_decl >>
    unit: unit >>
    alias_for: opt!(preceded!(t_alias, compound_unit)) >>
    t_semi >>
    (UnitDecl {attrs, unit_name: unit.0, alias_for, span})
)));

named!(conversion_factor(Span) -> ConversionDecl, ws_comments!(do_parse!(
    span: position!() >>
    t_conversion_decl >>
    left: expr >>
    t_eq >>
    right: expr >>
    t_semi >>
    (ConversionDecl {left, right, span})
)));

named!(macro_invoke(Span) -> MacroInvoke, ws_comments!(do_parse!(
    span: position!() >>
    name: macro_name >>
    tokens: macro_args >>
    (MacroInvoke {
        name,
        tokens,
        span,
    })
)));

named!(macro_name(Span) -> IdentPath, terminated!(ident_path, char!('!')));
named!(macro_args(Span) -> Vec<Token>, alt!(
    delimited!(t_left_paren, many0!(tt), t_right_paren) |
    delimited!(t_left_bracket, many0!(tt), t_right_bracket) |
    delimited!(t_left_brace, many0!(tt), t_right_brace)
));

named!(function(Span) -> Function, ws_comments!(do_parse!(
    attrs: many0!(attribute) >>
    span: position!() >>
    tag!("pub") >>
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
    span: position!() >>
    t_hash >>
    name_body: delimited!(t_left_bracket, tuple!(ident, many0!(tt)), t_right_bracket) >>
    (Attribute {name: name_body.0, tokens: name_body.1, span})
)));

named!(block(Span) -> Block, ws_comments!(do_parse!(
    t_left_brace >>
    span: position!() >>
    body: many0!(statement) >>
    ret: opt!(expr) >>
    end_span: t_right_brace >>
    (Block {body, ret: ret.unwrap_or(Expr::UnitValue(end_span)), span})
)));

named!(statement(Span) -> Statement, ws_comments!(alt!(
    function => { Statement::Function } |
    let_decl => { Statement::Let } |
    terminated!(expr, t_semi) => { Statement::Expr }
)));

named!(let_decl(Span) -> LetDecl, ws_comments!(do_parse!(
    span: position!() >>
    t_let >>
    label: ident >>
    expected_unit: opt!(compound_unit) >>
    t_becomes >>
    value: expr >>
    t_semi >>
    (LetDecl {label, expected_unit, value, span})
)));

named!(expr(Span) -> Expr, complete!(ws_comments!(do_parse!(
    first: term >>
    result: fold_many0!(
        ws_comments!(tuple!(position!(), alt!(t_plus | t_minus), term)),
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
        ws_comments!(tuple!(position!(), alt!(t_star | t_slash | t_percent), pow)),
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
        ws_comments!(tuple!(position!(), t_caret, factor_as)),
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
        ws_comments!(tuple!(position!(), t_as, compound_unit)),
        first,
        |acc, (span, op, unit): (_, Span, _)| match op.fragment.0 {
            "as" => Expr::ConvertTo(Box::new(acc), unit, span),
            _ => unreachable!(),
        }
    ) >>
    (result)
)));

named!(factor(Span) -> Expr, ws_comments!(alt!(
    tuple!(numeric_literal, position!(), opt!(compound_unit)) => {
        |(num, unit_span, unit): (_, _, Option<_>)| Expr::Number(num, default_unitless!(unit, unit_span))
    } |
    tuple!(position!(), fncall) => {
        |(span, (path, args))| Expr::Call(path, args, span)
    } |
    macro_invoke => { Expr::MacroCall } |
    tuple!(position!(), ident_path, position!(), opt!(compound_unit)) => {
        |(span, path, unit_span, unit)| match unit {
            None => Expr::Ident(path, span),
            Some(unit) => Expr::ConvertTo(
                Box::new(Expr::Ident(path, span)),
                unit,
                unit_span,
            ),
        }
    } |
    delimited!(t_left_paren, expr, t_right_paren) |
    block => {
        |block| Expr::Block(Box::new(block))
    } |
    tuple!(position!(), t_return, expr) => {
        |(span, _, return_expr)| Expr::Return(Box::new(return_expr), span)
    } |
    t_unit => { Expr::UnitValue }
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
        ws_comments!(tuple!(position!(), unitterm)),
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
        |acc, (span, op, rhs): (_, Span, Decimal)| match op.fragment.0 {
            "^" => UnitExpr::Pow(Box::new(acc), rhs.to_i64().expect("i64 overflow"), span),
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
        "_" => UnitName::unitless(),
        name => UnitName::from(name),
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
    tuple!(position!(), integer_literal) => { |(span, value)| NumericLiteral {value, span} } |
    tuple!(position!(), float_literal) => { |(span, value)| NumericLiteral {value, span} }
));

named!(integer_literal(Span) -> Decimal, do_parse!(
    literal: flat_map!(recognize!(
        tuple!(
            opt!(alt!(t_plus | t_minus)),
            many1!(digit)
        )
    ), parse_to!(i64)) >>
    // Cannot be followed by any part of a float
    alt!(not!(not!(eof!())) | not!(alt!(char!('.') | char!('e') | char!('E')))) >>
    (Decimal::new(literal, 0))
));

named!(float_literal(Span) -> Decimal,
    map!(recognize!(tuple!(not!(eof!()), call!(recognize_float))), |lit| {
        let mut lit = lit.fragment.0.to_lowercase();
        if !lit.contains("e") {
            lit.extend("e0".chars());
        }
        Decimal::from_scientific(&lit).unwrap()
    })
);

macro_rules! keyword {
    ($parser:ident, $tag:expr) => {
        named!($parser(Span) -> Span, terminated!(tag!($tag), not!(alpha)));
    };
}

named!(t_left_brace(Span) -> Span, tag!("{"));
named!(t_right_brace(Span) -> Span, tag!("}"));
named!(t_left_paren(Span) -> Span, tag!("("));
named!(t_right_paren(Span) -> Span, tag!(")"));
named!(t_left_bracket(Span) -> Span, tag!("["));
named!(t_right_bracket(Span) -> Span, tag!("]"));
named!(t_eq(Span) -> Span, tag!("=="));
named!(t_ne(Span) -> Span, tag!("!="));
named!(t_gt(Span) -> Span, tag!(">"));
named!(t_ge(Span) -> Span, tag!(">="));
named!(t_le(Span) -> Span, tag!("<="));
named!(t_lt(Span) -> Span, tag!("<"));
named!(t_plus(Span) -> Span, tag!("+"));
named!(t_minus(Span) -> Span, tag!("-"));
named!(t_star(Span) -> Span, tag!("*"));
named!(t_slash(Span) -> Span, tag!("/"));
named!(t_percent(Span) -> Span, tag!("%"));
named!(t_caret(Span) -> Span, tag!("^"));
named!(t_comma(Span) -> Span, tag!(","));
named!(t_hash(Span) -> Span, tag!("#"));
named!(t_semi(Span) -> Span, tag!(";"));
named!(t_unit(Span) -> Span, tag!("()"));
named!(t_for(Span) -> Span, tag!("for"));
named!(t_arrow(Span) -> Span, tag!("->"));
named!(t_becomes(Span) -> Span, tag!("="));

keyword!(t_fn, "fn");
keyword!(t_return, "return");
keyword!(t_as, "as");
keyword!(t_alias, "alias");
keyword!(t_let, "let");
keyword!(t_const, "const");
keyword!(t_unit_decl, "unit");
keyword!(t_conversion_decl, "conversion");

named!(whitespace_comment(Span) -> Span, alt!(comment | whitespace));

named!(whitespace(Span) -> Span, call!(sp));
named!(comment(Span) -> Span, recognize!(tuple!(tag!("//"), take_until_and_consume!("\n"))));

// tt = token tree (any tokens)
named!(tt(Span) -> Token, ws_comments!(alt!(
    delimited!(t_left_paren, many0!(tt), t_right_paren) => { Token::Parens } |
    delimited!(t_left_bracket, many0!(tt), t_right_bracket) => { Token::Brackets } |
    delimited!(t_left_brace, many0!(tt), t_right_brace) => { Token::Braces } |
    macro_invoke => { Token::MacroInvoke } |
    function => { Token::Function } |
    attribute => { Token::Attribute } |
    block => { Token::Block } |
    expr => { Token::Expr } |
    compound_unit => { Token::UnitExpr } |
    ident_path => { Token::IdentPath } |
    numeric_literal => { Token::NumericLiteral } |
    t_eq => { |_| Token::Equals } |
    t_ne => { |_| Token::NotEquals } |
    t_gt => { |_| Token::GreaterThan } |
    t_ge => { |_| Token::GreaterThanOrEqual } |
    t_le => { |_| Token::LessThanOrEqual } |
    t_lt => { |_| Token::LessThan } |
    t_plus => { |_| Token::Plus } |
    t_minus => { |_| Token::Minus } |
    t_star => { |_| Token::Star } |
    t_slash => { |_| Token::Slash } |
    t_percent => { |_| Token::Percent } |
    t_caret => { |_| Token::Caret } |
    t_comma => { |_| Token::Comma } |
    t_hash => { |_| Token::Hash } |
    t_semi => { |_| Token::Semi } |
    t_return => { |_| Token::Return } |
    t_unit => { |_| Token::Unit } |
    t_for => { |_| Token::For } |
    t_as => { |_| Token::As } |
    t_arrow => { |_| Token::Arrow } |
    t_alias => { |_| Token::Alias } |
    t_let => { |_| Token::Let } |
    t_const => { |_| Token::Const } |
    t_becomes => { |_| Token::Becomes } |
    t_unit_decl => { |_| Token::UnitKeyword } |
    t_conversion_decl => { |_| Token::Conversion }
)));

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parser {
        ($parser:ident ( $input:expr ) -> ok) => {
            let input = Span::new(CompleteStr($input));
            match $parser(input) {
                Ok((remaining, _)) => {
                    assert!(remaining.fragment.0.is_empty(),
                        "fail: parser did not completely read input for: `{}`\nRemaining: `{}`", $input, remaining.fragment.0);
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
                        "fail: parser did not completely read input for: `{}`\nRemaining: `{}`", $input, remaining.fragment.0);
                    assert_eq!(output, $expected, "Incorrect result for parse of input: `{}`", $input);
                },
                Err(err) => panic!("parse of `{}` failed. Error: {:?}", $input, err),
            }
        };
    }

    #[test]
    fn program_parser() {
        test_parser!(program("") -> ok);
        test_parser!(program(" ") -> ok);
        test_parser!(program("// heelloooooo!\n") -> ok);
        test_parser!(program("
        // heelloooooo!
        // what's up

         // hello
         // foo // foo

         // hiiii
        ") -> ok);
        test_parser!(program("
        pub fn foo() {}
        pub fn foo() {}

        // foo is a foo
        pub fn foo() {}
        ") -> ok);
        test_parser!(program("
        pub fn velocity() -> 'm / 's {
            let F = 5.2 'N;
            let m = 150 'lbs;
            let t = 30 'min;
            // Answer is automatically converted to the unit of the variable.
            // It is an error if the conversion is impossible.
            let v 'km / 'hour = F / m * t;
            v
        }
        ") -> ok);
    }

    #[test]
    fn function_parser() {
        test_parser!(function("") -> err);
        test_parser!(function("pub fn main() {}") -> ok);
        test_parser!(function("pub fn main() -> 'a {}") -> ok);
        test_parser!(function("pub fn() -> 'a {}") -> ok);
        test_parser!(function("pub fn main() -> 'km / 'h {

        }") -> ok);
        test_parser!(function("fnmain() -> 'a {}") -> err);
    }

    #[test]
    fn let_decl_parser() {
        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 8, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        test_parser!(let_decl("let F = 5.2 'N;") -> ok,
            LetDecl {
                label: "F",
                expected_unit: None,
                value: Expr::Number(
                    NumericLiteral {value: Decimal::from_scientific("5.2e0").unwrap(), span: span4},
                    UnitExpr::Unit(UnitName::from("N"), span5),
                ),
                span: span1,
            }
        );
    }

    #[test]
    fn expr_parser() {
        test_parser!(expr("") -> err);
        test_parser!(expr("1 + 3 'a") -> ok);
        test_parser!(expr("1 + 3 'a * (4 / 6 'a)") -> ok);
        test_parser!(expr("1 + 3 'a * ((4 / 6 'a) / 22 'b ^ 2)") -> ok);
        test_parser!(expr("1 + 3 'a * ((4 / 6 'a) + (331.2 'a + 4.2) / 1 'b ^ 2)") -> ok);
    }

    #[test]
    fn compound_unit_parser() {
        test_parser!(compound_unit("") -> err);
        test_parser!(compound_unit("'a") -> ok);
        test_parser!(compound_unit("'a'b") -> ok);
        test_parser!(compound_unit("'a 'b") -> ok);
        test_parser!(compound_unit("'a 'b / 'c") -> ok);

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 10, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a * 'kph / 'b") -> ok,
            UnitExpr::Div(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                span4,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 13, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a * ('kph / 'b)") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                    span4,
                )),
                span2,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 13, line: 1, fragment: CompleteStr("") };
        let span6 = Span { offset: 17, line: 1, fragment: CompleteStr("") };
        let span7 = Span { offset: 19, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a * ('kph / 'b) * 'b") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Div(
                        Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                        Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                        span4,
                    )),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("b"), span7)),
                span6,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 10, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a / 'kph * 'b") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                span4,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 13, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a / ('kph * 'b)") -> ok,
            UnitExpr::Div(
                Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                    span4,
                )),
                span2,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 8, line: 1, fragment: CompleteStr("") };
        let span6 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        let span7 = Span { offset: 15, line: 1, fragment: CompleteStr("") };
        let span8 = Span { offset: 18, line: 1, fragment: CompleteStr("") };
        let span9 = Span { offset: 20, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'b * 'a ^ 2 * ('e / 'f)") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span1)),
                    Box::new(UnitExpr::Pow(
                        Box::new(UnitExpr::Unit(UnitName::from("a"), span3)),
                        Decimal::new(2, 0),
                        span4,
                    )),
                    span2,
                )),
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("e"), span7)),
                    Box::new(UnitExpr::Unit(UnitName::from("f"), span9)),
                    span8,
                )),
                span6,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 8, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a 'b / 'c 'd") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Div(
                        Box::new(UnitExpr::Unit(UnitName::from("b"), span2)),
                        Box::new(UnitExpr::Unit(UnitName::from("c"), span4)),
                        span3,
                    )),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("d"), span5)),
                span5,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 10, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        let span6 = Span { offset: 15, line: 1, fragment: CompleteStr("") };
        let span7 = Span { offset: 18, line: 1, fragment: CompleteStr("") };
        let span8 = Span { offset: 20, line: 1, fragment: CompleteStr("") };
        let span9 = Span { offset: 25, line: 1, fragment: CompleteStr("") };
        let span10 = Span { offset: 27, line: 1, fragment: CompleteStr("") };
        test_parser!(compound_unit("'a * 'kph / 'b 'a * 'kph / 'b") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Mul(
                        Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                        Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                        span2,
                    )),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                    span4,
                )),
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Mul(
                        Box::new(UnitExpr::Unit(UnitName::from("a"), span6)),
                        Box::new(UnitExpr::Unit(UnitName::from("kph"), span8)),
                        span7,
                    )),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span10)),
                    span9,
                )),
                span6,
            )
        );
    }

    #[test]
    fn unitterm_parser() {
        test_parser!(unitterm("") -> err);
        test_parser!(unitterm("'a") -> ok);
        test_parser!(unitterm("'km") -> ok);
        test_parser!(unitterm("'_") -> ok);
        test_parser!(unitterm("'a_b") -> ok);
        test_parser!(unitterm("'kph") -> ok);

        test_parser!(unitterm("'a*'b") -> ok);
        test_parser!(unitterm("'km*'b") -> ok);
        test_parser!(unitterm("'_*'b") -> ok);
        test_parser!(unitterm("'a_b*'b") -> ok);
        test_parser!(unitterm("'kph*'b") -> ok);

        test_parser!(unitterm("'a * 'b") -> ok);
        test_parser!(unitterm("'km * 'b") -> ok);
        test_parser!(unitterm("'_ * 'b") -> ok);
        test_parser!(unitterm("'a_b * 'b") -> ok);
        test_parser!(unitterm("'kph * 'b") -> ok);

        test_parser!(unitterm("'a/'b") -> ok);
        test_parser!(unitterm("'km/'b") -> ok);
        test_parser!(unitterm("'_/'b") -> ok);
        test_parser!(unitterm("'a_b/'b") -> ok);
        test_parser!(unitterm("'kph/'b") -> ok);

        test_parser!(unitterm("'a / 'b") -> ok);
        test_parser!(unitterm("'km / 'b") -> ok);
        test_parser!(unitterm("'_ / 'b") -> ok);
        test_parser!(unitterm("'a_b / 'b") -> ok);
        test_parser!(unitterm("'kph / 'b") -> ok);

        test_parser!(unitterm("'a * 'a / 'b") -> ok);
        test_parser!(unitterm("'a * 'km / 'b") -> ok);
        test_parser!(unitterm("'a * '_ / 'b") -> ok);
        test_parser!(unitterm("'a * 'a_b / 'b") -> ok);

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 10, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        test_parser!(unitterm("'a * 'kph / 'b") -> ok,
            UnitExpr::Div(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                span4,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 13, line: 1, fragment: CompleteStr("") };
        test_parser!(unitterm("'a * ('kph / 'b)") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                    span4,
                )),
                span2,
            )
        );

        test_parser!(unitterm("'a / 'a * 'b") -> ok);
        test_parser!(unitterm("'a / 'km * 'b") -> ok);
        test_parser!(unitterm("'a / '_ * 'b") -> ok);
        test_parser!(unitterm("'a / 'a_b * 'b") -> ok);

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 10, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        test_parser!(unitterm("'a / 'kph * 'b") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    span2,
                )),
                Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                span4,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 6, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 13, line: 1, fragment: CompleteStr("") };
        test_parser!(unitterm("'a / ('kph * 'b)") -> ok,
            UnitExpr::Div(
                Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("kph"), span3)),
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span5)),
                    span4,
                )),
                span2,
            )
        );

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 5, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 8, line: 1, fragment: CompleteStr("") };
        let span6 = Span { offset: 12, line: 1, fragment: CompleteStr("") };
        let span7 = Span { offset: 15, line: 1, fragment: CompleteStr("") };
        let span8 = Span { offset: 18, line: 1, fragment: CompleteStr("") };
        let span9 = Span { offset: 20, line: 1, fragment: CompleteStr("") };
        test_parser!(unitterm("'b * 'a ^ 2 * ('e / 'f)") -> ok,
            UnitExpr::Mul(
                Box::new(UnitExpr::Mul(
                    Box::new(UnitExpr::Unit(UnitName::from("b"), span1)),
                    Box::new(UnitExpr::Pow(
                        Box::new(UnitExpr::Unit(UnitName::from("a"), span3)),
                        Decimal::new(2, 0),
                        span4,
                    )),
                    span2,
                )),
                Box::new(UnitExpr::Div(
                    Box::new(UnitExpr::Unit(UnitName::from("e"), span7)),
                    Box::new(UnitExpr::Unit(UnitName::from("f"), span9)),
                    span8,
                )),
                span6,
            )
        );
    }

    #[test]
    fn unitpow_parser() {
        test_parser!(unitpow("") -> err);
        test_parser!(unitpow("'a") -> ok);
        test_parser!(unitpow("'km") -> ok);
        test_parser!(unitpow("'_") -> ok);
        test_parser!(unitpow("'_a") -> err);
        test_parser!(unitpow("'a_b") -> ok);
        test_parser!(unitpow("'kph") -> ok);

        // Disallow non-integer exponents
        test_parser!(unitpow("'a^i") -> err);
        test_parser!(unitpow("'km^i") -> err);
        test_parser!(unitpow("'_^i") -> err);
        test_parser!(unitpow("'a_b^i") -> err);
        test_parser!(unitpow("'kph^i") -> err);

        test_parser!(unitpow("'a^^2") -> err);
        test_parser!(unitpow("'km^^2") -> err);
        test_parser!(unitpow("'_^^2") -> err);
        test_parser!(unitpow("'a_b^^2") -> err);
        test_parser!(unitpow("'kph^^2") -> err);

        test_parser!(unitpow("'a^2") -> ok);
        test_parser!(unitpow("'km^2") -> ok);
        test_parser!(unitpow("'_^2") -> ok);
        test_parser!(unitpow("'a_b^2") -> ok);
        test_parser!(unitpow("'kph^2") -> ok);

        test_parser!(unitpow("'a ^ 2") -> ok);
        test_parser!(unitpow("'km ^ 2") -> ok);
        test_parser!(unitpow("'_ ^ 2") -> ok);
        test_parser!(unitpow("'a_b ^ 2") -> ok);
        test_parser!(unitpow("'kph ^ 2") -> ok);

        test_parser!(unitpow("'a ^ 2   ^   3") -> ok);
        test_parser!(unitpow("'km ^ 2  ^   3") -> ok);
        test_parser!(unitpow("'_ ^ 2   ^   3") -> ok);
        test_parser!(unitpow("'a_b ^ 2     ^   3") -> ok);
        test_parser!(unitpow("'kph ^ 2     ^   3") -> ok);

        let span1 = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        let span2 = Span { offset: 3, line: 1, fragment: CompleteStr("") };
        let span3 = Span { offset: 7, line: 1, fragment: CompleteStr("") };
        let span4 = Span { offset: 11, line: 1, fragment: CompleteStr("") };
        let span5 = Span { offset: 15, line: 1, fragment: CompleteStr("") };
        test_parser!(unitpow("'a ^ 2 ^ 3 ^ 4 ^ 5") -> ok,
            UnitExpr::Pow(
                Box::new(UnitExpr::Pow(
                    Box::new(UnitExpr::Pow(
                        Box::new(UnitExpr::Pow(
                            Box::new(UnitExpr::Unit(UnitName::from("a"), span1)),
                            Decimal::new(2, 0),
                            span2
                        )),
                        Decimal::new(3, 0),
                        span3,
                    )),
                    Decimal::new(4, 0),
                    span4,
                )),
                Decimal::new(5, 0),
                span5,
            )
        );
    }

    #[test]
    fn unitfactor_parser() {
        test_parser!(unitfactor("") -> err);
        test_parser!(unitfactor("'a") -> ok);
        test_parser!(unitfactor("'km") -> ok);
        test_parser!(unitfactor("'_") -> ok);
        test_parser!(unitfactor("'_a") -> err);
        test_parser!(unitfactor("'a_b") -> ok);
        test_parser!(unitfactor("'kph") -> ok);

        test_parser!(unitfactor("('a)") -> ok);
        test_parser!(unitfactor("('km)") -> ok);
        test_parser!(unitfactor("('_)") -> ok);
        test_parser!(unitfactor("('a_b)") -> ok);
        test_parser!(unitfactor("('kph)") -> ok);

        test_parser!(unitfactor("( 'a)") -> ok);
        test_parser!(unitfactor("( 'km)") -> ok);
        test_parser!(unitfactor("( '_)") -> ok);
        test_parser!(unitfactor("( 'a_b)") -> ok);
        test_parser!(unitfactor("( 'kph)") -> ok);

        test_parser!(unitfactor("('a )") -> ok);
        test_parser!(unitfactor("('km )") -> ok);
        test_parser!(unitfactor("('_ )") -> ok);
        test_parser!(unitfactor("('a_b )") -> ok);
        test_parser!(unitfactor("('kph )") -> ok);

        test_parser!(unitfactor("(      'a )") -> ok);
        test_parser!(unitfactor("(      'km )") -> ok);
        test_parser!(unitfactor("(      '_    )") -> ok);
        test_parser!(unitfactor("(      'a_b )") -> ok);
        test_parser!(unitfactor("(      'kph )") -> ok);

        test_parser!(unitfactor("(( (  ( ('a))   )  )  )") -> ok);
        test_parser!(unitfactor("(( (  ( ('km))   )  )  )") -> ok);
        test_parser!(unitfactor("(( (  ( ('_))   )  )  )") -> ok);
        test_parser!(unitfactor("(( (  ( ('a_b))   )  )  )") -> ok);
        test_parser!(unitfactor("(( (  ( ('kph))   )  )  )") -> ok);

        // unbalanced
        test_parser!(unitfactor("(( (  ( ('a)   )  )  )") -> err);
        test_parser!(unitfactor("((   ( ('kph))   )  )  )") -> err);
    }

    #[test]
    fn unit_parser() {
        let span = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        test_parser!(unit("") -> err);
        test_parser!(unit("'a") -> ok, (UnitName::from("a"), span));
        test_parser!(unit("'km") -> ok, (UnitName::from("km"), span));
        test_parser!(unit("'_") -> ok, (UnitName::unitless(), span));
        test_parser!(unit("'a_b") -> ok, (UnitName::from("a_b"), span));
        test_parser!(unit("'kph") -> ok, (UnitName::from("kph"), span));
        test_parser!(unit("'_a") -> err);

        // Must parse greek letters:
        test_parser!(unit("'\u{03b1}\u{03bf}\u{03a6}xx01\u{03a3}") -> ok);
        test_parser!(unit("'μm") -> ok); // micro meters

        // http://kestrel.nmt.edu/~raymond/software/howtos/greekscape.xhtml
        test_parser!(unit("'\u{03b1}") -> ok);
        test_parser!(unit("'\u{03b2}") -> ok);
        test_parser!(unit("'\u{03b3}") -> ok);
        test_parser!(unit("'\u{03b4}") -> ok);
        test_parser!(unit("'\u{03b5}") -> ok);
        test_parser!(unit("'\u{03b6}") -> ok);
        test_parser!(unit("'\u{03b7}") -> ok);
        test_parser!(unit("'\u{03b8}") -> ok);
        test_parser!(unit("'\u{03b9}") -> ok);
        test_parser!(unit("'\u{03ba}") -> ok);
        test_parser!(unit("'\u{03bb}") -> ok);
        test_parser!(unit("'\u{03bc}") -> ok);
        test_parser!(unit("'\u{03bd}") -> ok);
        test_parser!(unit("'\u{03be}") -> ok);
        test_parser!(unit("'\u{03bf}") -> ok);
        test_parser!(unit("'\u{03c0}") -> ok);
        test_parser!(unit("'\u{03c1}") -> ok);
        test_parser!(unit("'\u{03c2}") -> ok);
        test_parser!(unit("'\u{03c3}") -> ok);
        test_parser!(unit("'\u{03c4}") -> ok);
        test_parser!(unit("'\u{03c5}") -> ok);
        test_parser!(unit("'\u{03c6}") -> ok);
        test_parser!(unit("'\u{03c7}") -> ok);
        test_parser!(unit("'\u{03c8}") -> ok);
        test_parser!(unit("'\u{03c9}") -> ok);

        test_parser!(unit("'\u{0391}") -> ok);
        test_parser!(unit("'\u{0392}") -> ok);
        test_parser!(unit("'\u{0393}") -> ok);
        test_parser!(unit("'\u{0394}") -> ok);
        test_parser!(unit("'\u{0395}") -> ok);
        test_parser!(unit("'\u{0396}") -> ok);
        test_parser!(unit("'\u{0397}") -> ok);
        test_parser!(unit("'\u{0398}") -> ok);
        test_parser!(unit("'\u{0399}") -> ok);
        test_parser!(unit("'\u{039a}") -> ok);
        test_parser!(unit("'\u{039b}") -> ok);
        test_parser!(unit("'\u{039c}") -> ok);
        test_parser!(unit("'\u{039d}") -> ok);
        test_parser!(unit("'\u{039e}") -> ok);
        test_parser!(unit("'\u{039f}") -> ok);
        test_parser!(unit("'\u{03a0}") -> ok);
        test_parser!(unit("'\u{03a1}") -> ok);
        //test_parser!(unit("'\u{03a2}") -> ok); // reserved, not a letter!
        test_parser!(unit("'\u{03a3}") -> ok);
        test_parser!(unit("'\u{03a4}") -> ok);
        test_parser!(unit("'\u{03a5}") -> ok);
        test_parser!(unit("'\u{03a6}") -> ok);
        test_parser!(unit("'\u{03a7}") -> ok);
        test_parser!(unit("'\u{03a8}") -> ok);
        test_parser!(unit("'\u{03a9}") -> ok);
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

        // Must parse greek letters:
        test_parser!(ident("\u{03b1}\u{03bf}\u{03a6}xx01\u{03a3}") -> ok);

        // http://kestrel.nmt.edu/~raymond/software/howtos/greekscape.xhtml
        test_parser!(ident("\u{03b1}") -> ok);
        test_parser!(ident("\u{03b2}") -> ok);
        test_parser!(ident("\u{03b3}") -> ok);
        test_parser!(ident("\u{03b4}") -> ok);
        test_parser!(ident("\u{03b5}") -> ok);
        test_parser!(ident("\u{03b6}") -> ok);
        test_parser!(ident("\u{03b7}") -> ok);
        test_parser!(ident("\u{03b8}") -> ok);
        test_parser!(ident("\u{03b9}") -> ok);
        test_parser!(ident("\u{03ba}") -> ok);
        test_parser!(ident("\u{03bb}") -> ok);
        test_parser!(ident("\u{03bc}") -> ok);
        test_parser!(ident("\u{03bd}") -> ok);
        test_parser!(ident("\u{03be}") -> ok);
        test_parser!(ident("\u{03bf}") -> ok);
        test_parser!(ident("\u{03c0}") -> ok);
        test_parser!(ident("\u{03c1}") -> ok);
        test_parser!(ident("\u{03c2}") -> ok);
        test_parser!(ident("\u{03c3}") -> ok);
        test_parser!(ident("\u{03c4}") -> ok);
        test_parser!(ident("\u{03c5}") -> ok);
        test_parser!(ident("\u{03c6}") -> ok);
        test_parser!(ident("\u{03c7}") -> ok);
        test_parser!(ident("\u{03c8}") -> ok);
        test_parser!(ident("\u{03c9}") -> ok);

        test_parser!(ident("\u{0391}") -> ok);
        test_parser!(ident("\u{0392}") -> ok);
        test_parser!(ident("\u{0393}") -> ok);
        test_parser!(ident("\u{0394}") -> ok);
        test_parser!(ident("\u{0395}") -> ok);
        test_parser!(ident("\u{0396}") -> ok);
        test_parser!(ident("\u{0397}") -> ok);
        test_parser!(ident("\u{0398}") -> ok);
        test_parser!(ident("\u{0399}") -> ok);
        test_parser!(ident("\u{039a}") -> ok);
        test_parser!(ident("\u{039b}") -> ok);
        test_parser!(ident("\u{039c}") -> ok);
        test_parser!(ident("\u{039d}") -> ok);
        test_parser!(ident("\u{039e}") -> ok);
        test_parser!(ident("\u{039f}") -> ok);
        test_parser!(ident("\u{03a0}") -> ok);
        test_parser!(ident("\u{03a1}") -> ok);
        //test_parser!(ident("\u{03a2}") -> ok); // reserved, not a letter!
        test_parser!(ident("\u{03a3}") -> ok);
        test_parser!(ident("\u{03a4}") -> ok);
        test_parser!(ident("\u{03a5}") -> ok);
        test_parser!(ident("\u{03a6}") -> ok);
        test_parser!(ident("\u{03a7}") -> ok);
        test_parser!(ident("\u{03a8}") -> ok);
        test_parser!(ident("\u{03a9}") -> ok);
    }

    #[test]
    fn numeric_literal_parser() {
        let span = Span { offset: 0, line: 1, fragment: CompleteStr("") };
        test_parser!(numeric_literal("") -> err);
        test_parser!(numeric_literal("a") -> err);
        test_parser!(numeric_literal("'km") -> err);
        test_parser!(numeric_literal("0") -> ok, NumericLiteral {value: Decimal::from_scientific("0.0e0").unwrap(), span});
        test_parser!(numeric_literal("0") -> ok, NumericLiteral {value: Decimal::from_scientific("0.0e0").unwrap(), span});
        test_parser!(numeric_literal("1") -> ok, NumericLiteral {value: Decimal::from_scientific("1.0e0").unwrap(), span});
        test_parser!(numeric_literal("123") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e0").unwrap(), span});
        test_parser!(numeric_literal("-123") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.0e0").unwrap(), span});
        test_parser!(numeric_literal(".123") -> ok, NumericLiteral {value: Decimal::from_scientific("0.123e0").unwrap(), span});
        test_parser!(numeric_literal("-.123") -> ok, NumericLiteral {value: Decimal::from_scientific("-0.123e0").unwrap(), span});
        test_parser!(numeric_literal("123.") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e0").unwrap(), span});
        test_parser!(numeric_literal("-123.") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.0e0").unwrap(), span});
        test_parser!(numeric_literal("123.e1") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e1").unwrap(), span});
        test_parser!(numeric_literal("123.e-1") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e-1").unwrap(), span});
        test_parser!(numeric_literal("123.456e10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e10").unwrap(), span});
        test_parser!(numeric_literal("123.456e-10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e-10").unwrap(), span});
        test_parser!(numeric_literal("123.456E10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e10").unwrap(), span});
        test_parser!(numeric_literal("123.456E-10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e-10").unwrap(), span});
        test_parser!(numeric_literal("0.456E-10") -> ok, NumericLiteral {value: Decimal::from_scientific("0.456e-10").unwrap(), span});
        test_parser!(numeric_literal("123.0E-10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e-10").unwrap(), span});
        test_parser!(numeric_literal("0.456E10") -> ok, NumericLiteral {value: Decimal::from_scientific("0.456e10").unwrap(), span});
        test_parser!(numeric_literal("123.0E10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.0e10").unwrap(), span});
        test_parser!(numeric_literal("+123.456e10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e10").unwrap(), span});
        test_parser!(numeric_literal("+123.456e-10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e-10").unwrap(), span});
        test_parser!(numeric_literal("+123.456E10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e10").unwrap(), span});
        test_parser!(numeric_literal("+123.456E-10") -> ok, NumericLiteral {value: Decimal::from_scientific("123.456e-10").unwrap(), span});
        test_parser!(numeric_literal("-123.456e10") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.456e10").unwrap(), span});
        test_parser!(numeric_literal("-123.456e-10") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.456e-10").unwrap(), span});
        test_parser!(numeric_literal("-123.456E10") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.456e10").unwrap(), span});
        test_parser!(numeric_literal("-123.456E-10") -> ok, NumericLiteral {value: Decimal::from_scientific("-123.456e-10").unwrap(), span});
    }

    #[test]
    fn integer_literal_parser() {
        test_parser!(integer_literal("") -> err);
        test_parser!(integer_literal("a") -> err);
        test_parser!(integer_literal("'km") -> err);
        test_parser!(integer_literal("0") -> ok, Decimal::new(0, 0));
        test_parser!(integer_literal("1") -> ok, Decimal::new(1, 0));
        test_parser!(integer_literal("123") -> ok, Decimal::new(123, 0));
        test_parser!(integer_literal("-123") -> ok, Decimal::new(-123, 0));
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
        test_parser!(float_literal("0") -> ok, Decimal::from_scientific("0.0e0").unwrap());
        test_parser!(float_literal("1") -> ok, Decimal::from_scientific("1.0e0").unwrap());
        test_parser!(float_literal("123") -> ok, Decimal::from_scientific("123.0e0").unwrap());
        test_parser!(float_literal("-123") -> ok, Decimal::from_scientific("-123.0e0").unwrap());
        test_parser!(float_literal(".123") -> ok, Decimal::from_scientific("0.123e0").unwrap());
        test_parser!(float_literal("-.123") -> ok, Decimal::from_scientific("-0.123e0").unwrap());
        test_parser!(float_literal("123.") -> ok, Decimal::from_scientific("123.0e0").unwrap());
        test_parser!(float_literal("-123.") -> ok, Decimal::from_scientific("-123.0e0").unwrap());
        test_parser!(float_literal("123.e1") -> ok, Decimal::from_scientific("123.0e1").unwrap());
        test_parser!(float_literal("123.e-1") -> ok, Decimal::from_scientific("123.0e-1").unwrap());
        test_parser!(float_literal("123.456e10") -> ok, Decimal::from_scientific("123.456e10").unwrap());
        test_parser!(float_literal("123.456e-10") -> ok, Decimal::from_scientific("123.456e-10").unwrap());
        test_parser!(float_literal("123.456E10") -> ok, Decimal::from_scientific("123.456e10").unwrap());
        test_parser!(float_literal("123.456E-10") -> ok, Decimal::from_scientific("123.456e-10").unwrap());
        test_parser!(float_literal("0.456E-10") -> ok, Decimal::from_scientific("0.456e-10").unwrap());
        test_parser!(float_literal("123.0E-10") -> ok, Decimal::from_scientific("123.0e-10").unwrap());
        test_parser!(float_literal("0.456E10") -> ok, Decimal::from_scientific("0.456e10").unwrap());
        test_parser!(float_literal("123.0E10") -> ok, Decimal::from_scientific("123.0e10").unwrap());
        test_parser!(float_literal("+123.456e10") -> ok, Decimal::from_scientific("123.456e10").unwrap());
        test_parser!(float_literal("+123.456e-10") -> ok, Decimal::from_scientific("123.456e-10").unwrap());
        test_parser!(float_literal("+123.456E10") -> ok, Decimal::from_scientific("123.456e10").unwrap());
        test_parser!(float_literal("+123.456E-10") -> ok, Decimal::from_scientific("123.456e-10").unwrap());
        test_parser!(float_literal("-123.456e10") -> ok, Decimal::from_scientific("-123.456e10").unwrap());
        test_parser!(float_literal("-123.456e-10") -> ok, Decimal::from_scientific("-123.456e-10").unwrap());
        test_parser!(float_literal("-123.456E10") -> ok, Decimal::from_scientific("-123.456e10").unwrap());
        test_parser!(float_literal("-123.456E-10") -> ok, Decimal::from_scientific("-123.456e-10").unwrap());
    }

    #[test]
    fn whitespace_comment_parser() {
        test_parser!(whitespace_comment("") -> ok);
        test_parser!(whitespace_comment("// helloooo!\n") -> ok, Span::new(CompleteStr("// helloooo!\n")));
        test_parser!(whitespace_comment("// helloooo! // nested\n") -> ok, Span::new(CompleteStr("// helloooo! // nested\n")));
        // comments must have a newline at the end (at least for the timebeing)
        test_parser!(whitespace_comment("// helloooo!") -> err);
        test_parser!(whitespace_comment("  \n\t \r \n\n\n       ") -> ok, Span::new(CompleteStr("  \n\t \r \n\n\n       ")));
    }

    #[test]
    fn comment_parser() {
        test_parser!(comment("") -> err);
        test_parser!(comment("// helloooo!\n") -> ok, Span::new(CompleteStr("// helloooo!\n")));
        test_parser!(comment("// helloooo! // nested\n") -> ok, Span::new(CompleteStr("// helloooo! // nested\n")));
        // comments must have a newline at the end (at least for the timebeing)
        test_parser!(comment("// helloooo!") -> err);
    }

    #[test]
    fn whitespace_parser() {
        test_parser!(whitespace("") -> ok);
        test_parser!(whitespace("  \n\t \r \n\n\n       ") -> ok, Span::new(CompleteStr("  \n\t \r \n\n\n       ")));
    }
}
