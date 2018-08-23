use std::borrow::Cow;
use std::str::FromStr;

use bigdecimal::BigDecimal;
use num_traits::ToPrimitive;

use ast::*;
use ir::{Number, ConversionRatio};
use unit_graph::UnitGraph;
use symbols::SymbolTable;
use canonical::{self, CanonicalUnit};
use display_string::DisplayString;

#[derive(Debug, Clone)]
pub enum ConstExprError<'a> {
    UndeclaredName {
        name: Ident<'a>,
        span: Span<'a>,
    },
    UndeclaredUnit {
        name: UnitName<'a>,
        span: Span<'a>,
    },
    UnsupportedConstExpr {
        expr: Expr<'a>,
        span: Span<'a>,
    },
    ConversionFailed(ConversionFailed, Span<'a>),
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for ConstExprError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        ConstExprError::UndeclaredUnit {name, span}
    }
}

impl<'a> DisplayString for ConstExprError<'a> {
    fn display<'b>(&self, units: &UnitGraph<'b>) -> String {
        use self::ConstExprError::*;
        match self {
            UndeclaredName {name, span} => format!("Line {}: Undeclared name: {}", span.line, name),
            UndeclaredUnit {name, span} => format!("Line {}: Undeclared unit: {}", span.line, name),
            UnsupportedConstExpr {expr: _, span} => {
                format!("Line {}: Unsupported Constant Expression: Only a limited set of constant expressions can be evaluated at compile time", span.line)
            },
            ConversionFailed(err, span) => format!("Line {}: {}", span.line, err.display(units)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclError<'a> {
    UndeclaredName {
        name: Ident<'a>,
        span: Span<'a>,
    },
    UndeclaredUnit {
        name: UnitName<'a>,
        span: Span<'a>,
    },
    DuplicateUnitDecl {
        name: UnitName<'a>,
        span: Span<'a>,
    },
    UnknownAttribute {
        name: Ident<'a>,
        span: Span<'a>,
    },
    PrefixSystemSyntaxError {
        span: Span<'a>,
    },
    PrefixSystemUnknownTransform {
        transform: Ident<'a>,
        span: Span<'a>,
    },
    UnknownPrefixSystem {
        name: Ident<'a>,
        span: Span<'a>,
    },
    DuplicateConst {
        name: Ident<'a>,
        span: Span<'a>,
    },
    MismatchedUnit {
        expected: UnitExpr<'a>,
        found: CanonicalUnit,
        span: Span<'a>,
    },
    ConversionFailed(ConversionFailed, Span<'a>),
    ConstExprError(ConstExprError<'a>),
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for DeclError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        DeclError::UndeclaredUnit {name, span}
    }
}

impl<'a> From<ConstExprError<'a>> for DeclError<'a> {
    fn from(err: ConstExprError<'a>) -> Self {
        DeclError::ConstExprError(err)
    }
}

impl<'a> DisplayString for DeclError<'a> {
    fn display<'b>(&self, units: &UnitGraph<'b>) -> String {
        use self::DeclError::*;
        match self {
            UndeclaredName {name, span} => format!("Line {}: Undeclared name: {}", span.line, name),
            UndeclaredUnit {name, span} => format!("Line {}: Undeclared unit: {}", span.line, name),
            DuplicateUnitDecl {name, span} => format!("Line {}: Unit was declared multiple times: {}", span.line, name),
            UnknownAttribute {name, span} => format!("Line {}: Unknown attribute: {}", span.line, name),
            PrefixSystemSyntaxError {span} => format!("Line {}: Syntax error in prefix system declaration", span.line),
            PrefixSystemUnknownTransform {transform, span} => format!("Line {}: Unknown prefix system transform: `{}`", span.line, transform),
            UnknownPrefixSystem {name, span} => format!("Line {}: Unknown prefix system: {}", span.line, name),
            DuplicateConst {name, span} => format!("Line {}: const was declared multiple times: {}", span.line, name),
            MismatchedUnit {expected, found, span} => {
                format!("Line {}: Mismatched unit:\n    expected: {}\n    found: {}",
                    span.line,
                    //TODO: Format unit expr properly
                    CanonicalUnit::from_unit_expr(expected, units).unwrap().display(units),
                    found.display(units),
                )
            },
            ConversionFailed(err, span) => format!("Line {}: {}", span.line, err.display(units)),
            ConstExprError(err) => err.display(units),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EvalError<'a> {
    UndeclaredName {
        name: Ident<'a>,
        span: Span<'a>,
    },
    UndeclaredUnit {
        name: UnitName<'a>,
        span: Span<'a>,
    },
    ConversionFailed(ConversionFailed, Span<'a>),
    ConstExprError(ConstExprError<'a>),
    DivideByZero {
        span: Span<'a>,
    },
    ExponentMustBeUnitless {
        found: CanonicalUnit,
        span: Span<'a>,
    },
    /// Exponents must be unitless *integer const expressions*
    UnsupportedExponent {
        value: Number,
        span: Span<'a>,
    },
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for EvalError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        EvalError::UndeclaredUnit {name, span}
    }
}

impl<'a> From<ConstExprError<'a>> for EvalError<'a> {
    fn from(err: ConstExprError<'a>) -> Self {
        EvalError::ConstExprError(err)
    }
}

impl<'a> DisplayString for EvalError<'a> {
    fn display<'b>(&self, units: &UnitGraph<'b>) -> String {
        use self::EvalError::*;
        match self {
            UndeclaredName {name, span} => format!("Line {}: Undeclared name: {}", span.line, name),
            UndeclaredUnit {name, span} => format!("Line {}: Undeclared unit: {}", span.line, name),
            ConversionFailed(err, span) => format!("Line {}: {}", span.line, err.display(units)),
            ConstExprError(err) => err.display(units),
            DivideByZero {span} => format!("Line {}: attempt to divide by zero", span.line),
            ExponentMustBeUnitless {found, span} => {
                format!("Line {}: can only raise to the power of a unitless quantity\n    expected: '_\n    found: {}",
                    span.line, found.display(units))
            },
            UnsupportedExponent {value, span} => {
                format!("Line {}: Exponent must be a unitless integer constant expression\n    found: {}",
                    span.line, value.display(units))
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConversionFailed {
    start: CanonicalUnit,
    end: CanonicalUnit,
}

impl DisplayString for ConversionFailed {
    fn display<'a>(&self, units: &UnitGraph<'a>) -> String {
        format!("Cannot convert from {} to {}", self.start.display(units), self.end.display(units))
    }
}

#[derive(Debug, Default)]
struct PrefixSystemArgs {
    lowercase: bool,
    longprefix: bool,
}

impl PrefixSystemArgs {
    pub fn from_tokens<'a>(mut tokens: &[Token<'a>], span: Span<'a>) -> Result<Self, DeclError<'a>> {
        let mut args = PrefixSystemArgs::default();

        loop {
            tokens = args.expect_single_arg(tokens, span)?;
            // Reached the end of the arguments
            if tokens.is_empty() {
                break;
            }
            // Must have at least one comma plus something else (i.e. no trailing comma allowed)
            else if tokens.len() > 1 {
                match tokens[0] {
                    Token::Comma  => {},
                    _ => return Err(DeclError::PrefixSystemSyntaxError {span})
                }
                tokens = &tokens[1..];
            }
            else {
                return Err(DeclError::PrefixSystemSyntaxError {span});
            }
        }
        Ok(args)
    }

    // Extracts a single argument and returns the remaining tokens
    fn expect_single_arg<'a, 'b>(&mut self, tokens: &'b [Token<'a>], span: Span<'a>) -> Result<&'b [Token<'a>], DeclError<'a>> {
        // Must at least have "ARG_NAME" "=" "VALUE"
        if tokens.len() < 3 {
            return Err(DeclError::PrefixSystemSyntaxError {span})
        }
        let arg_name = match tokens[0] {
            Token::Expr(Expr::Ident(ref path, _)) if path.len() == 1 => path[0],
            _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
        };

        match tokens[1] {
            Token::Becomes => {}, // GOOD
            _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
        }

        let value = &tokens[2];

        match arg_name {
            "transform" => self.collect_transforms(value, span)?,
            _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
        }

        Ok(&tokens[3..])
    }

    fn collect_transforms<'a>(&mut self, value: &Token<'a>, span: Span<'a>) -> Result<(), DeclError<'a>> {
        if let Token::Brackets(tokens) = value {
            let mut tokens = &tokens[..];
            while !tokens.is_empty() {
                let (transform, span) = match tokens[0] {
                    Token::Expr(Expr::Ident(ref path, span)) if path.len() == 1 => (path[0], span),
                    _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
                };

                match transform {
                    "lowercase" => self.lowercase = true,
                    "longprefix" => self.longprefix = true,
                    _ => return Err(DeclError::PrefixSystemUnknownTransform {transform, span}),
                }

                // If there is anything left now, we expect a comma AND something else after it
                // (i.e. no trailing comma)
                match tokens.len() {
                    // must be at least one because we got to this point
                    0 => unreachable!(),
                    // at the end
                    1 => tokens = &tokens[1..],
                    // must be a trailing comma or some other problem
                    2 => return Err(DeclError::PrefixSystemSyntaxError {span}),
                    // must be a comma followed by some more stuff
                    _ => {
                        match tokens[1] {
                            Token::Comma => {},
                            _ => return Err(DeclError::PrefixSystemSyntaxError {span})
                        }
                        tokens = &tokens[2..];
                    }
                }
            }

            Ok(())
        }
        else {
            Err(DeclError::PrefixSystemSyntaxError {span})
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn format_with_units<T: DisplayString>(&self, value: &T) -> String {
        value.display(&self.units)
    }

    /// Load declarations into the global scope of the interpreter
    pub fn load_decls(&mut self, program: &Program<'a>) -> Result<(), DeclError<'a>> {
        // We need to do this in several passes in order to satisfy the potential items each type
        // of declaration might need.

        // Macros need to be expanded before anything else
        let program = self.expand_macros(Cow::Borrowed(program))?;

        self.load_unit_decls(&program)?;
        // Constants need units so we can verify their declared unit if provided.
        self.load_const_decls(&program)?;
        // Conversions need both units and constants (+ const functions) so we can evaluate their
        // declared ratios
        self.load_conversion_decls(&program)?;

        // Functions will need the units loaded so we can verify that their parameter and return
        // units are declared. We also check the body of each function at this point to make sure
        // all unit conversions are mathematically valid so we need those loaded as well.
        self.load_fn_decls(&program)?;

        Ok(())
    }

    /// Expand any macros used in the program
    ///
    /// Any declared macros will be collected along the way and used as necessary while
    /// recursively expanding the macros
    fn expand_macros<'c>(&self, program: Cow<'c, Program<'a>>) -> Result<Cow<'c, Program<'a>>, DeclError<'a>> {
        // By using Cow here, we avoid an unnecessary clone if the program has no macros

        for decl in &program.decls {
            //TODO: Macro declarations will be processed here eventually

            if let Decl::MacroInvoke(_) = decl {
                unimplemented!();
            }
        }
        Ok(program)
    }

    /// Load declared units into the global unit graph of the interpreter
    fn load_unit_decls(&mut self, program: &Program<'a>) -> Result<(), DeclError<'a>> {
        for decl in &program.decls {
            if let &Decl::UnitDecl(UnitDecl {ref attrs, ref unit_name, span, ..}) = decl {
                // Must be inserted before prefix system attributes are computed
                self.units.insert_unit(unit_name.clone(), span)
                    .map_err(|_| DeclError::DuplicateUnitDecl {name: unit_name.clone(), span})?;

                for &Attribute {name, ref tokens, span} in attrs {
                    match name {
                        "prefix" => self.apply_prefix_system(unit_name, span, tokens)?,
                        _ => return Err(DeclError::UnknownAttribute {name, span}),
                    }
                }
            }
        }
        Ok(())
    }

    /// Load declared constants and `const fn`s into the global scope of the interpreter
    fn load_const_decls(&mut self, program: &Program<'a>) -> Result<(), DeclError<'a>> {
        for decl in &program.decls {
            //TODO: const fn declarations will be processed and unit checked here eventually

            if let &Decl::Constant(Constant {name, unit: ref unit_expr, ref value, span}) = decl {
                let target_unit = CanonicalUnit::from_unit_expr(unit_expr, &self.units)?;
                let value = self.reduce_const_expr(value)?;
                if value.unit != target_unit {
                    return Err(DeclError::MismatchedUnit {
                        expected: unit_expr.clone(),
                        found: value.unit,
                        span,
                    });
                }

                self.symbols.insert_const(name, value, span).map_err(|_| {
                    DeclError::DuplicateConst {name, span}
                })?;
            }
        }
        Ok(())
    }

    /// Load unit conversions and evaluate any const exprs found within them
    fn load_conversion_decls(&mut self, program: &Program<'a>) -> Result<(), DeclError<'a>> {
        for decl in &program.decls {
            match decl {
                // Aliases need to be processed later when all the units have already been walked
                Decl::UnitDecl(UnitDecl {unit_name, alias_for: Some(alias), ..}) => {
                    let unit_id = self.units.unit_id(unit_name)
                        .expect("bug: unit should have already been declared");
                    let left_unit = CanonicalUnit::from(unit_id);
                    let right_unit = CanonicalUnit::from_unit_expr(alias, &self.units)?;

                    // Alias means: 1 left_unit == 1 right_unit
                    self.units.add_conversion(ConversionRatio {
                        left: Number {
                            value: BigDecimal::from(1),
                            unit: left_unit,
                        },
                        right: Number {
                            value: BigDecimal::from(1),
                            unit: right_unit,
                        },
                    })
                },
                Decl::ConversionDecl(ConversionDecl {left, right, ..}) => {
                    let left = self.reduce_const_expr(left)?;
                    let right = self.reduce_const_expr(right)?;
                    self.units.add_conversion(ConversionRatio {left, right});
                },
                _ => {},
            }
        }
        Ok(())
    }

    /// Load function declarations into the global scope and check their bodies to ensure that any
    /// conversions are valid
    fn load_fn_decls(&mut self, program: &Program<'a>) -> Result<(), DeclError<'a>> {
        for decl in &program.decls {
            if let Decl::Function(_) = decl {
                unimplemented!();
            }
        }
        Ok(())
    }

    fn apply_prefix_system(&mut self, unit_name: &UnitName<'a>, span: Span<'a>, tokens: &[Token<'a>]) -> Result<(), DeclError<'a>> {
        if tokens.len() != 1 {
            return Err(DeclError::PrefixSystemSyntaxError {span});
        }

        match tokens[0] {
            Token::Parens(ref tokens) if tokens.len() >= 1 => match tokens[0] {
                Token::Expr(Expr::Ident(ref path, span)) => if path.len() == 1 {
                    match path[0] {
                        "SI" => self.apply_si_prefix_system(unit_name, span, &tokens[1..])?,
                        name => return Err(DeclError::UnknownPrefixSystem {name, span}),
                    }
                } else {
                    return Err(DeclError::PrefixSystemSyntaxError {span});
                },
                _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
            },
            _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
        }

        Ok(())
    }

    fn apply_si_prefix_system(&mut self, unit_name: &UnitName<'a>, span: Span<'a>, tokens: &[Token<'a>]) -> Result<(), DeclError<'a>> {
        let args = match tokens.get(0) {
            None => PrefixSystemArgs::default(),
            // If we have more tokens after the prefix system name, there must be a comma followed
            // by at least one other thing (i.e. trailing comma is not allowed here)
            Some(Token::Comma) if tokens.len() > 1 => PrefixSystemArgs::from_tokens(&tokens[1..], span)?,
            _ => return Err(DeclError::PrefixSystemSyntaxError {span}),
        };
        let prefixes = [
            ("Y", "yotta", BigDecimal::from_str("1e24").unwrap()),
            ("Z", "zetta", BigDecimal::from_str("1e21").unwrap()),
            ("E", "exa", BigDecimal::from_str("1e18").unwrap()),
            ("P", "peta", BigDecimal::from_str("1e15").unwrap()),
            ("T", "tera", BigDecimal::from_str("1e12").unwrap()),
            ("G", "giga", BigDecimal::from_str("1e9").unwrap()),
            ("M", "mega", BigDecimal::from_str("1e6").unwrap()),
            ("k", "kilo", BigDecimal::from_str("1e3").unwrap()),
            ("h", "hecto", BigDecimal::from_str("1e2").unwrap()),
            ("da", "deca", BigDecimal::from_str("1e1").unwrap()),
            ("d", "deci", BigDecimal::from_str("1e-1").unwrap()),
            ("c", "centi", BigDecimal::from_str("1e-2").unwrap()),
            ("m", "milli", BigDecimal::from_str("1e-3").unwrap()),
            ("u", "micro", BigDecimal::from_str("1e-6").unwrap()),
            // greek letter "mu"
            ("\u{03BC}", "\u{03BC}", BigDecimal::from_str("1e-6").unwrap()),
            ("n", "nano", BigDecimal::from_str("1e-9").unwrap()),
            ("p", "pico", BigDecimal::from_str("1e-12").unwrap()),
            ("f", "femto", BigDecimal::from_str("1e-15").unwrap()),
            ("a", "atto", BigDecimal::from_str("1e-18").unwrap()),
            ("z", "zepto", BigDecimal::from_str("1e-21").unwrap()),
            ("y", "yocto", BigDecimal::from_str("1e-24").unwrap()),
        ];

        let unit_id = self.units.unit_id(unit_name)
            .expect("bug: unit must be declared before prefix system is applied");
        let unit = CanonicalUnit::from(unit_id);
        let unit_name = if args.lowercase {
            unit_name.as_ref().to_lowercase()
        } else { unit_name.as_ref().to_string() };
        for &(short, long, ref factor) in prefixes.iter() {
            let prefix = if args.longprefix { long } else { short };
            let prefix_unit_name = UnitName::from(prefix.to_string() + unit_name.as_ref());
            let prefix_unit_id = self.units.insert_unit(prefix_unit_name.clone(), span)
                .map_err(|_| DeclError::DuplicateUnitDecl {name: prefix_unit_name.clone(), span})?;
            let prefix_unit = CanonicalUnit::from(prefix_unit_id);
            let left = Number {
                value: factor.clone(),
                unit: unit.clone(),
            };
            let right = Number {
                value: BigDecimal::from(1),
                unit: prefix_unit,
            };
            self.units.add_conversion(ConversionRatio {left, right});
        }

        Ok(())
    }

    fn reduce_const_expr(&self, expr: &Expr<'a>) -> Result<Number, ConstExprError<'a>> {
        match expr {
            &Expr::Number(NumericLiteral {ref value, span: _}, ref unit) => {
                let unit = CanonicalUnit::from_unit_expr(unit, &self.units)?;
                Ok(Number {value: value.clone(), unit})
            },
            &Expr::Ident(ref path, span) => {
                if path.len() != 1 { unimplemented!() }
                let name = path.first().unwrap();
                self.symbols.get_const(name).cloned().ok_or_else(|| ConstExprError::UndeclaredName {name, span})
            },
            Expr::ConvertTo(expr, target_unit, span) => {
                let number = self.reduce_const_expr(expr)?;
                let target_unit = CanonicalUnit::from_unit_expr(target_unit, &self.units)?;
                if number.unit.is_unitless() {
                    Ok(Number {
                        value: number.value,
                        unit: target_unit,
                    })
                }
                else {
                    self.convert(number, target_unit)
                        .map_err(|err| ConstExprError::ConversionFailed(err, *span))
                }
            },
            Expr::Add(_, _, span) | Expr::Sub(_, _, span) | Expr::Mul(_, _, span) |
            //TODO: catch divisions by zero when we support division
            //TODO: catch remainder by zero when we support remainder
            Expr::Div(_, _, span) | Expr::Mod(_, _, span) | Expr::Pow(_, _, span) |
            Expr::Call(_, _, span) | Expr::MacroCall(MacroInvoke {span, ..}) |
            Expr::Return(_, span) | Expr::UnitValue(span) => {
                Err(ConstExprError::UnsupportedConstExpr {expr: expr.clone(), span: *span})
            },
            Expr::Block(_) => unimplemented!(),
        }
    }

    pub fn evaluate_expr(&self, expr: &Expr<'a>) -> Result<Number, EvalError<'a>> {
        Ok(match expr {
            &Expr::Number(NumericLiteral {ref value, span: _}, ref unit) => {
                let unit = CanonicalUnit::from_unit_expr(unit, &self.units)?;
                Number {value: value.clone(), unit}
            },
            &Expr::Ident(ref path, span) => {
                if path.len() != 1 { unimplemented!() }
                let name = path.first().unwrap();
                //TODO: Allow getting variables defined with `let` in the interpreter
                self.symbols.get_const(name).cloned().ok_or_else(|| EvalError::UndeclaredName {name, span})?
            },
            Expr::ConvertTo(expr, target_unit, span) => {
                let number = self.evaluate_expr(expr)?;
                let target_unit = CanonicalUnit::from_unit_expr(target_unit, &self.units)?;
                if number.unit.is_unitless() {
                    Number {
                        value: number.value,
                        unit: target_unit,
                    }
                }
                else {
                    self.convert(number, target_unit)
                        .map_err(|err| EvalError::ConversionFailed(err, *span))?
                }
            },
            Expr::Add(lhs, rhs, span) => {
                let lhs = self.evaluate_expr(lhs)?;
                let rhs = self.convert(self.evaluate_expr(rhs)?, lhs.unit.clone())
                    .map_err(|err| EvalError::ConversionFailed(err, *span))?;
                Number {
                    value: lhs.value + rhs.value,
                    unit: lhs.unit,
                }
            },
            Expr::Sub(lhs, rhs, span) => {
                let lhs = self.evaluate_expr(lhs)?;
                let rhs = self.convert(self.evaluate_expr(rhs)?, lhs.unit.clone())
                    .map_err(|err| EvalError::ConversionFailed(err, *span))?;
                Number {
                    value: lhs.value - rhs.value,
                    unit: lhs.unit,
                }
            },
            Expr::Mul(lhs, rhs, _) => {
                let lhs = self.evaluate_expr(lhs)?;
                let rhs = self.evaluate_expr(rhs)?;
                // Attempt to convert if possible, but otherwise just leave it
                let rhs = self.convert(rhs.clone(), lhs.unit.clone()).unwrap_or_else(|_| rhs);
                Number {
                    value: lhs.value * rhs.value,
                    unit: lhs.unit * rhs.unit,
                }
            },
            Expr::Div(lhs, rhs, span) => {
                let lhs = self.evaluate_expr(lhs)?;
                let rhs = self.evaluate_expr(rhs)?;
                // Attempt to convert if possible, but otherwise just leave it
                let rhs = self.convert(rhs.clone(), lhs.unit.clone()).unwrap_or_else(|_| rhs);
                if rhs.value == BigDecimal::from(0) {
                    return Err(EvalError::DivideByZero {span: *span});
                }

                Number {
                    value: lhs.value / rhs.value,
                    unit: lhs.unit / rhs.unit,
                }
            },
            Expr::Mod(..) => {
                //TODO: Figure out how units are going to work for remainder operator
                //TODO: catch remainder by zero when we support remainder (EvalError::ModuloByZero)
                unimplemented!();
            },
            Expr::Pow(lhs, rhs, span) => {
                let lhs = self.evaluate_expr(lhs)?;
                // Since all units must be known statically at compile time, the rhs must be
                // a constexpr that evaluates to a unitless integer
                let rhs = self.reduce_const_expr(rhs)?;
                if !rhs.unit.is_unitless() {
                    return Err(EvalError::ExponentMustBeUnitless {found: rhs.unit, span: *span});
                }

                let exponent = rhs.value.to_i64()
                    .ok_or_else(|| EvalError::UnsupportedExponent {value: rhs.clone(), span: *span})?;
                if BigDecimal::from(exponent) != rhs.value {
                    return Err(EvalError::UnsupportedExponent {value: rhs, span: *span});
                }

                match exponent {
                    // Anything to the power of zero is 1 '_
                    0 => Number {
                        value: BigDecimal::from(1),
                        unit: CanonicalUnit::unitless(),
                    },
                    // Anything to the power of 1 is itself
                    1 => lhs,
                    2 => Number {
                        value: lhs.value.square(),
                        unit: lhs.unit^2,
                    },
                    3 => Number {
                        value: lhs.value.cube(),
                        unit: lhs.unit^3,
                    },
                    // BigDecimal doesn't support pow, so we're calculating it imprecisely until
                    // supported: https://github.com/akubera/bigdecimal-rs/issues/45
                    _ => Number {
                        value: BigDecimal::from(
                            lhs.value.to_f64().expect("cannot represent with f64").powi(exponent as i32)
                        ),
                        unit: lhs.unit^exponent,
                    },
                }
            },
            _ => unimplemented!(),
        })
    }

    /// Attempts to convert the given value to the given unit
    ///
    /// Does not automatically promote unitless to the target unit as that is not valid in all cases
    fn convert(&self, number: Number, target_unit: CanonicalUnit) -> Result<Number, ConversionFailed> {
        if number.unit == target_unit {
            // No conversion necessary
            Ok(number)
        }
        else {
            let path = self.units.conversion_path(&number.unit, &target_unit)
                .ok_or_else(|| ConversionFailed {start: number.unit.clone(), end: target_unit.clone()})?;
            let factor = path.conversion_factor();
            Ok(Number {
                value: number.value * factor,
                unit: target_unit,
            })
        }
    }
}
