use std::borrow::Cow;
use std::str::FromStr;

use bigdecimal::BigDecimal;

use ast::*;
use ir::{Number, ConversionRatio};
use unit_graph::UnitGraph;
use symbols::SymbolTable;
use canonical::{self, CanonicalUnit};

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
    UnknownPrefixSystem {
        name: Ident<'a>,
        span: Span<'a>,
    },
    DuplicateConst {
        name: Ident<'a>,
        span: Span<'a>,
    },
    UnsupportedConstExpr {
        expr: Expr<'a>,
        span: Span<'a>,
    },
    MismatchedUnit {
        expected: UnitExpr<'a>,
        found: UnitExpr<'a>,
        span: Span<'a>,
    },
    ConversionFailed(ConversionFailed, Span<'a>),
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for DeclError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        DeclError::UndeclaredUnit {name, span}
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
    MismatchedUnit {
        expected: UnitExpr<'a>,
        found: UnitExpr<'a>,
        span: Span<'a>,
    },
    ConversionFailed(ConversionFailed, Span<'a>),
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for EvalError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        EvalError::UndeclaredUnit {name, span}
    }
}

#[derive(Debug, Clone)]
pub struct ConversionFailed {
    start: CanonicalUnit,
    end: CanonicalUnit,
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn print_unit(&self, unit: &CanonicalUnit) {
        let mut names = unit.iter_unit_names(&self.units);

        let print_unit = |(unit_name, exp)| {
            if exp != 1 {
                print!("{}^{}", unit_name, exp);
            }
            else {
                print!("{}", unit_name);
            }
        };
        if let Some(pair) = names.next() {
            print_unit(pair);
        }
        for pair in names {
            print!(" ");
            print_unit(pair);
        }
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
                let unit = CanonicalUnit::from_unit_expr(unit_expr, &self.units)?;
                let value = match value {
                    &Expr::Number(NumericLiteral {ref value, span: const_span}, ref const_unit) => {
                        // Units must match
                        if CanonicalUnit::from_unit_expr(const_unit, &self.units)? == unit {
                            value.clone()
                        }
                        else {
                            return Err(DeclError::MismatchedUnit {
                                expected: unit_expr.clone(),
                                found: const_unit.clone(),
                                span: const_span,
                            })
                        }
                    },
                    _ => return Err(DeclError::UnsupportedConstExpr {expr: value.clone(), span}),
                };
                self.symbols.insert_const(name, value, unit, span).map_err(|_| {
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
        let prefixes = [
            ("Y", BigDecimal::from_str("1e24").unwrap()),
            ("Z", BigDecimal::from_str("1e21").unwrap()),
            ("E", BigDecimal::from_str("1e18").unwrap()),
            ("P", BigDecimal::from_str("1e15").unwrap()),
            ("T", BigDecimal::from_str("1e12").unwrap()),
            ("G", BigDecimal::from_str("1e9").unwrap()),
            ("M", BigDecimal::from_str("1e6").unwrap()),
            ("k", BigDecimal::from_str("1e3").unwrap()),
            ("h", BigDecimal::from_str("1e2").unwrap()),
            ("da", BigDecimal::from_str("1e1").unwrap()),
            ("d", BigDecimal::from_str("1e-1").unwrap()),
            ("c", BigDecimal::from_str("1e-2").unwrap()),
            ("m", BigDecimal::from_str("1e-3").unwrap()),
            ("u", BigDecimal::from_str("1e-6").unwrap()),
            ("n", BigDecimal::from_str("1e-9").unwrap()),
            ("p", BigDecimal::from_str("1e-12").unwrap()),
            ("f", BigDecimal::from_str("1e-15").unwrap()),
            ("a", BigDecimal::from_str("1e-18").unwrap()),
            ("z", BigDecimal::from_str("1e-21").unwrap()),
            ("y", BigDecimal::from_str("1e-24").unwrap()),
        ];

        //TODO: Process tokens for more arguments

        let unit_id = self.units.unit_id(unit_name)
            .expect("bug: unit must be declared before prefix system is applied");
        let unit = CanonicalUnit::from(unit_id);
        for &(prefix, ref factor) in prefixes.iter() {
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

    fn reduce_const_expr(&self, expr: &Expr<'a>) -> Result<Number, DeclError<'a>> {
        match expr {
            &Expr::Number(NumericLiteral {ref value, span: _}, ref unit) => {
                let unit = CanonicalUnit::from_unit_expr(unit, &self.units)?;
                Ok(Number {value: value.clone(), unit})
            },
            &Expr::Ident(ref path, span) => {
                if path.len() != 1 { unimplemented!() }
                let name = path.first().unwrap();
                self.symbols.get_const(name).cloned().ok_or_else(|| DeclError::UndeclaredName {name, span})
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
                        .map_err(|err| DeclError::ConversionFailed(err, *span))
                }
            },
            Expr::Add(_, _, span) | Expr::Sub(_, _, span) | Expr::Mul(_, _, span) |
            Expr::Div(_, _, span) | Expr::Mod(_, _, span) | Expr::Pow(_, _, span) |
            Expr::Call(_, _, span) | Expr::MacroCall(MacroInvoke {span, ..}) |
            Expr::Return(_, span) | Expr::UnitValue(span) => {
                Err(DeclError::UnsupportedConstExpr {expr: expr.clone(), span: *span})
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
            Expr::Div(lhs, rhs, _) => {
                let lhs = self.evaluate_expr(lhs)?;
                let rhs = self.evaluate_expr(rhs)?;
                // Attempt to convert if possible, but otherwise just leave it
                let rhs = self.convert(rhs.clone(), lhs.unit.clone()).unwrap_or_else(|_| rhs);
                Number {
                    value: lhs.value / rhs.value,
                    unit: lhs.unit / rhs.unit,
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
