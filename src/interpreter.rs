use std::borrow::Cow;

use rust_decimal::Decimal;

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
}

impl<'a> From<canonical::UndeclaredUnit<'a>> for DeclError<'a> {
    fn from(canonical::UndeclaredUnit {name, span}: canonical::UndeclaredUnit<'a>) -> Self {
        DeclError::UndeclaredUnit {name, span}
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}

impl<'a> Interpreter<'a> {
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
            if let &Decl::UnitDecl(UnitDecl {ref attrs, unit_name, span, ..}) = decl {
                //TODO: insert the unit into the unit graph and add the single conversion from
                //unitless to the unit

                for &Attribute {name, tokens: _, span} in attrs {
                    match name {
                        "prefix" => {}, //TODO
                        _ => return Err(DeclError::UnknownAttribute {name, span}),
                    }
                }

                self.units.insert_unit(unit_name, span)
                    .map_err(|_| DeclError::DuplicateUnitDecl {name: unit_name, span})?;
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
                            *value
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
                    let unit_id = self.units.unit_id(*unit_name)
                        .expect("bug: unit should have already been declared");
                    let left_unit = CanonicalUnit::from(unit_id);
                    let right_unit = CanonicalUnit::from_unit_expr(alias, &self.units)?;

                    // Alias means: 1 left_unit == 1 right_unit
                    self.units.add_conversion(ConversionRatio {
                        left: Number {
                            value: Decimal::from(1),
                            unit: left_unit,
                        },
                        right: Number {
                            value: Decimal::from(1),
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

    fn reduce_const_expr(&self, expr: &Expr<'a>) -> Result<Number, DeclError<'a>> {
        match expr {
            &Expr::Number(NumericLiteral {value, span: _}, ref unit) => {
                let unit = CanonicalUnit::from_unit_expr(unit, &self.units)?;
                Ok(Number {value, unit})
            },
            &Expr::Ident(ref path, span) => {
                if path.len() != 1 { unimplemented!() }
                let name = path.first().unwrap();
                self.symbols.get_const(name).cloned().ok_or_else(|| DeclError::UndeclaredName {name, span})
            },
            Expr::ConvertTo(expr, target_unit, _) => {
                let number = self.reduce_const_expr(expr)?;
                let target_unit = CanonicalUnit::from_unit_expr(target_unit, &self.units)?;
                if number.unit == target_unit {
                    // No conversion necessary
                    Ok(number)
                }
                // Can convert from unitless to any unit
                else if number.unit.is_unitless() {
                    Ok(Number {
                        value: number.value,
                        unit: target_unit,
                    })
                }
                else {
                    //TODO: Convert
                    unimplemented!();
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
}
