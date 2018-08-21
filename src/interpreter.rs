use std::borrow::Cow;

use ast::*;
use unit_graph::UnitGraph;
use symbols::SymbolTable;

#[derive(Debug, Clone)]
pub enum DeclError<'a> {
    UndeclaredUnit {
        name: Ident<'a>,
        span: Span<'a>,
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}

impl<'a> Interpreter<'a> {
    /// Load declarations into the global scope of the interpreter
    pub fn load_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
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
    fn expand_macros<'b, 'c>(&self, program: Cow<'c, Program<'b>>) -> Result<Cow<'c, Program<'b>>, DeclError<'b>> {
        // By using Cow here, we avoid an unnecessary clone if the program has no macros

        for decl in &program.decls {
            match *decl {
                //TODO: Macro declarations will be processed here eventually

                Decl::MacroInvoke(_) => unimplemented!(),
                _ => {},
            }
        }
        Ok(program)
    }

    /// Load declared units into the global unit graph of the interpreter
    fn load_unit_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
        for decl in &program.decls {
            match *decl {
                //TODO: insert the unit into the unit graph and add the single conversion from
                //unitless to the unit
                Decl::UnitDecl(_) => unimplemented!(),
                _ => {},
            }
        }
        Ok(())
    }

    /// Load declared constants and `const fn`s into the global scope of the interpreter
    fn load_const_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
        for decl in &program.decls {
            match *decl {
                //TODO: const fn declarations will be processed and unit checked here eventually

                Decl::Constant(_) => unimplemented!(),
                _ => {},
            }
        }
        Ok(())
    }

    /// Load unit conversions and evaluate any const exprs found within them
    fn load_conversion_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
        for decl in &program.decls {
            match *decl {
                Decl::ConversionDecl(_) => unimplemented!(),
                _ => {},
            }
        }
        Ok(())
    }

    /// Load function declarations into the global scope and check their bodies to ensure that any
    /// conversions are valid
    fn load_fn_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
        for decl in &program.decls {
            match *decl {
                Decl::Function(_) => unimplemented!(),
                _ => {},
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_program;

    #[test]
    #[ignore] //TODO: Unignore once implemented
    fn multitype_function() {
        let input = r#"
        #[unit]
        #[converter]
        fn(x '_) -> 'deg { x }
        "#;
        let program = parse_program(input).unwrap();

        let mut inter = Interpreter::default();
        inter.load_decls(&program).unwrap_err();
    }
}
