use ast::*;
use unit_graph::UnitGraph;
use symbols::SymbolTable;

pub enum DeclError<'a> {
    // Eventually we will probably want this to be DeclTypeConflict for #[unit] #[converter]
    // and DuplicateAttribute for #[unit] #[unit] or #[converter] #[converter]
    TooManyAttributes {
        function_name: Ident<'a>,
        span: Span<'a>,
    },
    InvalidAttribute {
        function_name: Ident<'a>,
        name: Ident<'a>,
        span: Span<'a>,
    },
    InvalidUnitDecl {
        span: Span<'a>,
    }
}

#[derive(Debug, Clone, Default)]
pub struct Context<'a> {
    units: UnitGraph<'a>,
    symbols: SymbolTable<'a>,
}

impl<'a> Context<'a> {
    pub fn walk_decls<'b>(&mut self, program: &Program<'b>) -> Result<(), DeclError<'b>> {
        for decl in &program.decls {
            match *decl {
                Decl::MacroInvoke(_) => unimplemented!(),
                Decl::Function(ref func) => self.walk_fn_decl(func)?,
                Decl::Constant(_) => unimplemented!(),
                Decl::UnitDecl(_) => unimplemented!(),
                Decl::ConversionDecl(_) => unimplemented!(),
            }
        }
        Ok(())
    }

    fn walk_fn_decl<'b>(&mut self, &Function {ref attrs, ref name, ref args, ref ret, ref body, span}: &Function<'b>) -> Result<(), DeclError<'b>> {
        // Figure out if the function is a unit, a converter, or neither
        match attrs.len() {
            0 => unimplemented!("insert function into symbol table!"),
            1 => {
                let attr = &attrs[0];
                match attr.name {
                    // Make sure tokens is empty
                    //TODO: Support prefix system declarations
                    "unit" if attr.tokens.is_empty() => {
                        ret.as_ref().ok_or_else(|| DeclError::InvalidUnitDecl {span}).and_then(|ret| {
                            self.insert_unit_decl(args, ret, body, span)
                        })
                    },
                    "converter" => unimplemented!(),
                    _ => Err(DeclError::InvalidAttribute {function_name: name, name: attr.name, span}),
                }
            }
            _ => Err(DeclError::TooManyAttributes {function_name: name, span}),
        }
    }

    fn insert_unit_decl<'b>(&mut self, args: &FnArgs<'b>, ret: &UnitExpr<'b>, body: &Block<'b>, span: Span<'b>) -> Result<(), DeclError<'b>> {
        //TODO: insert the unit into the unit graph and add the single conversion from unitless to
        //the unit
        unimplemented!();
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

        let mut context = Context::default();
        context.walk_decls(&program).unwrap_err();
    }
}
