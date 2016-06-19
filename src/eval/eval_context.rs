use std::collections::HashMap;

use parser::ast::{Function, Expr, Statement};
use math::rich_number::{Unit, RichNumber};
use math::conversion_table::ConversionTable;

use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_tree_node::EvalTreeNode;

use eval::defaults as default_functions;

use prelude::setup_prelude;

#[derive(PartialEq, Debug)]
pub enum EvalError {
    UndefinedSymbol(String),
    UnexpectedSymbols,
    ExpectedParams(usize),
    InvalidSymbolDefinition {
        expected_params: usize,
        actual_params: usize,
    },
    ConversionUndefined {
        start: Unit,
        target: Unit,
    },
    InvalidParams(String),
}

pub type EvalResult = Result<ContextItem, EvalError>;

pub struct EvalContext {
    // Symbol name to context item definition
    symbol_table: HashMap<String, ContextItem>,
    // Unit to Symbol name
    units: HashMap<Unit, String>,
    // Used to define new units
    next_unit: Unit,
    conversion_table: ConversionTable,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            symbol_table: HashMap::new(),
            units: HashMap::new(),
            next_unit: 1,
            conversion_table: ConversionTable::new(),
        }
    }

    /// Defines a reasonble set of default built_in methods
    pub fn defaults() -> EvalContext {
        let mut context = EvalContext::new();

        context.set_constant("PREFIX", Fixity::Prefix.to_string());
        context.set_constant("INFIX", Fixity::Infix.to_string());
        context.set_constant("POSTFIX", Fixity::Postfix.to_string());

        context.define_built_in_method_defaults(
            "operator",
            4,
            BuiltInFunction::new(default_functions::operator),
        );

        context.define_built_in_method_defaults(
            "defineUnit",
            1,
            BuiltInFunction::new(default_functions::define_unit),
        );

        context.define_built_in_method_defaults(
            "convert",
            2,
            BuiltInFunction::new(default_functions::convert),
        );

        context.define_built_in_method_defaults(
            "unitFor",
            1,
            BuiltInFunction::new(default_functions::unit_for),
        );

        context.define_built_in_method_defaults(
            "valueOf",
            1,
            BuiltInFunction::new(default_functions::value_of),
        );

        context.define_built_in_method_defaults(
            "conversion",
            3,
            BuiltInFunction::new(default_functions::conversion),
        );

        context
    }

    /// Defines a context with all the defaults as well as definitions
    /// for common operators and units
    pub fn prelude() -> EvalContext {
        let mut context = EvalContext::defaults();
        setup_prelude(&mut context);
        context
    }

    /// Gets a single value without evaluating it from the current context
    pub fn get(&self, name: &str) -> Option<ContextItem> {
        self.symbol_table.get(name).map(|item| (*item).clone())
    }

    /// Convenience method for setting a number in the context
    pub fn set_number(&mut self, name: &str, value: RichNumber) {
        self.set(name, ContextItem::Number(value))
    }

    /// Defines a function in the context
    pub fn define(
        &mut self,
        name: &str,
        fixity: Fixity,
        precedence: u8,
        function: Function,
    ) {
        self.set(name, ContextItem::new_definition(fixity, precedence, function));
    }

    /// Defines a function in the context with defaults for precedence and fixity
    pub fn define_defaults(
        &mut self,
        name: &str,
        function: Function,
    ) {
        self.set(name, ContextItem::function_defaults(function));
    }

    /// Defines a function linked to actual code rather than an expression
    pub fn define_built_in_method(
        &mut self,
        name: &str,
        fixity: Fixity,
        precedence: u8,
        params: usize,
        function: BuiltInFunction,
    ) {

        self.set(name, ContextItem::BuiltInMethod {
            fixity: fixity,
            precedence: precedence,
            params: params,
            function: function,
        });
    }

    /// Defines a built-in function in the context with defaults for precedence and fixity
    pub fn define_built_in_method_defaults(
        &mut self,
        name: &str,
        params: usize,
        function: BuiltInFunction,
    ) {
        self.set(name, ContextItem::built_in_defaults(function, params));
    }

    /// Creates a unique identifier for the given unit name
    /// Calling this more than once for the same unit name will end up
    /// with some duplicate definitons, should not be a huge deal but may
    /// lead to problems later on
    pub fn create_unit(&mut self, name: &str) -> Unit {
        let unit = self.next_unit;
        self.next_unit += 1;

        self.units.insert(unit, name.to_owned());

        unit
    }

    /// Creates a constant value
    pub fn set_constant(&mut self, name: &str, value: String) {
        self.set(name, ContextItem::Constant(value));
    }

    /// Creates a boolean value
    pub fn set_boolean(&mut self, name: &str, value: bool) {
        self.set(name, ContextItem::Boolean(value));
    }

    /// Adds to the current context (silently replaces if already present)
    pub fn set(&mut self, name: &str, value: ContextItem) {
        self.symbol_table.insert(name.to_owned(), value);
    }

    /// Converts the value to the given unit by doing appropriate
    /// transformations as prescribed by the conversion table
    /// Returns Err if no appropriate conversion is found
    pub fn convert(&mut self, value: RichNumber, unit: Option<Unit>) -> EvalResult {
        if value.unit.is_none() || unit.is_none() {
            Ok(ContextItem::Number(RichNumber::new(value.value, unit)))
        }
        else {
            let start = value.unit.unwrap();
            let unit = unit.unwrap();

            let conversions = self.conversion_table.conversion_steps(start, unit);

            if conversions.is_none() {
                Err(EvalError::ConversionUndefined {
                    start: start,
                    target: unit,
                })
            }
            else {
                let conversions = conversions.unwrap();

                let mut current = ContextItem::Number(value);
                for next_unit in conversions {
                    //TODO: unwrap_number() here may not be completely safe
                    //TODO: because it is dependent on the result of applying the converter
                    let current_number = current.unwrap_number();

                    // unwrap() is safe here because we used conversion_steps
                    let converter = self.conversion_table.get_converter(
                        current_number.unit.unwrap(),
                        next_unit
                    ).unwrap();

                    current = try!(self.apply_function(&converter, vec![
                        ContextItem::Number(current_number.without_units()),
                    ]));
                }

                debug_assert!({
                    let c = current.clone().unwrap_number().unit;
                    !c.is_none() && c.unwrap() == unit
                });

                Ok(current)
            }
        }
    }

    pub fn apply(&mut self, statement: Statement) -> EvalResult {
        match statement {
            Statement::NamedFunction {name, definition} => {
                self.define_defaults(&name, definition);
                Ok(ContextItem::Nothing)
            },
            Statement::AnonymousFunction(_) => Ok(ContextItem::Nothing),
            Statement::Assignment {name, value} => {
                let value = try!(self.evaluate(value));
                self.set(&name, value);
                Ok(ContextItem::Nothing)
            },
            Statement::Expression(value) => self.evaluate(value),
        }
    }

    fn evaluate(&mut self, expr: Expr) -> EvalResult {
        let root = try!(EvalTreeNode::from_expr(self, expr));

        self.reduce(root)
    }

    fn reduce(&mut self, node: EvalTreeNode) -> EvalResult {
        let EvalTreeNode {
            item,
            children,
        } = node;

        let mut params = Vec::new();
        for child in children {
            params.push(try!(self.reduce(child)));
        }

        match item {
            ContextItem::BuiltInMethod { ref function, params: ref pn, .. } => {
                debug_assert!(*pn != 0,
                    "Functions cannot have zero parameters");

                // Special case: when no parameters are provided, return
                // the function itself.
                // Added to support (f) and anonymous syntax
                if params.len() == 0 {
                    Ok(item.clone())
                }
                else {
                    function.call(self, params)
                }
            },
            ContextItem::Definition { ref function, .. } => {
                if params.len() == 0 {
                    Ok(item.clone())
                }
                else {
                    self.apply_function(function, params)
                }
            },
            otherwise => Ok(otherwise),
        }
    }

    /// Applies the given parameters to the given function
    fn apply_function(&mut self, function: &Function, params: Vec<ContextItem>) -> EvalResult {
        let param_names = &function.params;
        if params.len() != param_names.len() {
            return Err(EvalError::ExpectedParams(param_names.len()));
        }

        let name_mapping = param_names.iter()
            .zip(params.iter())
            .collect::<Vec<_>>();
            //.collect::<HashMap<String, ContextItem>>();
        println!("{:?}", name_mapping);

        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use api::parse;

    use ast::*;
    use math::rich_number::RichNumber;

    #[test]
    fn get_set_in_context() {
        let mut context = EvalContext::new();
        assert_eq!(context.get("a"), None);

        let value = RichNumber::from(2f64);
        context.set_number("a", value.clone());
        assert_eq!(context.get("a").map(|n| n.unwrap_number()), Some(value));
    }

    #[test]
    fn define_variable() {
        let mut context = EvalContext::new();
        apply_string(&mut context, "x = 3");

        assert_eq!(context.get("x").map(|n| n.unwrap_number()), Some(RichNumber::from(3f64)));
    }

    #[test]
    fn basic_operators_and_precedence() {
        let result = apply_single("2 * 3 ** 2 - (100e-1 + -77.2) / 13").unwrap();
        assert_eq!(result.unwrap_number(), RichNumber::from(23.1692307692f64));
    }

    fn test_single_boolean(string: &str, expected: bool) {
        let result = apply_single(string).unwrap();
        assert_eq!(result.unwrap_boolean(), expected);
    }

    fn apply_single(string: &str) -> EvalResult {
        let mut context = EvalContext::prelude();
        let statement = parse_statement(string);

        context.apply(statement)
    }

    fn parse_statement(string: &str) -> Statement {
        let mut parsed = parse(string).unwrap();
        assert!(parsed.len() == 1, "parse_statement() was passed more than one statement");

        parsed.pop().unwrap()
    }

    fn apply_string(context: &mut EvalContext, string: &str) {
        apply_program(context, parse(string).unwrap());
    }

    fn apply_program(context: &mut EvalContext, program: Program) {
        for statement in program {
            context.apply(statement).expect("Error while applying statement");
        }
    }
}

