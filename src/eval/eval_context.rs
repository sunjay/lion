use std::collections::HashMap;

use rand::random;

use parser::ast::{Function, Expr, ExprItem, Term, Statement};
use math::rich_number::{Unit, RichNumber};
use math::conversion_table::ConversionTable;

use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::built_in_function::BuiltInFunction;
use eval::eval_tree_node::EvalTreeNode;

use eval::defaults::setup_defaults;

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
    InvalidParam(String),
}

pub type EvalResult = Result<ContextItem, EvalError>;

pub struct EvalContext {
    // Symbol name to context item definition
    symbol_table: HashMap<String, ContextItem>,
    // Unit to Symbol name
    unit_names: HashMap<Unit, String>,
    // Symbol name to Unit
    units: HashMap<String, Unit>,
    // Used to define new units
    next_unit: Unit,
    conversion_table: ConversionTable,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            symbol_table: HashMap::new(),
            unit_names: HashMap::new(),
            units: HashMap::new(),
            next_unit: 1,
            conversion_table: ConversionTable::new(),
        }
    }

    /// Defines a reasonble set of default built_in methods
    pub fn defaults() -> EvalContext {
        let mut context = EvalContext::new();
        setup_defaults(&mut context);
        context
    }

    /// Defines a context with all the defaults as well as definitions
    /// for common operators and units
    pub fn prelude() -> EvalContext {
        let mut context = EvalContext::defaults();
        setup_prelude(&mut context);
        context
    }

    /// Attempts to lookup the name for the given unit value
    pub fn lookup_unit(&self, unit: Unit) -> Option<String> {
        self.unit_names.get(&unit).map(|x| x.clone())
    }

    /// Attempts to lookup the unit for the given unit name
    pub fn lookup_unit_name(&self, name: &str) -> Option<Unit> {
        self.units.get(name).map(|x| x.clone())
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
        assert!(fixity != Fixity::Infix || function.params.len() == 2);
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
        assert!(fixity != Fixity::Infix || params == 2);

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

        self.unit_names.insert(unit, name.to_owned());
        self.units.insert(name.to_owned(), unit);

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

    /// Removes a name from the context, does nothing if name is not there
    pub fn remove(&mut self, name: &str) {
        self.symbol_table.remove(name);
    }

    /// Calls the function with the given name in the context
    /// Panics if the value at this name is not a function
    /// Returns the result or None if the name was not found
    pub fn call(&mut self, name: &str, params: Vec<ContextItem>) -> Option<EvalResult> {
        assert!(!params.is_empty(),
            "Attempt to call function without passing any arguments");

        self.get(name).map(|f| match f {
            ContextItem::BuiltInMethod { ref function, params: ref pn, .. } => {
                debug_assert!(*pn != 0,
                    "Functions cannot be defined with zero parameters");

                function.call(self, params)
            },
            ContextItem::Definition { ref function, .. } => {
                self.apply_function(function, params)
            },
            _ => panic!("Attempt to call non-function '{}'", name),
        })
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

        // Special case: when no parameters are provided, return
        // the function itself.
        // Added to support (f) and anonymous syntax
        if item.is_function() && params.is_empty() {
            return Ok(item.clone())
        }

        match item {
            ContextItem::BuiltInMethod { ref function, params: ref pn, .. } => {
                debug_assert!(!params.is_empty());

                debug_assert!(*pn != 0,
                    "Functions cannot be defined with zero parameters");

                function.call(self, params)
            },
            ContextItem::Definition { ref function, .. } => {
                debug_assert!(!params.is_empty());
                self.apply_function(function, params)
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

        // need to munge parameter names
        // one day this will be replaced with a true stack
        // using characters that couldn't possibly be in any other symbol
        let munge = format!("[[{}]]", random::<u64>());

        let reserved_names = param_names.iter()
            .map(|n| (n.clone(), format!("{}{}", munge, n)))
            .collect::<HashMap<String, String>>();

        let body: Expr = self.munge_expr(&function.body, &reserved_names, &Vec::new());

        // Temporarily add names
        for (i, name) in param_names.iter().enumerate() {
            let reserve = reserved_names.get(name).unwrap();
            self.set(reserve, params[i].clone());
        }

        let result = self.evaluate(body);

        for name in param_names {
            let reserve = reserved_names.get(name).unwrap();
            self.remove(reserve);
        }

        result
    }

    /// munges the given expression using the given reserved names
    /// reserved_names maps names to their munged forms
    /// exceptions was added to allow munging of inner functions without messing up their arguments accidentally
    fn munge_expr(&self, expr: &Expr, reserved_names: &HashMap<String, String>, exceptions: &Vec<String>) -> Expr {
        expr.iter().map(|x| match *x {
            ExprItem::AnonymousFunction(Function { ref params, ref body }) => {
                ExprItem::AnonymousFunction(Function {
                    params: params.clone(),
                    body: self.munge_expr(body, reserved_names, params),
                })
            },
            ExprItem::SingleTerm(Term::Symbol(ref name)) => {
                if reserved_names.contains_key(name) && !exceptions.contains(name) {
                    let new_name = reserved_names.get(name).unwrap().clone();
                    ExprItem::SingleTerm(Term::Symbol(new_name))
                }
                else {
                    x.clone()
                }
            },
            ExprItem::SingleTerm(_) => {
                x.clone()
            }
            ExprItem::Group(ref expr) => {
                ExprItem::Group(self.munge_expr(expr, reserved_names, &Vec::new()))
            },
        }).collect()
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

    #[test]
    fn evaluates_functions_with_nested_groups() {
        let result = apply_single(r"(\a b c = (-1 * b + b ** 2 - 4 * a * c) / (-2 * a)) 1 2 3").unwrap();
        assert_eq!(result.unwrap_number(), RichNumber::from(5f64));
    }

    #[test]
    fn allows_nesting_functions_with_same_parameter_names() {
        let result = apply_single(r"(\x y = (\x = x * y) 3) 1 2").unwrap();
        assert_eq!(result.unwrap_number(), RichNumber::from(6f64));
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

