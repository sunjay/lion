use parser::ast::{Function};
use math::rich_number::RichNumber;

use eval::fixity::Fixity;
use eval::built_in_function::BuiltInFunction;

const LOWEST_PRECEDENCE: u8 = 0;
const HIGHEST_PRECEDENCE: u8 = 9;
const FUNCTION_PRECEDENCE: u8 = HIGHEST_PRECEDENCE;

const FUNCTION_FIXITY: Fixity = Fixity::Prefix;

#[derive(PartialEq, Debug, Clone)]
pub enum ContextItem {
    Number(RichNumber),
    Definition {
        // number from 0 to 9 where 9 is the highest precedence
        precedence: u8,
        fixity: Fixity,
        function: Function,
    },
    BuiltInMethod {
        precedence: u8,
        fixity: Fixity,
        params: usize,
        function: BuiltInFunction,
    },
    Constant(String),
    Boolean(bool),
    Nothing,
}

impl ContextItem {
    /// Returns a new context item for a defintion of a function
    pub fn new_definition(
        fixity: Fixity,
        precedence: u8,
        function: Function,
    ) -> ContextItem {
        debug_assert!(precedence >= LOWEST_PRECEDENCE && precedence <= HIGHEST_PRECEDENCE);

        ContextItem::Definition {
            fixity: fixity,
            precedence: precedence,
            function: function,
        }
    }

    /// Creates a Definition ContextItem from a function
    /// with the defaults assumed for all functions
    pub fn function_defaults(function: Function) -> ContextItem {
        ContextItem::new_definition(
            FUNCTION_FIXITY,
            FUNCTION_PRECEDENCE,
            function,
        )
    }

    /// Creates a BuiltInMethod ContextItem from a function
    /// with the defaults assumed for all functions
    pub fn built_in_defaults(function: BuiltInFunction, params: usize) -> ContextItem {
        ContextItem::BuiltInMethod {
            precedence: FUNCTION_PRECEDENCE,
            fixity: FUNCTION_FIXITY,
            params: params,
            function: function,
        }
    }

    pub fn unwrap_number(self) -> RichNumber {
        match self {
            ContextItem::Number(num) => num,
            _ => panic!("Expected to unwrap a Number"),
        }
    }

    pub fn unwrap_constant(self) -> String {
        match self {
            ContextItem::Constant(value) => value,
            _ => panic!("Expected to unwrap a Constant"),
        }
    }

    pub fn unwrap_boolean(self) -> bool {
        match self {
            ContextItem::Boolean(value) => value,
            _ => panic!("Expected to unwrap a Boolean"),
        }
    }

    pub fn is_number(&self) -> bool {
        match *self {
            ContextItem::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        self.is_definition() || self.is_built_in()
    }

    pub fn is_definition(&self) -> bool {
        match *self {
            ContextItem::Definition { .. } => true,
            _ => false,
        }
    }

    pub fn is_built_in(&self) -> bool {
        match *self {
            ContextItem::BuiltInMethod { .. } => true,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        match *self {
            ContextItem::Constant(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match *self {
            ContextItem::Constant(_) => true,
            _ => false,
        }
    }

    pub fn is_nothing(&self) -> bool {
        match *self {
            ContextItem::Nothing => true,
            _ => false,
        }
    }

    /// The most reliable way to get the precedence of any ContextItem
    /// ContextItems with no defined precedence return None
    pub fn resolve_precedence(&self) -> Option<&u8> {
        match *self {
            ContextItem::Definition { ref precedence, .. } => Some(precedence),
            ContextItem::BuiltInMethod { ref precedence, .. } => Some(precedence),
            _ => None,
        }
    }

    /// The most reliable way to get the fixity of any ContextItem
    /// ContextItems with no defined fixity return None
    pub fn resolve_fixity(&self) -> Option<Fixity> {
        match *self {
            ContextItem::Definition { fixity, .. } => Some(fixity),
            ContextItem::BuiltInMethod { fixity, .. } => Some(fixity),
            _ => None,
        }
    }

    /// The most reliable way to get the number of parameters of any ContextItem
    /// ContextItems with no defined number of parameters return None
    pub fn resolve_params(&self) -> Option<usize> {
        match *self {
            ContextItem::Definition {
                function: Function {
                    ref params,
                    ..
                },
                ..
            } => Some(params.len()),
            ContextItem::BuiltInMethod { ref params, .. } => Some(*params),
            _ => None,
        }
    }
}

