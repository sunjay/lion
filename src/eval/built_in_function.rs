use std::rc::Rc;
use std::fmt;

use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalResult};

#[derive(Clone)]
pub struct BuiltInFunction {
    f: Rc<Fn(&mut EvalContext, Vec<ContextItem>) -> EvalResult>,
}

impl BuiltInFunction {
    pub fn new<F: 'static>(f: F) -> BuiltInFunction
        where F: Fn(&mut EvalContext, Vec<ContextItem>) -> EvalResult {

        BuiltInFunction {
            f: Rc::new(f),
        }
    }

    pub fn call(&self, context: &mut EvalContext, params: Vec<ContextItem>) -> EvalResult {
        (*self.f)(context, params)
    }
}

impl fmt::Debug for BuiltInFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<built-in function>")
    }
}

impl PartialEq for BuiltInFunction {
    fn eq(&self, other: &Self) -> bool {
        // From #rust-beginners IRC user Yurume:
        // check the referential equality: when f1 and f2 are Rc<T>, they point to the same thing when `&*f1 as *const _ == &*f2 as *const _`
        &*(self.f) as *const _ == &*(other.f) as *const _
    }
}

