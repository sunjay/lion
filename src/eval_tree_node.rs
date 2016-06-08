use ast::{Expr, ExprItem, Term};
use rich_number::RichNumber;
use eval_context::{EvalContext, EvalError, ContextItem};

#[derive(Debug, Clone)]
pub struct EvalTreeNode {
    item: ContextItem,
    children: Vec<EvalTreeNode>,
}

impl EvalTreeNode {
    pub fn new(item: ContextItem) -> EvalTreeNode {
        EvalTreeNode {
            item: item,
            children: Vec::new(),
        }
    }

    pub fn from_expr(context: &EvalContext, expr: Expr) -> Result<EvalTreeNode, EvalError> {
        let nodes = try!(EvalTreeNode::convert_to_nodes(context, expr));

        unimplemented!();
    }

    fn convert_to_nodes(context: &EvalContext, expr: Expr) -> Result<Vec<EvalTreeNode>, EvalError> {
        let mut nodes: Vec<EvalTreeNode> = Vec::new();

        for item in expr.into_iter() {
            nodes.push(match item {
                ExprItem::AnonymousFunction(function) => {
                    EvalTreeNode::new(ContextItem::function_defaults(function))
                },
                ExprItem::SingleTerm(term) => match term {
                    Term::Symbol(sym) => {
                        let item = context.get(&sym);
                        if item.is_none() {
                            return Err(EvalError::UnknownSymbol(sym));
                        }
                        EvalTreeNode::new(item.unwrap())
                    },
                    Term::Number(num) => {
                        EvalTreeNode::new(
                            ContextItem::Number(
                                RichNumber::from(num)
                            )
                        )
                    },
                    Term::StringLiteral(value) => {
                        EvalTreeNode::new(
                            ContextItem::Constant(value)
                        )
                    },
                },
                ExprItem::Group(expr) => try!(EvalTreeNode::from_expr(context, expr)),
            });
        }

        Ok(nodes)
    }
}

