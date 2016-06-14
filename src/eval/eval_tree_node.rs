use ast::{Expr, ExprItem, Term};
use math::rich_number::RichNumber;
use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalError};

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
        let mut nodes = try!(EvalTreeNode::convert_to_nodes(context, expr));

        while nodes.len() != 1 {
            let node_index = EvalTreeNode::left_highest_precedence(&nodes);
            if node_index.is_none() {
                return Err(EvalError::UnexpectedSymbols);
            }
            let node_index = node_index.unwrap();

            let params = try!(EvalTreeNode::drain_params(&mut nodes, node_index));

            let mut node = &mut nodes[node_index];
            node.children = params;
        }

        debug_assert!(nodes.len() == 1);
        Ok(nodes.pop().unwrap())
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
                            return Err(EvalError::UndefinedSymbol(sym));
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

    /// Returns the index of the leftmost node with the highest precedence
    /// Only returns ContextItems with precedence
    fn left_highest_precedence(nodes: &Vec<EvalTreeNode>) -> Option<usize> {
        // (index, precedence)
        let mut highest: Option<(usize, u8)> = None;

        for (i, node) in nodes.iter().enumerate() {
            // This node has already been taken care of
            if !node.children.is_empty() {
                continue;
            }

            let precedence = node.item.resolve_precedence();
            if precedence.is_none() {
                continue;
            }
            let precedence = *precedence.unwrap();

            // This MUST be less than, NOT less than or equal to to get the
            // leftmost item
            if highest.is_none() || highest.unwrap().1 < precedence {
                highest = Some((i, precedence));
            }
        }

        highest.map(|x| x.0)
    }

    // Removes the nodes adjacent to the node_index (but not including the
    // node_index) that correspond to the number of parameters expected
    // by that node in the direction specified by that node's fixity
    fn drain_params(mut nodes: &mut Vec<EvalTreeNode>, node_index: usize) -> Result<Vec<EvalTreeNode>, EvalError> {
        let fixity;
        let params;
        {
            let node = &nodes[node_index];

            // unwrap() is safe here because node item should be a definition or built in
            fixity = node.item.resolve_fixity().unwrap();
            params = node.item.resolve_params().unwrap();
            debug_assert!(params != 0);
        }
        
        Ok(match fixity {
            Fixity::Infix => {
                // Infix functions MUST have 2 parameters
                if params != 2 {
                    return Err(EvalError::InvalidSymbolDefinition {
                        expected_params: 2,
                        actual_params: params,
                    });
                }

                // It is vital that the second remove happen before the
                // first because this operation shifts all indexes to the left
                let second = nodes.remove(node_index + 1);
                let first = nodes.remove(node_index - 1);

                vec![first, second]
            },
            Fixity::Prefix => {
                let start = node_index - params;
                let end = node_index;
                if start < 0 || end >= nodes.len() {
                    return Err(EvalError::ExpectedParams(params));
                }
                nodes.drain(start..end).collect()
            },
            Fixity::Postfix => {
                // The +1 in the end of the range is because ranges stop
                // at the index 1 before the end
                let start = node_index + 1;
                let end = node_index + params + 1;
                if start < 0 || end >= nodes.len() {
                    return Err(EvalError::ExpectedParams(params));
                }
                nodes.drain(start..end).collect()
            },
        })
    }
}

