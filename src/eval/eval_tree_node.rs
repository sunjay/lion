use ast::{Expr, ExprItem, Term};
use math::rich_number::RichNumber;
use eval::fixity::Fixity;
use eval::context_item::ContextItem;
use eval::eval_context::{EvalContext, EvalError};

#[derive(Debug, PartialEq, Clone)]
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

    pub fn with_children(item: ContextItem, children: Vec<EvalTreeNode>) -> EvalTreeNode {
        let mut node = EvalTreeNode::new(item);
        node.children = children;

        node
    }

    pub fn from_expr(context: &EvalContext, expr: Expr) -> Result<EvalTreeNode, EvalError> {
        let mut nodes = try!(EvalTreeNode::convert_to_nodes(context, expr));

        while nodes.len() != 1 {
            let node_index = EvalTreeNode::left_highest_precedence(&nodes);
            if node_index.is_none() {
                return Err(EvalError::UnexpectedSymbols);
            }
            let node_index = node_index.unwrap();

            let (node_index, params) = try!(EvalTreeNode::drain_params(&mut nodes, node_index));

            let node = &mut nodes[node_index];
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
    // Returns the (node_index, parameter nodes)
    // Needs to return node_index because that index may change
    // due to this operation
    fn drain_params(nodes: &mut Vec<EvalTreeNode>, node_index: usize) -> Result<(usize, Vec<EvalTreeNode>), EvalError> {
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

                // if either of these conditions is true, the following calculation
                // would result in either overflow or an index error
                if node_index == 0 || node_index >= nodes.len() {
                    return Err(EvalError::ExpectedParams(params));
                }

                // It is vital that the second remove happen before the
                // first because this operation shifts all indexes to the left
                let second = nodes.remove(node_index + 1);
                let first = nodes.remove(node_index - 1);

                // node_index shifts to where the first argument was
                (node_index - 1, vec![first, second])
            },
            Fixity::Postfix => {
                let start;
                let end = node_index;

                // params > node_index would result in a negative sum
                if params > node_index || end > nodes.len() {
                    return Err(EvalError::ExpectedParams(params));
                }
                // need to declare this here to avoid overflow
                start = node_index - params;

                // node_index shifts to where the start used to be after
                // all the params are drained
                (start, nodes.drain(start..end).collect())
            },
            Fixity::Prefix => {
                // No need to worry about node_index being less than zero because we
                // are adding 1 here
                let start = node_index + 1;
                // The +1 in the end of the range is because ranges stop
                // at the index 1 before the end
                let end = node_index + params + 1;
                if end > nodes.len() {
                    return Err(EvalError::ExpectedParams(params));
                }

                // node_index remains the same here because the params
                // are after the node
                (node_index, nodes.drain(start..end).collect())
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;

    use eval::fixity::Fixity;
    use eval::context_item::ContextItem;
    use eval::eval_context::EvalContext;
    use math::rich_number::RichNumber;

    #[test]
    fn creates_tree_with_correct_precedence() {
        // f 3 + (4 * 8 + 99 - x) - ((67)) / (x - g 4) + 32
        let expr: Expr = vec![
            ExprItem::SingleTerm(Term::Symbol("f".to_owned())),
            ExprItem::SingleTerm(Term::Number(3f64)),
            ExprItem::SingleTerm(Term::Symbol("+".to_owned())),
            ExprItem::Group(vec![
                ExprItem::SingleTerm(Term::Number(4f64)),
                ExprItem::SingleTerm(Term::Symbol("*".to_owned())),
                ExprItem::SingleTerm(Term::Number(8f64)),
                ExprItem::SingleTerm(Term::Symbol("+".to_owned())),
                ExprItem::SingleTerm(Term::Number(99f64)),
                ExprItem::SingleTerm(Term::Symbol("-".to_owned())),
                ExprItem::SingleTerm(Term::Symbol("x".to_owned())),
            ]),
            ExprItem::SingleTerm(Term::Symbol("-".to_owned())),
            ExprItem::Group(vec![
                ExprItem::Group(vec![
                    ExprItem::SingleTerm(Term::Number(67f64)),
                ])
            ]),
            ExprItem::SingleTerm(Term::Symbol("/".to_owned())),
            ExprItem::Group(vec![
                ExprItem::SingleTerm(Term::Symbol("x".to_owned())),
                ExprItem::SingleTerm(Term::Symbol("-".to_owned())),
                ExprItem::SingleTerm(Term::Symbol("g".to_owned())),
                ExprItem::SingleTerm(Term::Number(4f64)),
            ]),
            ExprItem::SingleTerm(Term::Symbol("+".to_owned())),
            ExprItem::SingleTerm(Term::Number(32f64)),
        ];

        let mut context = basic_context();

        let expected_tree = {
            let nleaf = |n| EvalTreeNode::new(ContextItem::Number(RichNumber::from(n)));
            let lookup = |name| context.get(name).unwrap();

            EvalTreeNode::with_children(lookup("-"), vec![
                EvalTreeNode::with_children(lookup("+"), vec![
                    EvalTreeNode::with_children(lookup("f"), vec![
                        nleaf(3),
                    ]),
                    EvalTreeNode::with_children(lookup("+"), vec![
                        EvalTreeNode::with_children(lookup("*"), vec![
                            nleaf(4),
                            nleaf(8),
                        ]),
                        EvalTreeNode::with_children(lookup("-"), vec![
                            nleaf(99),
                            EvalTreeNode::new(lookup("x")),
                        ]),
                    ]),
                ]),
                EvalTreeNode::with_children(lookup("+"), vec![
                    EvalTreeNode::with_children(lookup("/"), vec![
                        nleaf(67),
                        EvalTreeNode::with_children(lookup("-"), vec![
                            EvalTreeNode::new(lookup("x")),
                            EvalTreeNode::with_children(lookup("g"), vec![
                                nleaf(4),
                            ]),
                        ]),
                    ]),
                    nleaf(32),
                ]),
            ])
        };

        let tree = EvalTreeNode::from_expr(&mut context, expr).unwrap();

        assert_eq!(tree, expected_tree);
    }

    /// Defines a basic context with the operators: + - / * %
    /// with their normal, mathematical precedence
    /// as well as two functions f and g as well as a number x
    /// None of these definitions have a real, evaluatable function
    fn basic_context() -> EvalContext {
        let mut context = EvalContext::new();

        let fake_unary: Function = Function {
            params: vec!["a".to_owned()],
            body: Expr::new(),
        };

        let fake_binary: Function = Function {
            params: vec!["a".to_owned(), "b".to_owned()],
            body: Expr::new(),
        };

        context.set_number("x", RichNumber::from(3875f64));

        context.define_defaults("f", fake_unary.clone());
        context.define_defaults("g", fake_unary.clone());

        context.define("+", Fixity::Infix, 6, fake_binary.clone());
        context.define("-", Fixity::Infix, 6, fake_binary.clone());

        context.define("*", Fixity::Infix, 7, fake_binary.clone());
        context.define("/", Fixity::Infix, 7, fake_binary.clone());
        context.define("%", Fixity::Infix, 7, fake_binary.clone());

        context
    }
}

