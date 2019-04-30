#![recursion_limit="128"]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

pub mod ast;
pub mod parser;
pub mod unit_graph;
pub mod interpreter;
pub mod symbols;
pub mod canonical;
pub mod ir;
pub mod display_string;
