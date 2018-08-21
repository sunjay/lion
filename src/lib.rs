#![recursion_limit="128"]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_locate;
#[macro_use]
extern crate smallvec;
extern crate rust_decimal;

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
