#![deny(mutable_borrow_reservation_conflict)]
#![feature(option_unwrap_none)]

pub mod graph;
#[macro_use]
pub mod symbol;
pub mod trans;

// TODO: feature-gate
pub mod parse;
pub mod proc_macro;

pub use crate::{
    graph::*,
    symbol::{Ident, Name, Symbol, SymbolIndex, SymbolTable, SymbolTableInner, Var},
};

#[doc(hidden)]
pub use im::{hashset, vector};
