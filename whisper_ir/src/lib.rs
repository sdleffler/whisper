#![deny(mutable_borrow_reservation_conflict)]

pub mod graph;
#[macro_use]
pub mod symbol;
pub mod trans;

// TODO: feature-gate
pub mod parse;
pub mod proc_macro;

pub use crate::{graph::*, symbol::*};

#[doc(hidden)]
pub use im::{hashset, vector};
