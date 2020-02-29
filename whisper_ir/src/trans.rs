use std::hash::Hash;

pub mod reader;
pub mod writer;

pub use reader::{TermReader, TermVisitor};
pub use writer::{TermEmitter, TermGraph, TermWriter, VarScopeId};

pub trait Ref: Eq + Hash + Clone {}
impl<T: Eq + Hash + Clone> Ref for T {}

#[derive(Debug, Clone, Copy)]
pub enum CompoundKind {
    Tagged,
    Cons,
    Cons2,
    Struct(usize),
    Extern(usize),
    Opaque(usize),
}
