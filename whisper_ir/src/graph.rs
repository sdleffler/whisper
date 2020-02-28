//! A simple intermediate representation for Whisper which compiles to bytecode.
//!
//! There are two main "output" types for Whisper, and corresponding IR types:
//! - [`Clause`] is built from an [`IrModule`].
//! - [`Query`] is built from an [`IrQuery`].
//!
//! The Whisper IR is immutable, and for the most part allocated in an [`IrGraph`].
//! The [`IrGraph`] type acts as an arena holding node data and compound terms.
//!
//! ## The IR and the Whisper grammar
//!
//! Whisper is a very simple language. There are essentially two parts to the IR,
//! "terms" (also called "values") and then the things which hold terms, such as
//! queries, relations, and knowledge bases. Terms are a very simple tree structure.
//! These are the definitions of the types which describe a term:
//!
//! ```rust
//! # use ::{im::Vector, whisper_ir::{Symbol, graph::{IrRef, Blob}, parse::QuasiVar}};
//! #[non_exhaustive]
//! #[derive(Debug, Clone)]
//! pub enum IrNode {
//!     Var(Symbol),
//!     Const(Symbol),
//!     Ref(IrRef),
//!     Int32(i32),
//!     UInt32(u32),
//!     Float32(f32),
//!     Blob(Blob),
//!
//!     Quasi(QuasiVar),
//! }
//!
//! #[non_exhaustive]
//! #[derive(Debug, Clone)]
//! pub enum IrCompoundKind {
//!     Cons,
//!     Struct,
//!     Extern,
//!     Opaque,
//! }
//!
//! #[non_exhaustive]
//! #[derive(Debug, Clone)]
//! pub struct IrCompound {
//!     pub args: Vector<IrNode>,
//!     pub kind: IrCompoundKind,
//! }
//! ```
//!
//! [`IrRef`] here is a handle to an [`IrCompound`], from an [`IrGraph`]. It can be
//! dereferenced by indexing a graph like so: `ir_graph[ir_ref]`. A few other types
//! are nonobvious:
//! - `Blob` is arbitrary bincode-serialized bytes. This is for encoding special
//!   data directly into the heap, for manipulation by builtins or externs.
//! - `QuasiVar`, both in `IrCompoundKind` and `IrNode`, is a quasiquote variable.
//!   this is only used when compiling a macro into Rust code for codegen, and if
//!   encountered while emitting a heap, it will cause a panic.
//! - A "compound" term here is partially for precedence but refers to any term
//!   that is resolved to a pointer to other terms, once written to the heap (the
//!   exception to this rule is `Blob`, which does not have unification support.)
//!   compound types, when unified, will unify if and only if their elements unify.
//!   The default handler for externs will do this, and this behavior can be changed
//!   using an extern handler, but the idea is still that it will behave roughly as
//!   such. Behavior significantly departing from that norm should use an extern goal
//!   instead, maybe with a `Blob` to hold data if necessary.
//! - The four non-quasiquoted "compound" types are cons-cells, "structs" (tuples),
//!   externs (shiny tuples), and opaques (dark black tuples that most users don't
//!   need to see inside.) Cons-cells are special because they don't have a header,
//!   and require special list syntax sugar to express in textual form. That said,
//!   you can't write out opaques in the current syntax either, and that's for good
//!   reason.
//!
//! The rest of the grammar is comprised of [`IrRelation`], [`IrQuery`], [`IrGoal`],
//! [`IrModuleEntry`], and [`IrModule`]. A query is just a list of
//! compound terms describing the query goals. A relation is a compound term
//! describing the "head" of the relation, which is unified with a search term during
//! execution, and then a list of body goals. Relations can almost be thought of
//! as a query with a head. Last but not least, an [`IrModule`] is an ordered
//! collection of relations; order matters for execution, so it's important not to lose it.
//!
//! Currently, `IrGoal` is just a wrapper around an `IrCompound`. The difference is
//! important for parsing.
//!
//! ## Scoping of identifiers
//!
//! Identifiers in the Whisper IR, both for variables and constants, are [`Symbol`]s.
//! symbols consist of two parts, an interned string and an integer ID. The integer ID is
//! *not* intended for users, and is used by Whisper internally for scoping identifiers. The
//! way this works is that if a symbol has an ID of 0, it's considered to be globally
//! scoped. If a symbol has a nonzero ID, then it's considered to have a unique scope.
//! Whisper's support for scoping is currently *very* limited, and the only tools it gives
//! the user for handling scoping are the [`IrGraph::append_scoped`] and [`IrGraph::append_unscoped`]
//! methods, so use them wisely.

use crate::{
    parse::{self, ParseError},
    SymbolIndex,
};

use ::{
    derive_more::{From, Into},
    failure::Fail,
    im::Vector,
    std::mem,
};

pub mod fold;
pub mod module;
pub mod reader;
pub mod term;
pub mod visit;
pub mod writer;

pub use module::*;
pub use term::*;

#[derive(Debug, Fail, From)]
pub enum IrError {
    #[fail(display = "could not create module `{}`", _0)]
    ModuleCreationError(SymbolIndex, #[fail(cause)] Box<dyn Fail>),

    #[from]
    #[fail(display = "error parsing module")]
    ModuleParseError(#[fail(cause)] ParseError),
}

#[derive(Debug, Clone, From, Into)]
pub struct IrQuery {
    pub goals: Vector<IrGoal>,
}

impl From<Vector<IrRef>> for IrQuery {
    fn from(goals: Vector<IrRef>) -> Self {
        Self {
            goals: goals.into_iter().map(IrGoal::from).collect(),
        }
    }
}

impl IrTermGraph {
    pub fn parse_knowledge_base_str<S: AsRef<str>>(
        &mut self,
        string: S,
    ) -> Result<IrKnowledgeBase, IrError> {
        let terms_owned = mem::replace(self, IrTermGraph::new(self.symbols().clone()));
        parse::use_graphs(IrKnowledgeBase::new(self.symbols().clone()), terms_owned);

        let out = syn::parse_str::<IrKnowledgeBase>(string.as_ref()).map_err(|syn_err| {
            let parse_err = ParseError {
                src: string.as_ref().to_string(),
                syn_err,
            };
            IrError::from(parse_err)
        });

        *self = parse::swap_terms();
        Ok(out?)
    }

    pub fn parse_module_str<S: AsRef<str>>(
        &mut self,
        kb: &mut IrKnowledgeBase,
        module: IrModuleRef,
        string: S,
    ) -> Result<(), IrError> {
        let terms_owned = mem::replace(self, IrTermGraph::new(self.symbols().clone()));
        let kb_owned = mem::replace(kb, IrKnowledgeBase::new(self.symbols().clone()));

        parse::use_graphs(kb_owned, terms_owned);
        parse::set_module(module);

        let out = syn::parse_str::<IrModuleRef>(string.as_ref()).map_err(|syn_err| {
            let parse_err = ParseError {
                src: string.as_ref().to_string(),
                syn_err,
            };
            IrError::from(parse_err)
        });

        let (kb_recovered, terms_recovered) = parse::swap_graphs();
        *self = terms_recovered;
        *kb = kb_recovered;

        out?;
        Ok(())
    }

    pub fn parse_query_str<S: AsRef<str>>(&mut self, string: S) -> Result<IrQuery, IrError> {
        self.parse_query_str_with_root(SymbolIndex::MOD, string)
    }

    // TODO: figure out whether `root` is necessary for something and whether it's missing
    // some check or whatever here
    pub fn parse_query_str_with_root<S: AsRef<str>>(
        &mut self,
        _root: SymbolIndex,
        string: S,
    ) -> Result<IrQuery, IrError> {
        let terms_owned = mem::replace(self, IrTermGraph::new(self.symbols().clone()));
        parse::use_graphs(IrKnowledgeBase::new(self.symbols().clone()), terms_owned);
        let out = syn::parse_str(string.as_ref()).map_err(|syn_err| {
            let parse_err = ParseError {
                src: string.as_ref().to_string(),
                syn_err,
            };
            IrError::from(parse_err)
        });
        let (_, terms_recovered) = parse::swap_graphs();
        *self = terms_recovered;
        Ok(out?)
    }
}
