//! Whisper is a small, embeddable logic programming language in pure Rust. The `whisper` crate
//! includes functionality for easily constructing Whisper syntax and compiling it into bytecode
//! for execution. It supports a simplistic foreign function interface for calling into external
//! code, as well as facilities for supporting backtracking for external goals. This makes it
//! possible to use Whisper for doing a sort of traced proof-search, where if Whisper finds a
//! solution, you can use external goals to track what steps it took to get there, and react
//! accordingly.
//!
//! Due to its structure as a logic programming language, it can also work well as an embedded read-heavy
//! database. Writing would require recompiling a knowledge base, which is currently not terribly
//! performant since emitting a compiled heap means reading through the entire knowledge base. This could
//! be improved through incremental compilation but is not a high priority.
//!
//! ## Motivation
//!
//! Logic programming is a very convenient system for problems which need inference, but mixing
//! multiple programming languages can be very inconvenient. Whisper is intended to serve as a
//! simple, embeddable, and usably performant solution for embedding logic programs in Rust.
//!
//! Whisper is *not* intended as:
//! - A super-fast, feature-rich Pro/Hilog implementation which just happens to have good Rust interop.
//! - A database capable of streaming to/from disk.
//! - A super-small interpreter with a tiny dependency footprint.
//!
//! ## Features
//!
//! - Simple, human-readable syntax, with limited support for scoping constants
//! - A simple, easy to build and modify intermediate representation (IR)
//! - Macros for generating Whisper IR programmatically, with quasiquoting support
//! - A parser for parsing files containing Whisper syntax
//! - Simple but powerful interface for external goals written in Rust, with support for properly handling
//!   backtracking
//! - Simple but powerful interface for external datatypes, with support for handling unifying two external
//!   values against each other
//!

pub mod builder;
pub mod heap;
pub mod knowledge_base;
pub mod maybe_shared;
pub mod query;
pub mod runtime;
pub mod trans;
pub mod word;

pub use crate::{
    heap::{Heap, SharedHeap},
    knowledge_base::KnowledgeBase,
    query::{Query, SharedQuery},
    runtime::{ExternModule, SimpleSession},
    trans::HeapWriter,
    word::{Address, Tag, UnpackedWord, Word},
};

pub use whisper_ir::{Ident, Name, Symbol, SymbolIndex, SymbolTable, Var};

pub mod ir {
    pub use whisper_ir::graph::*;
}

pub use whisper_derive::*;

#[doc(hidden)]
pub use im::vector;

pub use whisper_ir::atom;

pub mod prelude {
    pub use crate::{
        builder::QueryBuilder,
        heap::{Heap, SharedHeap},
        knowledge_base::{KnowledgeBase, Module},
        query::{Query, SharedQuery},
        runtime::{
            goal::{Goal, Variable},
            ExternModule, Machine, ModuleCache, Resolver, Runtime, SimpleSession,
        },
    };

    pub use whisper_ir::{graph::*, Ident, Name, Symbol, SymbolIndex, SymbolTable, Var};
}

pub mod schema {
    pub use whisper_schema::{
        serde::{de, ser, Error},
        SchemaArena, SchemaGraph, SchemaNode,
    };
}

#[macro_export]
macro_rules! constant {
    ($id:ident) => {{
        #[derive(Serialize, Deserialize)]
        #[allow(non_camel_case_types)]
        enum __Keyword {
            $id,
        };

        __Keyword::$id
    }};
}

#[macro_export]
macro_rules! constants {
    ($v:vis enum $name:ident { $($id:ident),+ $(,)? }) => {
        #[derive(Debug, Serialize, Deserialize)]
        #[allow(non_camel_case_types)]
        $v enum $name {
            $($id),+
        }
    };
}
