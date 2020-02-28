//! Runtime inspection of Rust types.
//!
//! Whisper Schema is essentially a Whisper implementation of the (Serde data model)[https://serde.rs/data-model.html].
//! It is intended for simplicity and ease of use rather than speed. If performance is
//! necessary, then external predicates/external handlers are a better solution for
//! inspecting non-Whisper data from within a Whisper knowledge base.
//!
//! ## Encoding
//!
//! - `bool` => `"true", "false"`
//! - `i8`, `i16`, `i32` => directly to Whisper `Int32`
//! - `u8`, `u16`, `u32` => directly to Whisper `UInt32`
//! - `f32` => directly to Whisper `Float32`
//! - `i64`, `u64`, `f64` => ??? open question: use `Unused9` as a pointer to a 64-bit raw?
//!     Or maybe, an opaque tuple `(<type> <raw word>)`?
//! - `char` => ??? open question: use `Blob`, or tagged raw word `(<type> : <raw word>)`?
//! - `string` => ??? `Blob`? Or a tagged constant of some sort? `(SCHEMA::String <constant>)`?
//! - `byte array` => `Blob`
//! - `option` => `("Some" : <value>)` or `"None"`
//! - `unit` => `"()"` (this is a Whisper "raw" constant, not an empty Whisper tuple)
//! - `unit_struct` => see `unit`
//! - `unit_variant` => `<variant>` (variant name as constant, like `"None"` from `Option`)
//! - `newtype_struct` => directly serialize the inner value
//! - `newtype_variant` => `(<variant> : <value>)`
//! - `seq` => directly to Whisper cons-list
//! - `tuple` => directly to Whisper tuple
//! - `tuple_struct` => directly to Whisper tuple
//! - `tuple_variant` => (<variant> : <tuple>)`
//! - `map` => `{ <key>: <value> | <tail> }`
//! - `struct` => see `map`
//! - `struct_variant` => `(<variant> : <struct>)`
//!
//! ### Problems
//!
//! - `i64`, `u64`, and `f64` are annoying to encode because Whisper's tagged words have 64 bits
//!     for their tagged representation; so they can't be stored inline, and we don't have a
//!     specific 64-bit raw word representation yet.
//! - If we want to be self-describing (so that we can do things like use `serde_transcode`) then
//!     we need enum variants and tuples to be represented separately. This means we *cannot* use
//!     `(<variant> <value>)` for newtype variants/`(<variant> <struct>)` for struct variants if
//!     we want to be self-describing (which for convenience's sake, we *must* be.)
//! - Do we want to lean into using association lists for maps? Is there remotely a better option?
//!     We can't use hashmaps because we'd have to encode them into the heap somehow. It seems like
//!     association lists are the right structure up until we build in a more efficient hashmap,
//!     but any seriously map-heavy computations that aren't small enough to be done efficiently
//!     with an assoc-list should probably be done in native Rust, either outside of Whisper or
//!     through a builtin or extern goal.
//! - A consistent problem here is that `(<tag> <value>)` needs to be distinct from `[<tag>|<value>]`
//!     and `<tuple>`. We still have two unused tags in the "ref" half of our 4-bit tag space. Could
//!     we maybe use one to represent `<tag> <value>` pairs?
//!
//! ### Potential solutions
//!
//! - `Unused9` becomes `Tagged`; points to a two-word cell containing `<tag> <value>`.
//! - `Unused11` becomes `Cons2`; points to a three-word cell containing `<key> <value> <tail>`, for
//!     association-lists.
//! - The only downsides of this is that now we're out of tags. Can't add any more.
//!

use {
    ordered_float::OrderedFloat,
    typed_arena::Arena,
    whisper_ir::{
        graph::Blob,
        trans::{writer::VarScopeId, CompoundKind, TermEmitter, TermGraph, TermWriter},
        Ident, Scope, Symbol, Var,
    },
};

pub mod serde;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SchemaCompound<'arena> {
    Boxed(&'arena SchemaNode<'arena>),
    Seq(&'arena SchemaNode<'arena>, &'arena [SchemaNode<'arena>]),
    Tuple(&'arena [SchemaNode<'arena>]),
    TupleVariant(&'static str, &'arena [SchemaNode<'arena>]),
    Map(
        &'arena SchemaNode<'arena>,
        &'arena SchemaNode<'arena>,
        &'arena [SchemaNode<'arena>],
    ),
    StructVariant(&'static str, &'arena [SchemaNode<'arena>]),
    Tagged(&'static str, &'arena SchemaNode<'arena>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SchemaNode<'arena> {
    Var(Var),
    Const(Ident),
    Int32(i32),
    UInt32(u32),
    Float32(OrderedFloat<f32>),
    Blob(Blob),
    Raw(u64),

    ListNil,
    MapNil,

    Compound(SchemaCompound<'arena>),
}

pub type SchemaArena<'arena> = Arena<SchemaNode<'arena>>;

impl<'arena> SchemaNode<'arena> {
    #[inline(always)]
    fn visit<B>(&self, emitter: &mut TermEmitter<B, SchemaGraph<'arena>>, var_scope: VarScopeId)
    where
        B: TermWriter,
    {
        match self {
            SchemaNode::Var(v) => emitter.emit_var(var_scope, v.clone()),
            SchemaNode::Const(s) => emitter.emit_const(Scope::PUBLIC.symbol(s.clone()).into()),
            SchemaNode::Int32(i) => emitter.emit_i32(*i),
            SchemaNode::UInt32(u) => emitter.emit_u32(*u),
            SchemaNode::Float32(f) => emitter.emit_f32(f.into_inner()),
            SchemaNode::Blob(b) => emitter.emit_blob(b.clone()),
            SchemaNode::Raw(raw) => emitter.emit_raw(*raw),
            SchemaNode::ListNil => emitter.emit_const(Symbol::INTERNAL_LIST_NIL.into()),
            SchemaNode::MapNil => emitter.emit_const(Symbol::INTERNAL_MAP_NIL.into()),
            SchemaNode::Compound(c) => emitter.emit_compound(var_scope, *c),
        }
    }

    fn tagged(name: &'static str, node: &'arena SchemaNode<'arena>) -> Self {
        SchemaNode::Compound(SchemaCompound::Tagged(name, node))
    }

    fn const_from(s: impl Into<Ident>) -> Self {
        SchemaNode::Const(s.into())
    }
}

pub struct SchemaGraph<'arena> {
    arena: &'arena SchemaArena<'arena>,
}

impl<'arena> SchemaGraph<'arena> {
    pub fn new(arena: &'arena SchemaArena<'arena>) -> Self {
        Self { arena }
    }
}

impl<'arena> TermGraph for SchemaGraph<'arena> {
    type Id = SchemaCompound<'arena>;

    fn visit_compound<B>(
        emitter: &mut TermEmitter<B, Self>,
        var_scope: VarScopeId,
        &id: &Self::Id,
    ) -> B::Placement
    where
        B: TermWriter,
    {
        match id {
            SchemaCompound::Boxed(node) => {
                let placement = emitter.begin_box();
                node.visit(emitter, var_scope);
                emitter.end_box();
                placement
            }
            SchemaCompound::Seq(h, t) => {
                let placement = emitter.begin_compound(CompoundKind::Cons);
                h.visit(emitter, var_scope);
                match t.split_first() {
                    Some((h, t)) => emitter.emit_compound(var_scope, SchemaCompound::Seq(h, t)),
                    None => emitter.emit_const(Symbol::INTERNAL_LIST_NIL.into()),
                }
                emitter.end_compound();
                placement
            }
            SchemaCompound::Tuple(args) => {
                let placement = emitter.begin_compound(CompoundKind::Struct(args.len()));
                for arg in args {
                    arg.visit(emitter, var_scope);
                }
                emitter.end_compound();
                placement
            }
            SchemaCompound::TupleVariant(name, args) => {
                let placement = emitter.begin_compound(CompoundKind::Tagged);
                emitter.emit_const(Symbol::from(name).into());
                emitter.emit_compound(var_scope, SchemaCompound::Tuple(args));
                emitter.end_compound();
                placement
            }
            SchemaCompound::Map(k, v, t) => {
                let placement = emitter.begin_compound(CompoundKind::Cons2);
                k.visit(emitter, var_scope);
                v.visit(emitter, var_scope);
                match t
                    .split_first()
                    .and_then(|(k, t2)| t2.split_first().map(|(v, t)| (k, v, t)))
                {
                    Some((k, v, t)) => {
                        emitter.emit_compound(var_scope, SchemaCompound::Map(k, v, t))
                    }
                    None => emitter.emit_const(Symbol::INTERNAL_MAP_NIL.into()),
                }
                emitter.end_compound();
                placement
            }
            SchemaCompound::StructVariant(name, fields) => {
                let placement = emitter.begin_compound(CompoundKind::Tagged);
                emitter.emit_const(Symbol::from(name).into());

                if fields.is_empty() {
                    emitter.emit_const(Symbol::INTERNAL_MAP_NIL.into());
                } else {
                    emitter.emit_compound(
                        var_scope,
                        SchemaCompound::Map(&fields[0], &fields[1], &fields[2..]),
                    );
                }

                emitter.end_compound();
                placement
            }
            SchemaCompound::Tagged(tag, value) => {
                let placement = emitter.begin_compound(CompoundKind::Tagged);
                emitter.emit_const(Symbol::from(tag).into());
                value.visit(emitter, var_scope);
                emitter.end_compound();
                placement
            }
        }
    }
}
