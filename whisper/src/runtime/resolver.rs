use crate::{
    heap::Heap,
    knowledge_base::{KbEntry, KnowledgeBase, Matches, Module},
    query::{Query, QueryMap},
    runtime::ExternModule,
    word::{Address, Tag, Word},
    SymbolIndex, SymbolTable,
};

use ::{
    failure::Error,
    smallvec::SmallVec,
    std::{collections::HashMap, fmt, marker::PhantomData, mem, ops::Index},
    whisper_ir::{trans::TermEmitter, IrKnowledgeBase, IrTermGraph},
    whisper_schema::{SchemaArena, SchemaGraph},
};

#[derive(Debug, Clone)]
pub enum Resolved<E: ExternModule> {
    Dynamic(Module),
    Extern(E),
}

impl<'a, E: ExternModule> From<&'a KbEntry<E>> for Resolved<E> {
    fn from(entry: &'a KbEntry<E>) -> Self {
        match entry {
            KbEntry::Dynamic(module) => Resolved::Dynamic(module.clone()),
            KbEntry::Extern(ext) => Resolved::Extern(ext.clone()),
        }
    }
}

pub trait Resolver<E: ExternModule>: Send + Sync {
    fn symbols(&self) -> &SymbolTable;
    fn resolve(&self, idx: Word, heap: &Heap) -> Option<Resolved<E>>;
    fn root(&self) -> (Word, Resolved<E>);
}

impl<E: ExternModule> Resolver<E> for KnowledgeBase<E> {
    fn symbols(&self) -> &SymbolTable {
        self.symbols()
    }

    fn resolve(&self, idx: Word, heap: &Heap) -> Option<Resolved<E>> {
        self.get(SymbolIndex(
            idx.debug_assert_tag(Tag::Const).get_value() as usize,
            heap.symbols().id(),
        ))
        .map(Resolved::from)
    }

    fn root(&self) -> (Word, Resolved<E>) {
        let (root_idx, entry) = self.root();
        (Word::r#const(root_idx.0), Resolved::from(entry))
    }
}
