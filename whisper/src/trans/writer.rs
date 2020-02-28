use ::{
    num_integer::Integer,
    std::{collections::HashMap, mem},
};

use whisper_ir::{
    graph::Blob,
    trans::{CompoundKind, TermWriter},
    Ident, Name, SymbolTable, Var,
};

use crate::{
    heap::{Heap, SharedHeap},
    query::QueryMap,
    word::{Address, Word},
};

/// Represents a single variable scope. Maps variable names to a `Word::var`
/// pointing at their first occurrence, only if they have already been written
/// at least once.
#[derive(Debug, Default)]
pub struct VarScope(HashMap<Ident, usize>);

impl VarScope {
    pub fn to_query_map(&self) -> QueryMap {
        QueryMap::from(&self.0)
    }
}

#[derive(Debug, Clone)]
pub struct HeapWriter<'heap> {
    heap: SharedHeap<'heap>,
}

impl From<Heap> for HeapWriter<'static> {
    fn from(heap: Heap) -> Self {
        Self::from(SharedHeap::Owned(heap))
    }
}

impl<'heap> From<&'heap mut Heap> for HeapWriter<'heap> {
    fn from(heap_mut: &'heap mut Heap) -> Self {
        Self::from(SharedHeap::BorrowedMut(heap_mut))
    }
}

impl<'heap> From<SharedHeap<'heap>> for HeapWriter<'heap> {
    fn from(shared: SharedHeap<'heap>) -> Self {
        Self { heap: shared }
    }
}

impl HeapWriter<'static> {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self::from(SharedHeap::Owned(Heap::new(symbol_table)))
    }
}

impl<'heap> HeapWriter<'heap> {
    pub fn as_heap(&self) -> &Heap {
        &*self.heap
    }

    pub fn into_heap(self) -> SharedHeap<'heap> {
        self.heap
    }
}

impl<'heap> TermWriter for HeapWriter<'heap> {
    type VarScope = VarScope;

    type Hole = Address;
    type Placement = Word;

    fn write_var(&mut self, scope: &mut VarScope, var: &Var) {
        let heap_mut = &mut self.heap.to_mut();
        match var {
            Var::Named(name) => {
                if let Some(&addr) = scope.0.get(name) {
                    heap_mut.words.push(Word::var(addr));
                } else {
                    let addr = heap_mut.words.len();
                    heap_mut.words.push(Word::var(addr));
                    scope.0.insert(name.clone(), addr);
                }
            }
            Var::Anonymous => {
                let addr = heap_mut.words.len();
                heap_mut.words.push(Word::var(addr));
            }
        }
    }

    fn write_const(&mut self, name: &Name) {
        let heap_mut = self.heap.to_mut();
        let index = heap_mut.symbols.write().resolve(name);
        heap_mut.words.push(Word::r#const(index));
    }

    fn write_i32(&mut self, i: i32) {
        self.heap.to_mut().words.push(Word::int32(i));
    }

    fn write_u32(&mut self, i: u32) {
        self.heap.to_mut().words.push(Word::uint32(i));
    }

    fn write_f32(&mut self, f: f32) {
        self.heap.to_mut().words.push(Word::float32(f));
    }

    fn write_forward(&mut self) -> Address {
        let heap_mut = &mut self.heap.to_mut();
        let addr = heap_mut.words.len();
        heap_mut.words.push(Word::BAD);
        addr
    }

    fn write_ref(&mut self, &placement: &Word) {
        self.heap.to_mut().words.push(placement);
    }

    fn push_box(&mut self) -> Word {
        Word::var(self.heap.words.len())
    }

    fn pop_box(&mut self) {}

    fn push_compound(&mut self, kind: CompoundKind) -> Word {
        let heap_mut = self.heap.to_mut();
        let addr = heap_mut.words.len();
        match kind {
            CompoundKind::Tagged => Word::tagged(addr),
            CompoundKind::Cons => Word::cons(addr),
            CompoundKind::Cons2 => Word::cons2(addr),
            CompoundKind::Struct(arity) => {
                heap_mut.words.push(Word::struct_arity(arity));
                Word::struct_ref(addr)
            }
            CompoundKind::Extern(arity) => {
                heap_mut.words.push(Word::extern_arity(arity));
                Word::extern_ref(addr)
            }
            CompoundKind::Opaque(arity) => {
                heap_mut.words.push(Word::opaque_arity(arity));
                Word::opaque_ref(addr)
            }
        }
    }

    fn pop_compound(&mut self) {}

    fn emit_blob(&mut self, blob: &Blob) -> Word {
        let mut_heap = self.heap.to_mut();
        let word_len = blob.as_bytes().len().div_ceil(&mem::size_of::<Word>());
        mut_heap.words.push(Word::binary_arity(word_len));
        let top = mut_heap.len();
        let new_len = top + word_len;
        mut_heap.words.resize(new_len, Word::ZERO);

        unsafe {
            let (empty, byte_slice, also_empty) = mut_heap[top..new_len].align_to_mut();
            assert!(empty.is_empty() && also_empty.is_empty());
            byte_slice[..blob.as_bytes().len()].copy_from_slice(&blob.as_bytes());
        }

        Word::binary_ref(top)
    }

    fn write_raw(&mut self, raw: u64) {
        self.heap.to_mut().words.push(Word(raw));
    }

    fn fill(&mut self, hole: Address, placement: &Word) {
        self.heap.to_mut().words[hole] = *placement;
    }

    type Output = Word;
    fn get(&mut self, placement: &Word) -> Word {
        *placement
    }
}

// impl<'heap> HeapWriter<'heap> {
//     fn emit_compound<F>(&mut self, scope: &mut VarScope, header: Header, thunk: F) -> Emitted
//     where
//         F: FnOnce(&mut HeapWriter<'heap>, &mut VarScope),
//     {
//         let addr = self.heap.len();
//         let word = match header {
//             Header::Cons => Word::cons(addr),
//             Header::Struct(arity) => {
//                 self.emit(Word::struct_arity(arity));
//                 Word::struct_ref(addr)
//             }
//             Header::Extern(arity) => {
//                 self.emit(Word::extern_arity(arity));
//                 Word::extern_ref(addr)
//             }
//             Header::Opaque(arity) => {
//                 self.emit(Word::opaque_arity(arity));
//                 Word::opaque_ref(addr)
//             }
//         };
//         thunk(self, scope);
//         Emitted(word)
//     }

//     fn emit_forward(&mut self, scope: &mut VarScope, graph: &T, id: &T::Id) -> Emitted {
//         graph.resolve_forward(self, scope, id)
//     }

//     fn emit(&mut self, word: Word) {
//         self.heap.to_mut().words.push(word);
//     }
//     /// Emit a variable by symbol, using the address in the supplied scope if already
//     /// emitted and adding it to the scope if not.
//     fn emit_var_by_symbol(&mut self, scope: &mut VarScope, var: Atom) {
//         use hash_map::Entry::*;

//         match scope.0.entry(var) {
//             Occupied(entry) => self.heap.to_mut().words.push(Word::var(*entry.get())),
//             Vacant(entry) => {
//                 let addr = self.heap.words.len();
//                 self.heap.to_mut().words.push(Word::var(addr));
//                 entry.insert(addr);
//             }
//         }
//     }

//     /// Emit a constant and intern it to the heap's symbol table if necessary.
//     fn emit_const(&mut self, cst: Name) {
//         let index = self.heap.to_mut().symbols.resolve(cst);
//         self.heap.to_mut().words.push(Word::r#const(index));
//     }

//     /// Emit an `i32` `Word`.
//     fn emit_i32(&mut self, i: i32) {
//         self.heap.to_mut().words.push(Word::int32(i));
//     }

//     /// Emit a `u32` `Word`.
//     fn emit_u32(&mut self, i: u32) {
//         self.heap.to_mut().words.push(Word::uint32(i));
//     }

//     /// Emit an `f32` `Word`.
//     fn emit_f32(&mut self, f: f32) {
//         self.heap.to_mut().words.push(Word::float32(f));
//     }

//     /// Emit either the known address of a struct/extern or a forward reference,
//     /// for later resolution.
//     fn emit_ref(&mut self, forward: ForwardRef<T::Id>) {
//         if let Some(&loc) = self.placements.get(&forward) {
//             self.heap.to_mut().words.push(loc);
//         } else {
//             let addr = self.heap.words.len();
//             self.heap.to_mut().words.push(Word::BAD);
//             self.forwards.push_back((addr, forward));
//         }
//     }

//     fn emit_blob(&mut self, obj: &Blob) -> Emitted {
//         let mut_heap = self.heap.to_mut();
//         let word_len = obj.as_bytes().len().div_ceil(&mem::size_of::<Word>());
//         mut_heap.words.push(Word::binary_arity(word_len));
//         let top = mut_heap.len();
//         let new_len = top + word_len;
//         mut_heap.words.resize(new_len, Word::ZERO);

//         unsafe {
//             let (empty, byte_slice, also_empty) = mut_heap[top..new_len].align_to_mut();
//             assert!(empty.is_empty() && also_empty.is_empty());
//             byte_slice[..obj.as_bytes().len()].copy_from_slice(&obj.as_bytes());
//         }

//         Emitted(Word::binary_ref(top))
//     }

//     /// Resolve any remaining forward references in the queue.
//     pub fn resolve_forwards(&mut self, scope: &mut VarScope, graph: &T) {
//         while let Some((addr, forward)) = self.forwards.pop_front() {
//             let resolved = match self.placements.get(&forward) {
//                 Some(resolved) => *resolved,
//                 None => {
//                     let Emitted(loc) = match &forward {
//                         ForwardRef::Compound(id) => graph.resolve_forward(self, scope, id),
//                         ForwardRef::Blob(object) => self.emit_blob(object),
//                     };
//                     self.placements.insert(forward, loc);
//                     loc
//                 }
//             };

//             self.heap.to_mut()[addr] = resolved;
//         }
//     }
// }

// impl<'heap> HeapWriter<'heap, IrTermGraph> {
//     /// Emit a single node into a word representing it and push it onto the heap,
//     /// emitting child nodes as necessary.
//     ///
//     /// If presented with a struct, this method will emit a [Word::BAD] rather than
//     /// a slice word with an address, and register the struct in the forwards queue
//     /// to be resolved later.
//     pub fn emit_node(&mut self, scope: &mut VarScope, node: &IrNode) {
//         use IrNode::*;
//         match node {
//             Var(v) => self.emit_var_by_symbol(scope, v.clone()),
//             Const(c) => self.emit_const(c.clone()),
//             Ref(s) => self.emit_ref(ForwardRef::Compound(*s)),
//             Int32(v) => self.heap.to_mut().words.push(Word::int32(*v)),
//             UInt32(v) => self.heap.to_mut().words.push(Word::uint32(*v)),
//             Float32(v) => self.heap.to_mut().words.push(Word::float32(*v)),
//             Blob(o) => self.emit_ref(ForwardRef::Blob(o.clone())),
//             _ => unreachable!("unrecognized node variant {:?}", node),
//         }
//     }

//     /// Emit a relation and related metadata.
//     pub fn emit_relation(&mut self, graph: &IrTermGraph, relation: &IrRelation) -> Relation {
//         assert!(self.forwards.is_empty());
//         self.placements.clear();
//         let scope = &mut VarScope::default();

//         let start = self.emit_forward(scope, graph, &relation.head).into();
//         self.resolve_forwards(scope, graph);

//         let neck = self.heap.len();
//         let mut goals = SmallVec::new();

//         for goal in &relation.body {
//             goals.push(self.emit_forward(scope, graph, &goal.unwrap()).into());
//         }
//         self.resolve_forwards(scope, graph);

//         Relation {
//             start,
//             neck,
//             end: self.heap.len(),
//             goals,
//         }
//     }

//     /// Emit a knowledge base.
//     pub fn emit_knowledge_base(&mut self, terms: &IrTermGraph, kb: &IrModule) -> KnowledgeBase {
//         let relations = kb
//             .entries
//             .iter()
//             .map(|entry| self.emit_relation(terms, entry.unwrap_ref()))
//             .collect();

//         KnowledgeBase::from_mapped_heap(SharedHeap::to_shared(&mut self.heap), relations)
//     }

//     /// Emit a query.
//     pub fn emit_query(&mut self, graph: &IrTermGraph, query: &IrQuery) -> Query {
//         assert!(self.forwards.is_empty());
//         self.placements.clear();

//         let mut scope = VarScope::default();
//         let mut goals = SmallVec::new();

//         for goal in &query.goals {
//             goals.push(self.emit_forward(&mut scope, graph, &goal.unwrap()).into());
//         }
//         self.resolve_forwards(&mut scope, graph);

//         Query {
//             heap: SharedHeap::to_shared(&mut self.heap),
//             goals,
//             vars: QueryMap::from(scope),
//         }
//     }
// }
