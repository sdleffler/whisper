use ::{serde::Serialize, std::mem};

use whisper_ir::{trans::TermEmitter, IrGoal, IrNode, IrQuery, IrTermGraph};
use whisper_schema::{SchemaArena, SchemaGraph};

use crate::{
    maybe_shared::MaybeShared,
    query::Query,
    trans::{writer::VarScope, HeapWriter},
    word::{Address, Word},
};

pub struct QueryBuilder<'query> {
    query: MaybeShared<'query, Query>,
    var_scope: VarScope,
}

impl From<Query> for QueryBuilder<'static> {
    fn from(query: Query) -> Self {
        Self::new(MaybeShared::Owned(query))
    }
}

impl<'query> From<&'query mut Query> for QueryBuilder<'query> {
    fn from(query: &'query mut Query) -> Self {
        Self::new(MaybeShared::BorrowedMut(query))
    }
}

impl<'query> From<&'query Query> for QueryBuilder<'query> {
    fn from(query: &'query Query) -> Self {
        Self::new(MaybeShared::Borrowed(query))
    }
}

impl<'query> QueryBuilder<'query> {
    pub fn new(query: MaybeShared<'query, Query>) -> Self {
        let var_scope = VarScope::default();
        Self { query, var_scope }
    }

    pub fn bind<S: Serialize>(&mut self, value: &S) -> IrNode {
        let mut_query = self.query.to_mut();
        let writer = HeapWriter::from(&mut mut_query.heap);
        let addr = writer.as_heap().len();

        let arena = SchemaArena::new();
        let graph = SchemaGraph::new(&arena);
        let mut emitter = TermEmitter::new(graph, writer);
        whisper_schema::serde::ser::to_emitter(&mut emitter, value).unwrap();
        emitter.resolve_forwards();

        IrNode::Raw(Word::pointer_to(self.query.heap[addr], addr).0)
    }

    pub fn goal<S: Serialize>(&mut self, goal: &S) -> Address {
        let mut_query = self.query.to_mut();
        let writer = HeapWriter::from(&mut mut_query.heap);
        let addr = writer.as_heap().len();

        let arena = SchemaArena::new();
        let graph = SchemaGraph::new(&arena);
        let mut emitter = TermEmitter::new(graph, writer);
        whisper_schema::serde::ser::to_emitter(&mut emitter, goal).unwrap();
        emitter.resolve_forwards();

        let goal_word = Word::pointer_to(mut_query.heap[addr], addr);
        mut_query.goals.push(goal_word);

        addr
    }

    pub fn push(&mut self, terms: &IrTermGraph, ir_query: &IrQuery) {
        let mut_query = self.query.to_mut();
        assert_eq!(terms.symbols(), mut_query.heap.symbols());

        let writer = HeapWriter::from(&mut mut_query.heap);
        let mut emitter = TermEmitter::new(terms, writer);
        let var_scope_id = emitter.push_scope(mem::take(&mut self.var_scope));
        for ir_goal in &ir_query.goals {
            let goal = match ir_goal {
                IrGoal::Goal(ir_ref) => emitter.emit_full(var_scope_id, *ir_ref),
                _ => todo!("error handling: unbound quasivariable"),
            };
            mut_query.goals.push(goal);
        }
        emitter.resolve_forwards();

        self.var_scope = emitter.take_var_scope(var_scope_id);
    }

    pub fn finish(mut self) -> MaybeShared<'query, Query> {
        let vars = self.var_scope.to_query_map();
        self.query.to_mut().vars = vars;
        self.query
    }
}
