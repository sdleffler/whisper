use ::{serde::Serialize, smallvec::SmallVec};

use whisper_ir::{trans::TermEmitter, IrGoal, IrNode, IrQuery, IrTermGraph};
use whisper_schema::{SchemaArena, SchemaGraph};

use crate::{
    heap::Heap,
    query::Query,
    trans::{writer::VarScope, HeapWriter},
    word::Word,
};

pub struct QueryBuilder<'heap> {
    writer: HeapWriter<'heap>,
    var_scope: VarScope,
    goals: SmallVec<[Word; 8]>,
}

impl<'heap> QueryBuilder<'heap> {
    pub fn new(heap: Heap) -> Self {
        let writer = HeapWriter::from(heap);
        let var_scope = VarScope::default();
        let goals = SmallVec::new();
        Self {
            writer,
            var_scope,
            goals,
        }
    }

    pub fn bind<S: Serialize>(&mut self, value: &S) -> IrNode {
        let addr = self.writer.as_heap().len();

        let arena = SchemaArena::new();
        let graph = SchemaGraph::new(&arena);
        let mut emitter = TermEmitter::new(graph, &mut self.writer);
        whisper_schema::serde::ser::to_emitter(&mut emitter, value).unwrap();
        emitter.resolve_forwards();

        IrNode::Raw(Word::pointer_to(self.writer.as_heap()[addr], addr).0)
    }

    pub fn finish(mut self, terms: &IrTermGraph, ir_query: &IrQuery) -> Query {
        assert_eq!(terms.symbols(), self.writer.as_heap().symbols());

        let mut emitter = TermEmitter::new(terms, &mut self.writer);
        let var_scope = emitter.push_scope(self.var_scope);
        for ir_goal in &ir_query.goals {
            let goal = match ir_goal {
                IrGoal::Goal(ir_ref) => emitter.emit_full(var_scope, *ir_ref),
                _ => todo!("error handling: unbound quasivariable"),
            };
            self.goals.push(goal);
        }
        emitter.resolve_forwards();
        let vars = emitter.get_var_scope(var_scope).to_query_map();

        Query {
            heap: self.writer.into_heap().into_unscoped(),
            goals: self.goals,
            vars,
        }
    }
}
