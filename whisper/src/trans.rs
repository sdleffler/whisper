use smallvec::SmallVec;

use whisper_ir::graph::{IrGoal, IrKnowledgeBase, IrModule, IrModuleEntry, IrQuery, IrTermGraph};

pub mod reader;
pub mod writer;

pub use whisper_ir::trans::TermEmitter;

pub use reader::HeapReader;
pub use writer::HeapWriter;

use crate::{
    heap::Heap,
    knowledge_base::{KnowledgeBase, Module, Relation},
    query::Query,
};

pub fn query(terms: &IrTermGraph, query: &IrQuery, mut heap: Heap) -> Query {
    assert_eq!(terms.symbols(), heap.symbols());

    let mut goals = SmallVec::new();
    heap.clear();
    let mut emitter = TermEmitter::new(terms, HeapWriter::from(&mut heap));
    let var_scope = emitter.insert_fresh_scope();

    for ir_goal in &query.goals {
        let goal = match ir_goal {
            IrGoal::Goal(ir_ref) => emitter.emit_full(var_scope, *ir_ref),
            _ => todo!("error handling: unbound quasivariable"),
        };

        goals.push(goal);
    }

    let vars = emitter.get_var_scope(var_scope).to_query_map();

    Query { heap, goals, vars }
}

pub fn module(terms: &IrTermGraph, module: &IrModule, mut heap: Heap) -> Module {
    let mut emitter = TermEmitter::new(terms, HeapWriter::from(&mut heap));
    let mut relations = Vec::<Relation>::new();

    for entry in module.entries.iter() {
        let relation = match entry {
            IrModuleEntry::Relation(it) => it,
            _ => todo!("error handling: unbound quasivariable"),
        };

        let var_scope = emitter.insert_fresh_scope();

        let start = emitter.emit_full(var_scope, relation.head);
        let neck = emitter.get_builder().as_heap().len();

        let mut goals = SmallVec::new();
        for ir_goal in relation.body.iter() {
            let goal = match ir_goal {
                IrGoal::Goal(ir_ref) => emitter.emit_full(var_scope, *ir_ref),
                _ => todo!("error handling: unbound quasivariable"),
            };

            goals.push(goal);
        }

        let end = emitter.get_builder().as_heap().len();

        relations.push(Relation {
            start,
            end,
            neck,
            goals,
        });
    }

    Module::from_mapped_heap(module.root().clone(), heap, relations)
}

pub fn knowledge_base(terms: &IrTermGraph, ir_modules: &IrKnowledgeBase) -> KnowledgeBase {
    let mut kb = KnowledgeBase::new(terms.symbols().clone());
    for ir_module in ir_modules.iter() {
        kb.insert(module(terms, ir_module, Heap::new(terms.symbols().clone())));
    }

    kb
}
