// use ::{
//     failure::Error,
//     itertools::Itertools,
//     std::io::{self, prelude::*},
//     whisper::{ir::IrGraph, Heap, HeapEmitter, SharedKnowledgeBase, SharedQuery, SimpleSession},
// };

// fn main() -> Result<(), Error> {
//     let mut ir_graph = IrGraph::new();
//     let ir_kb = ir_graph.parse_clause(include_str!("./simple_knowledge_base.wh"));
//     let kb = SharedKnowledgeBase::Owned(
//         HeapEmitter::new(ir_graph.symbols().clone()).emit_knowledge_base(&ir_graph, &ir_kb),
//     );

//     println!("Compiled knowledge base:\n{}", kb.display());
//     // println!("(debug view)\n{:?}", kb);

//     let stdin = io::stdin();
//     for line in stdin.lock().lines() {
//         let ir_query = ir_graph.parse_query(&line?);
//         let query = SharedQuery::Owned(
//             HeapEmitter::from(Heap::new(kb.symbols().clone()))
//                 .emit_query(&ir_graph, &ir_query),
//         );

//         // println!("Compiled query:\n{}", *query);

//         let mut session = SimpleSession::new(kb.clone(), query);
//         let mut solutions_found = 0;
//         const SOLUTION_LIMIT: usize = 128;
//         loop {
//             if !session.resume() || solutions_found >= SOLUTION_LIMIT {
//                 if solutions_found >= SOLUTION_LIMIT {
//                     println!(
//                         "LIMIT EXCEEDED! Halting! (Limit: {} solutions)",
//                         SOLUTION_LIMIT
//                     );
//                 }

//                 println!(
//                     "no more solutions; {} total solutions found.",
//                     solutions_found
//                 );
//                 break;
//             }

//             println!(
//                 "found solution #{} ({}).",
//                 solutions_found,
//                 session
//                     .query_vars()
//                     .iter()
//                     .format_with(", ", |(k, v), f| f(&format_args!(
//                         "{} => {}",
//                         k,
//                         session.heap().display_at(*v)
//                     )))
//             );

//             solutions_found += 1;
//         }
//     }

//     Ok(())
// }

fn main() {}
