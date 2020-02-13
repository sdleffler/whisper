// use ::{
//     failure::Error,
//     itertools::Itertools,
//     std::io::{self, prelude::*},
//     whisper::{
//         ir::IrTermGraph, session::DebugHandler, Heap, HeapEmitter, Session, SharedKnowledgeBase,
//         SharedQuery,
//     },
// };

// fn main() -> Result<(), Error> {
//     let symbol_table = SymbolTable::new();
//     let mut terms = IrTermGraph::new(symbol_table);
//     let ir_kb = terms.parse_clause(include_str!("./extern_call.wh"));
//     let kb = SharedKnowledgeBase::Owned(
//         HeapEmitter::new(terms.symbol_table().clone()).emit_knowledge_base(&terms, &ir_kb),
//     );

//     println!("Compiled knowledge base:\n{}", kb.display());
//     // println!("(debug view)\n{:?}", kb);

//     let stdin = io::stdin();
//     for line in stdin.lock().lines() {
//         let ir_query = terms.parse_query(&line?);
//         let query = SharedQuery::Owned(
//             HeapEmitter::from(Heap::new(kb.symbol_table().clone())).emit_query(&terms, &ir_query),
//         );

//         // println!("Compiled query:\n{}", *query);

//         let mut session = Session::with_handler(kb.clone(), query, DebugHandler);
//         loop {
//             if !session.resume() {
//                 println!("no more solutions.");
//                 break;
//             }

//             println!(
//                 "found solution [{}].",
//                 session
//                     .query_vars()
//                     .iter()
//                     .format_with(", ", |(k, v), f| f(&format_args!(
//                         "{} => {}",
//                         k,
//                         session.heap().display_at(*v)
//                     )))
//             );
//         }
//     }

//     Ok(())
// }

fn main() {}
