use ::{
    failure::Error,
    itertools::Itertools,
    std::io::{self, prelude::*},
    whisper::{
        ir::{IrKnowledgeBase, IrTermGraph},
        Heap, KnowledgeBase, SharedQuery, SimpleSession, Symbol, SymbolTable,
    },
};

whisper::module! {
    fn simple_knowledge_base();

    trude is_mother_of sally;

    tom  is_father_of sally;
    tom  is_father_of erica;
    mike is_father_of tom;

    X is_sibling_of Y if Z is_parent_of X, Z is_parent_of Y;

    X is_parent_of Y if X is_father_of Y;
    X is_parent_of Y if X is_mother_of Y;
}

fn main() -> Result<(), Error> {
    let symbol_table = SymbolTable::new();
    let mut modules = IrKnowledgeBase::new(symbol_table.clone());
    let mut terms = IrTermGraph::new(symbol_table.clone());

    let module = modules.new_named_module_with_root(Symbol::PUBLIC);
    simple_knowledge_base(&mut terms, &mut modules, module);
    modules.link(&mut terms, module);

    println!("Compiled module IR:\n{:?}", modules[module]);

    let kb = whisper::trans::knowledge_base(&terms, &modules);

    println!(
        "Compiled knowledge base:\n{}",
        kb.get(Symbol::PUBLIC_INDEX).unwrap().display()
    );
    // println!("(debug view)\n{:?}", kb);

    let mut session = SimpleSession::new(symbol_table.clone(), kb);

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let ir_query = terms
            .parse_query_str_with_root(modules[module].get_root().clone(), &line?)
            .unwrap();

        let query = SharedQuery::Owned(whisper::trans::query(
            &terms,
            &ir_query,
            Heap::new(symbol_table.clone()),
        ));

        println!("Compiled query:\n{}", *query);

        session.load(query);
        let mut solutions_found = 0;
        const SOLUTION_LIMIT: usize = 32;
        loop {
            if !session.resume() || solutions_found >= SOLUTION_LIMIT {
                if solutions_found >= SOLUTION_LIMIT {
                    println!(
                        "LIMIT EXCEEDED! Halting! (Limit: {} solutions)",
                        SOLUTION_LIMIT
                    );
                }

                println!(
                    "no more solutions; {} total solutions found.",
                    solutions_found
                );
                break;
            }

            println!(
                "found solution #{} ({}).",
                solutions_found,
                session
                    .query_vars()
                    .iter()
                    .format_with(", ", |(k, v), f| f(&format_args!(
                        "{} => {}",
                        k,
                        session.heap().display_at(*v)
                    )))
            );

            solutions_found += 1;
        }
    }

    Ok(())
}
