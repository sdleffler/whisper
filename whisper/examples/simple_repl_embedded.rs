use ::{
    failure::Error,
    itertools::Itertools,
    std::io::{self, prelude::*},
    whisper::{
        prelude::*,
        runtime::{NullHandler, Session},
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

    let module = modules.module(SymbolIndex::MOD);
    simple_knowledge_base(&mut terms, &mut modules, module);

    println!("Compiled module IR:\n{:?}", modules[module]);

    let kb = whisper::trans::knowledge_base::<NullHandler>(&terms, &modules);
    // println!("(debug view)\n{:?}", kb);

    let mut session = Session::new(symbol_table.clone());
    let mut module_cache = ModuleCache::new();
    module_cache.init(&kb);

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let ir_query = terms
            .parse_query_str_with_root(modules[module].root(), &line?)
            .unwrap();

        let query = whisper::trans::query(&terms, &ir_query, Heap::new(symbol_table.clone()));

        println!("Compiled query:\n{}", query);

        session.load(query, &module_cache);
        let mut solutions_found = 0;
        const SOLUTION_LIMIT: usize = 32;
        loop {
            if !session.resume(&mut module_cache, &kb).is_solution()
                || solutions_found >= SOLUTION_LIMIT
            {
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
