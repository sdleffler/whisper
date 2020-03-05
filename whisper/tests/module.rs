use ::{
    itertools::Itertools,
    whisper::{
        prelude::*,
        runtime::{NullHandler, Session},
    },
};

whisper::module! {
    fn list_module();

    fubar if
        in(*) println "fubar";
}

whisper::module! {
    fn wrap_module();

    wrap if
        in(*) println "wrap",
        in(list) fubar;
}

whisper::query! {
    fn wrap_query();

    wrap;
}

#[test]
fn query_module_inner() {
    let symbols = SymbolTable::new();
    let mut terms = IrTermGraph::new(symbols.clone());
    let mut ir_kb = IrKnowledgeBase::new(symbols.clone());

    let list_mod_ref = ir_kb.module(symbols.write().insert_with_parent("list", SymbolIndex::MOD));
    list_module(&mut terms, &mut ir_kb, list_mod_ref);

    let wrap_mod_ref = ir_kb.module(SymbolIndex::MOD);
    wrap_module(&mut terms, &mut ir_kb, wrap_mod_ref);

    let kb: KnowledgeBase<NullHandler> = whisper::trans::knowledge_base(&terms, &ir_kb);
    let mut session = Session::new(symbols.clone());

    let ir_query = wrap_query(&mut terms, SymbolIndex::PUBLIC);
    let mut builder = QueryBuilder::from(Query::new(symbols.clone()));
    builder.push(&terms, &ir_query);

    let mut modules = ModuleCache::new();
    modules.init(&kb);
    session.load(builder.finish().into_owned(), &modules);

    assert!(session.resume(&mut modules, &kb).is_solution());

    println!(
        "{}",
        &session.query_vars().iter().format_with(", ", |(k, v), f| {
            f(&format_args!("{} => {}", k, session.heap().display_at(*v)))
        })
    );
}
