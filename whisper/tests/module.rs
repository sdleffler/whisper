use ::{
    itertools::Itertools,
    whisper::{prelude::*, session::DebugHandler},
};

whisper::module! {
    fn list_module();

    fubar if fubar in extern;
}

whisper::module! {
    fn wrap_module();

    wrap if
        wrap in extern,
        fubar in list;
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

    let list_mod_ref = ir_kb.new_named_module_with_root(Scope::MOD.symbol("list"));
    list_module(&mut terms, &mut ir_kb, list_mod_ref);

    let wrap_mod_ref = ir_kb.new_named_module_with_root(Symbol::MOD);
    wrap_module(&mut terms, &mut ir_kb, wrap_mod_ref);

    let kb = whisper::trans::knowledge_base(&terms, &ir_kb);
    let mut session = Session::<DebugHandler>::new(symbols.clone(), kb.into());

    let ir_query = wrap_query(&mut terms, &Symbol::PUBLIC);
    let query = QueryBuilder::new(Heap::new(symbols.clone())).finish(&terms, &ir_query);
    session.load(query.into());

    assert!(session.resume());

    println!(
        "{}",
        &session.query_vars().iter().format_with(", ", |(k, v), f| {
            f(&format_args!("{} => {}", k, session.heap().display_at(*v)))
        })
    );
}
