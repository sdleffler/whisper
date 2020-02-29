use ::{
    itertools::Itertools,
    serde::{Deserialize, Serialize},
    whisper::prelude::*,
};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Bar {
    Baz(i32),
    Quux(String),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Foo {
    maybe_thing: Option<u8>,
    corge: bool,
    bar: Bar,
}

whisper::module! {
    fn validator();

    valid (Foo) Schema if valid_fields Foo Schema;

    valid_fields { Name: Value | Fields } Schema if
        valid_field Name Value Schema,
        valid_fields Fields Schema;
    valid_fields {} Schema;

    valid_field Name Value { Name: Value | Rest };
    valid_field Name Value { X: Y | Rest } if
        valid_field Name Value Rest;
}

whisper::query! {
    fn validator_query(foo: IrNode);

    valid_fields #foo {
        <corge> : <true>,
        <bar> : Y,
        <maybe_thing> : Z,
    };
}

#[test]
fn query_foo() {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("query_foo");

    let symbol_table = SymbolTable::new();
    let mut modules = IrKnowledgeBase::new(symbol_table.clone());
    let mut terms = IrTermGraph::new(symbol_table.clone());

    let validator_mod = modules.module(SymbolIndex::MOD);
    validator(&mut terms, &mut modules, validator_mod);

    let validator_kb = whisper::trans::knowledge_base(&terms, &modules);

    let mut session = SimpleSession::new(symbol_table.clone());

    let mut query = Query::new(symbol_table.clone());

    let mut builder = QueryBuilder::from(&mut query);
    let foo_addr = builder.bind(&Foo {
        maybe_thing: None,
        corge: true,
        bar: Bar::Baz(42),
    });
    let ir_query = validator_query(&mut terms, modules[validator_mod].root(), foo_addr);
    builder.push(&terms, &ir_query);
    builder.finish();

    let mut module_cache = ModuleCache::new();
    module_cache.init(&validator_kb);
    session.load_with_extern_state_and_reuse_query(&mut query, &module_cache, ());

    assert!(session.resume(&mut module_cache, &validator_kb));
    artifact.insert_display(
        "session query",
        &session.query_vars().iter().format_with(", ", |(k, v), f| {
            f(&format_args!("{} => {}", k, session.heap().display_at(*v)))
        }),
    );
    assert!(!session.resume(&mut module_cache, &validator_kb));

    query.clear();
    let mut builder = QueryBuilder::from(&mut query);
    let foo_addr = builder.bind(&Option::<bool>::None);
    let ir_query = validator_query(&mut terms, modules[validator_mod].root(), foo_addr);
    builder.push(&terms, &ir_query);
    builder.finish();

    session.load_with_extern_state_and_reuse_query(&mut query, &module_cache, ());
    assert!(!session.resume(&mut module_cache, &validator_kb));

    egress.close_and_assert_unregressed().unwrap();
}
