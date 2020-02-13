use ::{
    failure::Error,
    im,
    serde::{
        de::{DeserializeOwned, Deserializer},
        Deserialize,
    },
    serde_json::Value,
    std::marker::PhantomData,
    whisper::{prelude::*, session::DebugHandler},
};

#[derive(Debug, Clone)]
pub struct Schema(SharedKnowledgeBase);

impl Schema {
    pub fn from_str<S: AsRef<str> + ?Sized>(string: &S) -> Self {
        let mut terms = IrTermGraph::new(SymbolTable::new());
        let ir_kb = terms.parse_knowledge_base_str(string).expect("oops");
        Self(whisper::trans::knowledge_base(&terms, &ir_kb).into())
    }

    pub fn from_embedded(embedded: fn(&mut IrTermGraph) -> IrKnowledgeBase) -> Self {
        let mut terms = IrTermGraph::new(SymbolTable::new());
        let ir_kb = embedded(&mut terms);
        Self(whisper::trans::knowledge_base(&terms, &ir_kb).into())
    }
}

whisper::query! {
    fn validator_query(input: IrNode);

    valid #input;
}

#[derive(Debug)]
pub struct Validator<T: DeserializeOwned> {
    symbols: SymbolTable,
    session: Session<DebugHandler>,
    _phantom: PhantomData<T>,
}

impl<T: DeserializeOwned> Validator<T> {
    pub fn new(schema: &Schema) -> Self {
        Self {
            symbols: schema.0.symbol_table().clone(),
            session: Session::new(schema.0.symbol_table().clone(), schema.0.clone()),
            _phantom: PhantomData,
        }
    }

    fn build_validator_query(&self, value: &Value) -> SharedQuery {
        let mut terms = IrTermGraph::new(self.symbols.clone());
        let mut builder = QueryBuilder::new(Heap::new(self.symbols.clone()));
        let bound = builder.bind(value);
        let ir_query = validator_query(&mut terms, &Symbol::MOD, bound);
        SharedQuery::from(builder.finish(&terms, &ir_query))
    }

    pub fn deserialize_validated<'de, D: Deserializer<'de>>(
        &mut self,
        deserializer: D,
    ) -> Result<T, Error> {
        let serializer = serde_json::value::Serializer;
        let json_value = serde_transcode::transcode(deserializer, serializer).unwrap();
        self.session.load(self.build_validator_query(&json_value));

        if self.session.resume() {
            Ok(serde_json::from_value(json_value)?)
        } else {
            failure::bail!("Failed to validate!");
        }
    }
}

whisper::knowledge_base! {
    fn foo_schema();

    // If TLS is disabled, we dgaf about the key/cert.
    valid Foo if
        Foo matches { tls_enabled: false | _ } in std::map::match;

    // If TLS is enabled, we want to be sure that key/cert are both `Some`.
    valid Foo if
        Foo matches {
            tls_enabled: true,
            tls: {
                cert: _,
                key: _,
            }
        } in std::map::match;

    // Eventually we'll have some stuff for error reporting in the stdlib.
    // There are a number of ways to get data out of Whisper in a way that's
    // convenient for aggregating errors, but for now imagine this is a println.
    valid Foo if
        "failed to validate!" Foo in extern,
        error "TODO: add some stdlib stuff so that we can report errors!";
}

#[derive(Debug, Clone, Deserialize)]
pub struct TlsConfig {
    cert: Option<String>,
    key: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FooConfig {
    tls: Option<TlsConfig>,
    tls_enabled: bool,
}

#[test]
fn validate_foo() -> Result<(), Error> {
    // First, we construct our validator from the knowledge base we made above.
    let mut schema = Schema::from_embedded(foo_schema);

    let import_point = schema.0.symbol_table().normalize(Name {
        root: Symbol::MOD,
        path: im::vector![Atom::from("std"), Atom::from("map")],
    });

    schema
        .0
        .to_mut()
        .import_serialized(&import_point, whisper_std::map());

    let mut validator = Validator::<FooConfig>::new(&schema);

    // Now we can try to validate a few configs! Normally you'd be reading things
    // in from a file or something but in the interest of simplicity here we'll
    // just `include_str!` them.
    let foo_config_a = include_str!("foo_schema/foo_ok.toml");
    let foo_config_b = include_str!("foo_schema/foo_bad.toml");
    let foo_config_c = include_str!("foo_schema/foo_also_ok.toml");

    // Cool... Let's try to deserialize these! We'll start with `foo_config_a`.
    let val = validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_a))?;
    println!("Valid: {:#?}", val);

    // Okay... so far so good, let's try `foo_config_b`.
    match validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_b)) {
        Ok(val) => println!("All good! Also valid: {:#?}", val),
        Err(err) => println!("Oh no! {}", err),
    }

    // Oops, that last one failed! Alright, last but not least, let's try `foo_config_c`.
    let val = validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_c))?;
    println!("Valid: {:#?}", val);

    // And that one passes with flying colors.

    Ok(())
}
