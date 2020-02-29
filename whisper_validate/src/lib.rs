use ::{
    failure::Error,
    serde::{de::DeserializeOwned, Deserializer},
    serde_json::Value,
    whisper::prelude::*,
};

whisper::query! {
    fn validator_query(input: IrNode);

    valid #input;
}

#[derive(Debug)]
pub struct Validator {
    symbols: SymbolTable,
    module_cache: ModuleCache,
    knowledge_base: KnowledgeBase,
    query: Query,
    session: Session<()>,
}

impl Validator {
    pub fn from_str<S: AsRef<str> + ?Sized>(string: &S) -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let ir_kb = terms.parse_knowledge_base_str(string).expect("oops");
        let knowledge_base = whisper::trans::knowledge_base(&terms, &ir_kb);
        let mut module_cache = ModuleCache::new();
        module_cache.init(&knowledge_base);

        Self {
            symbols: symbols.clone(),
            module_cache,
            knowledge_base,
            session: Session::new(symbols.clone()),
            query: Query::new(symbols),
        }
    }

    pub fn from_embedded(embedded: fn(&mut IrTermGraph) -> IrKnowledgeBase) -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let ir_kb = embedded(&mut terms);
        let knowledge_base = whisper::trans::knowledge_base(&terms, &ir_kb);
        let mut module_cache = ModuleCache::new();
        module_cache.init(&knowledge_base);

        Self {
            symbols: symbols.clone(),
            module_cache,
            knowledge_base,
            session: Session::new(symbols.clone()),
            query: Query::new(symbols),
        }
    }

    fn initialize_query(&mut self, value: &Value) {
        let mut terms = IrTermGraph::new(self.symbols.clone());
        self.query.clear();
        let mut builder = QueryBuilder::from(&mut self.query);
        let bound = builder.bind(value);
        let ir_query = validator_query(&mut terms, SymbolIndex::MOD, bound);
        builder.push(&terms, &ir_query);
        builder.finish();
    }

    pub fn deserialize_validated<'de, T, D>(&mut self, deserializer: D) -> Result<T, Error>
    where
        T: DeserializeOwned,
        D: Deserializer<'de>,
    {
        let serializer = serde_json::value::Serializer;
        let json_value = serde_transcode::transcode(deserializer, serializer).unwrap();
        self.initialize_query(&json_value);
        self.session.load_with_extern_state_and_reuse_query(
            &mut self.query,
            &self.module_cache,
            (),
        );

        if self
            .session
            .resume(&mut self.module_cache, &self.knowledge_base)
        {
            Ok(serde_json::from_value(json_value)?)
        } else {
            failure::bail!("Failed to validate!");
        }
    }
}
