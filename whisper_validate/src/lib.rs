use ::{
    failure::Error,
    serde::{de::DeserializeOwned, Deserializer},
    serde_json::Value,
    whisper::{prelude::*, session::DebugHandler},
};

whisper::query! {
    fn validator_query(input: IrNode);

    valid #input;
}

#[derive(Debug)]
pub struct Validator {
    symbols: SymbolTable,
    session: Session<DebugHandler>,
}

impl Validator {
    pub fn from_str<S: AsRef<str> + ?Sized>(string: &S) -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let ir_kb = terms.parse_knowledge_base_str(string).expect("oops");
        let kb = whisper::trans::knowledge_base(&terms, &ir_kb);

        Self {
            symbols: symbols.clone(),
            session: Session::new(symbols, kb.into()),
        }
    }

    pub fn from_embedded(embedded: fn(&mut IrTermGraph) -> IrKnowledgeBase) -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let ir_kb = embedded(&mut terms);
        let kb = whisper::trans::knowledge_base(&terms, &ir_kb);

        Self {
            symbols: symbols.clone(),
            session: Session::new(symbols, kb.into()),
        }
    }

    fn build_validator_query(&self, value: &Value) -> SharedQuery {
        let mut terms = IrTermGraph::new(self.symbols.clone());
        let mut builder = QueryBuilder::new(Heap::new(self.symbols.clone()));
        let bound = builder.bind(value);
        let ir_query = validator_query(&mut terms, &Symbol::MOD, bound);
        SharedQuery::from(builder.finish(&terms, &ir_query))
    }

    pub fn deserialize_validated<'de, T, D>(&mut self, deserializer: D) -> Result<T, Error>
    where
        T: DeserializeOwned,
        D: Deserializer<'de>,
    {
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
