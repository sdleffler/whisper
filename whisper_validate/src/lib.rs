use ::{
    failure::Error,
    serde::{de::DeserializeOwned, Deserialize, Deserializer},
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
