use ::{
    lazy_static::lazy_static,
    whisper::{
        knowledge_base::{KnowledgeBase, SerializedKnowledgeBase},
        prelude::*,
    },
};

pub fn list() -> &'static SerializedKnowledgeBase {
    lazy_static! {
        static ref KB: SerializedKnowledgeBase = {
            let kb_bytes = include_bytes!(concat!(env!("OUT_DIR"), "/list.kb"));
            bincode::deserialize_from(&mut &kb_bytes[..]).expect("this must not fail!")
        };
    };

    &KB
}

pub fn map() -> &'static SerializedKnowledgeBase {
    lazy_static! {
        static ref KB: SerializedKnowledgeBase = {
            let kb_bytes = include_bytes!(concat!(env!("OUT_DIR"), "/map.kb"));
            bincode::deserialize_from(&mut &kb_bytes[..]).expect("this must not fail!")
        };
    };

    &KB
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_no_bincode_errors() {
        let symbols = SymbolTable::new();
        let _ = list().into_knowledge_base_with_root(symbols, &Symbol::MOD);
    }

    #[test]
    fn map_no_bincode_errors() {
        let symbols = SymbolTable::new();
        let _ = map().into_knowledge_base_with_root(symbols, &Symbol::MOD);
    }
}
