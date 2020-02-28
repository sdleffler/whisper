use crate::{
    graph::{
        term::{IrGoal, IrRef, IrTermGraph},
        IrError,
    },
    parse::{self, ParseError, QuasiVar},
    symbol::Path,
    Name, Symbol, SymbolIndex, SymbolTable,
};

use ::{
    im::{HashMap, HashSet, Vector},
    std::{
        mem,
        ops::{Index, IndexMut},
    },
};

#[derive(Debug, Clone)]
pub struct IrRelation {
    pub head: IrRef,
    pub body: Vector<IrGoal>,
}

#[derive(Debug, Clone)]
pub enum IrModuleEntry {
    Relation(IrRelation),
    Quasi(QuasiVar),
}

impl IrModuleEntry {
    pub fn unwrap(self) -> IrRelation {
        match self {
            IrModuleEntry::Relation(ir_relation) => ir_relation,
            IrModuleEntry::Quasi(quasi) => panic!("IrModuleEntry: unexpected quasi! {:?}", quasi),
        }
    }

    pub fn unwrap_ref(&self) -> &IrRelation {
        match self {
            IrModuleEntry::Relation(ir_relation) => ir_relation,
            IrModuleEntry::Quasi(quasi) => panic!("IrModuleEntry: unexpected quasi! {:?}", quasi),
        }
    }
}

/// A knowledge base is just a collection of relations. Until we need more info
/// along with the contents, it can just be a type alias.
#[derive(Debug, Clone)]
pub struct IrModule {
    pub entries: Vector<IrModuleEntry>,
    pub exports: HashSet<Name>,
    pub imports: IrImports,

    root: SymbolIndex,
}

impl IrModule {
    pub fn path(&self, path: impl Into<Path>) -> Name {
        Name {
            root: self.root,
            path: path.into(),
        }
    }

    pub fn root(&self) -> SymbolIndex {
        self.root
    }

    /// A module is "closed" if it has no imports which need to be resolved.
    pub fn is_closed(&self) -> bool {
        self.imports.is_empty()
    }
}

impl Extend<IrModuleEntry> for IrModule {
    fn extend<T: IntoIterator<Item = IrModuleEntry>>(&mut self, iter: T) {
        self.entries.extend(iter);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IrModuleRef(usize);

#[derive(Debug, Clone)]
pub struct IrKnowledgeBase {
    modules: Vector<IrModule>,
    symbols: SymbolTable,
}

impl Index<IrModuleRef> for IrKnowledgeBase {
    type Output = IrModule;

    fn index(&self, idx: IrModuleRef) -> &Self::Output {
        &self.modules[idx.0]
    }
}

impl IndexMut<IrModuleRef> for IrKnowledgeBase {
    fn index_mut(&mut self, idx: IrModuleRef) -> &mut Self::Output {
        &mut self.modules[idx.0]
    }
}

impl IrKnowledgeBase {
    pub fn new(symbols: SymbolTable) -> Self {
        Self {
            modules: Vector::new(),
            symbols,
        }
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn module(&mut self, root: SymbolIndex) -> IrModuleRef {
        let handle = IrModuleRef(self.modules.len());
        self.modules.push_back(IrModule {
            entries: Vector::new(),
            exports: HashSet::new(),
            imports: HashMap::new(),
            root,
        });
        handle
    }

    pub fn parse_str<S: AsRef<str>>(
        &mut self,
        terms: &mut IrTermGraph,
        string: S,
    ) -> Result<IrModuleRef, IrError> {
        let modules_owned = mem::replace(self, IrKnowledgeBase::new(self.symbols().clone()));
        let terms_owned = mem::replace(terms, IrTermGraph::new(terms.symbols().clone()));
        parse::use_graphs(modules_owned, terms_owned);
        let out = syn::parse_str(string.as_ref()).map_err(|syn_err| {
            ParseError {
                src: string.as_ref().to_string(),
                syn_err,
            }
            .into()
        });
        let (modules_recovered, terms_recovered) = parse::swap_graphs();
        *self = modules_recovered;
        *terms = terms_recovered;
        out
    }

    pub fn iter(&self) -> impl Iterator<Item = &IrModule> {
        self.modules.iter()
    }
}

#[derive(Debug, Clone)]
pub struct IrImport {
    pub path: Path,
    pub from: IrModuleRef,
}

pub type IrImports = HashMap<Symbol, IrImport>;

#[cfg(test)]
use serde_json::{json, Value as JsonValue};

#[cfg(test)]
impl IrRelation {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        let head_json = self.head.to_json(terms);
        let body_json = self
            .body
            .iter()
            .map(|t| t.to_json(terms))
            .collect::<JsonValue>();
        json! {{
            "head": head_json,
            "body": body_json,
        }}
    }
}

#[cfg(test)]
impl IrModuleEntry {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        match self {
            IrModuleEntry::Relation(rel) => rel.to_json(terms),
            IrModuleEntry::Quasi(qv) => json!({"Quasi": qv.0.to_string()}),
        }
    }
}

#[cfg(test)]
impl IrModuleRef {
    pub fn to_json(&self, terms: &IrTermGraph, kb: &IrKnowledgeBase) -> JsonValue {
        kb[*self].to_json(terms)
    }
}

#[cfg(test)]
impl IrModule {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        let root = terms.symbols().read().display(self.root).to_string();
        let entries = self
            .entries
            .iter()
            .map(|entry| entry.to_json(terms))
            .collect::<JsonValue>();
        json! {{
            "root": root,
            "entries": entries,
        }}
    }
}

#[cfg(test)]
impl IrKnowledgeBase {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        let modules = self
            .modules
            .iter()
            .map(|m| m.to_json(terms))
            .collect::<JsonValue>();

        json! {{
            "modules": modules,
        }}
    }
}
