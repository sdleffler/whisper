use crate::{
    graph::{
        fold::{self, Fold},
        term::{IrGoal, IrRef, IrTermGraph},
        IrError,
    },
    parse::{self, ParseError, QuasiVar},
    symbol::Path,
    Ident, Name, Scope, Symbol, SymbolTable, SymbolTableInner,
};

use ::{
    im::{HashMap, HashSet, Vector},
    std::{
        collections::BTreeSet,
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

    root: Symbol,
}

impl IrModule {
    pub fn symbol(&self, ident: impl Into<Ident>) -> Symbol {
        self.root.get_scope().symbol(ident)
    }

    pub fn path(&self, path: impl Into<Path>) -> Name {
        Name {
            root: self.root.clone(),
            path: path.into(),
        }
    }

    pub fn get_root(&self) -> &Symbol {
        &self.root
    }

    pub fn get_scope(&self) -> Scope {
        self.root.get_scope()
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

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn new_named_module_with_root(&mut self, root: Symbol) -> IrModuleRef {
        let handle = IrModuleRef(self.modules.len());
        self.modules.push_back(IrModule {
            entries: Vector::new(),
            exports: HashSet::new(),
            imports: HashMap::new(),
            root,
        });
        handle
    }

    pub fn new_named_module<S: AsRef<str>>(&mut self, name: S) -> IrModuleRef {
        let mut symbols_write = self.symbols.write();

        let handle = IrModuleRef(self.modules.len());
        let scope = symbols_write.insert_unique_scope(name.as_ref());
        let root = symbols_write.get_scope_metadata(scope).name.clone();
        self.modules.push_back(IrModule {
            entries: Vector::new(),
            exports: HashSet::new(),
            imports: HashMap::new(),
            root: root.clone(),
        });
        handle
    }

    pub fn new_anonymous_module(&mut self) -> IrModuleRef {
        self.new_named_module("")
    }

    pub fn parse_str<S: AsRef<str>>(
        &mut self,
        terms: &mut IrTermGraph,
        string: S,
    ) -> Result<IrModuleRef, IrError> {
        let modules_owned = mem::replace(self, IrKnowledgeBase::new(self.symbol_table().clone()));
        let terms_owned = mem::replace(terms, IrTermGraph::new(terms.symbol_table().clone()));
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

struct LinkModules<'ctx> {
    symbols: &'ctx mut SymbolTableInner,
    modules: &'ctx mut IrKnowledgeBase,
    imports: &'ctx IrImports,
    new_root_scope: Scope,
    memo: HashMap<Name, Name>,
}

impl<'ctx> Fold for LinkModules<'ctx> {
    fn fold_name(&mut self, _ir_graph: &mut IrTermGraph, name: &Name) -> Option<Name> {
        if let Some(cached) = self.memo.get(name) {
            return Some(cached.clone());
        }

        let sym = self.symbols.normalize(name);
        //println!("Folding symbol {} => {:?}", sym, self.imports.get(&sym));
        if let Some(import) = self.imports.get(&sym) {
            let name = self.modules[import.from].path(import.path.clone());
            let foreign = self.symbols.normalize(name.clone());
            let local = self.new_root_scope.symbol(sym.ident());
            self.symbols.alias(local.clone(), foreign);

            let local_name = Name::from(local);
            self.memo.insert(name, local_name.clone());

            //println!("Folded: {}", local_name);

            Some(local_name)
        } else {
            None
        }
    }
}

impl IrKnowledgeBase {
    fn do_link(
        &mut self,
        terms: &mut IrTermGraph,
        root: IrModuleRef,
        imports: &IrImports,
        new_module: IrModuleRef,
    ) {
        let mut entries = self[root].entries.clone();
        let mut exports = self[root].exports.clone();

        let new_root_scope = self[new_module].get_scope();
        let symbols = terms.symbol_table().clone();
        let mut link_modules = LinkModules {
            symbols: &mut *symbols.write(),
            modules: self,
            imports,
            new_root_scope,
            memo: Default::default(),
        };

        for entry_mut in entries.iter_mut() {
            if let Some(new_entry) = fold::fold_module_entry(&mut link_modules, terms, &*entry_mut)
            {
                *entry_mut = new_entry;
            }
        }

        for export_mut in exports.iter_mut() {
            if let Some(new_export) = fold::fold_name(&mut link_modules, terms, &*export_mut) {
                *export_mut = new_export;
            }
        }

        let import_module_set = imports
            .values()
            .map(|import| import.from)
            .collect::<BTreeSet<_>>();

        let combined_entries = import_module_set
            .into_iter()
            .map(|m| self[m].entries.clone())
            .sum::<Vector<_>>()
            + entries;

        self[new_module].entries = combined_entries;
        self[new_module].exports = exports;
    }

    // pub fn link_modules(
    //     &mut self,
    //     terms: &mut IrTermGraph,
    //     root: IrModuleRef,
    //     imports: &IrImports,
    // ) -> IrModuleRef {
    //     let module = self.new_anonymous_module();
    //     self.do_link(terms, root, imports, module);
    //     module
    // }

    pub fn link(&mut self, terms: &mut IrTermGraph, module: IrModuleRef) {
        let imports = self[module].imports.clone();
        for (_, import) in &imports {
            if !self[import.from].is_closed() {
                self.link(terms, import.from);
            }
        }

        //println!("imports: {:?}", imports);
        self.do_link(terms, module, &imports, module);
        //println!("linked!");
    }
}

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
        let root = self.root.to_string();
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
