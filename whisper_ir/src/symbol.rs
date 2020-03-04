use ::{
    failure::Fail,
    im::Vector,
    parking_lot::RwLock,
    std::{
        collections::HashMap,
        fmt,
        marker::PhantomPinned,
        mem,
        ops::{Deref, Index},
        ptr,
        sync::Arc,
    },
};

whisper_codegen::reserved_symbols! {
    symbol::Atom, atom!;

    // Root scopes. It's very important these be in order because currently
    // the order defines their IDs. Here, that's 0 through 3, inclusive.
    super pub "PUBLIC",
    super pub "INTERNAL",
    super pub "LOCAL",
    super mod "mod",

    // Other reserved scopes
    mod "super",
    mod "std",
    mod "self",

    // Keywords (only allowed in certain positions, parse error otherwise,
    // cause special goal behavior)
    pub "let",
    pub "is",
    pub "try",
    pub "cut",

    // Foobar corgefred.
    pub "println",

    // List sugar
    internal "list_nil",

    // Map sugar (Cons2)
    internal "map_nil",

    // Arithmetic operators
    internal "add",
    internal "sub",
    internal "mul",
    internal "div",
    internal "neg",

    // Types used for dynamic checking
    internal "u32",
    internal "i32",
    internal "f32",

    // (Otherwise unnecessary) constants for Rust interop
    pub "true",
    pub "false",
    pub "()",
    pub "Some",
    pub "None",
    pub "i64",
    pub "u64",
    pub "f64",
    pub "char",
    pub "str",
    pub "byte array",

    // // Symbols used for serializing/deserializing Serde-enabled types.
    // schema "i64",
    // schema "u64",
    // schema "f64",
    // schema "char",
    // schema "str",
    // schema "byteslice",
}

#[macro_use]
pub mod ident;

pub use ident::Ident;

#[derive(Debug, Fail)]
pub enum NameError {
    #[fail(
        display = "a scope with identifier `{}` already exists in this symbol table",
        _0
    )]
    ScopeAlreadyDefined(SymbolIndex),
}

/// Identifies a lexical scope.
// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Scope(SymbolIndex, usize);

// impl Scope {
//     pub const PUBLIC: Scope = Scope(Symbol::PUBLIC, 0);
//     pub const INTERNAL: Scope = Scope(Symbol::INTERNAL, 0);

//     /// Special scope used during compilation to represent a term which is
//     /// only available in the local scope.
//     pub const LOCAL: Scope = Scope(Symbol::LOCAL, 0);

//     pub const MOD: Scope = Scope(Symbol::MOD, 0);

//     const NUM_RESERVED: u64 = 4;

//     pub const fn is_reserved(&self) -> bool {
//         self.0 < Self::NUM_RESERVED
//     }

//     pub const fn get_id(&self) -> u64 {
//         self.0
//     }

//     #[doc(hidden)]
//     pub const fn from_id(id: u64) -> Self {
//         Scope(id, 0)
//     }

//     pub fn symbol(&self, ident: impl Into<Ident>) -> Symbol {
//         Symbol::new(ident.into(), *self)
//     }
// }

// impl fmt::Display for Scope {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match *self {
//             Scope::PUBLIC => write!(f, "PUBLIC"),
//             Scope::INTERNAL => write!(f, "INTERNAL"),
//             Scope::LOCAL => write!(f, "LOCAL"),
//             Scope::MOD => write!(f, "MOD"),
//             Scope(other, _table) => write!(f, "{}", other),
//         }
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolTableId(usize);

impl SymbolTableId {
    pub const NULL: SymbolTableId = SymbolTableId(0);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIndex(pub usize, pub SymbolTableId);

impl fmt::Display for SymbolIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_builtin() {
            BUILTINS_IDX_TO_SYM[self.0].ident.fmt(f)
        } else {
            self.0.fmt(f)
        }
    }
}

impl SymbolIndex {
    pub fn is_reserved(&self) -> bool {
        self.0 < NUM_RESERVED && self.1 == SymbolTableId::NULL
    }

    pub fn is_builtin(&self) -> bool {
        self.0 < NUM_BUILTINS && self.1 == SymbolTableId::NULL
    }
}

/// A scoped identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    ident: Ident,

    /// Scoped identifiers have parent scopes. The tree ends when
    /// we hit a "builtin" scope.
    parent: SymbolIndex,
}

impl Symbol {
    pub const fn new(ident: Ident, parent: SymbolIndex) -> Self {
        Self { ident, parent }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn parent(&self) -> SymbolIndex {
        self.parent
    }

    pub fn is_builtin(&self) -> bool {
        BUILTINS_SYM_TO_IDX.contains_key(self)
    }
}

pub struct DisplaySymbol<'a> {
    sym: &'a Symbol,
    table: &'a SymbolTableInner,
}

impl<'a> fmt::Display for DisplaySymbol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sym.parent.is_reserved() {
            if self.sym.parent == SymbolIndex::PUBLIC {
                write!(f, "{}", self.sym.ident)
            } else if self.sym.parent == SymbolIndex::MOD {
                write!(f, "::{}", self.sym.ident)
            } else {
                write!(f, "{}${}", self.sym.ident, self.sym.parent)
            }
        } else {
            write!(
                f,
                "{}::{}",
                self.table.display(self.sym.parent),
                self.sym.ident,
            )
        }
    }
}

impl<'a> From<&'a str> for Symbol {
    fn from(s: &'a str) -> Self {
        Self::from(Atom::from(s))
    }
}

impl From<Atom> for Symbol {
    fn from(atom: Atom) -> Self {
        Self {
            ident: Ident::from(atom),
            parent: SymbolIndex::PUBLIC,
        }
    }
}

impl<'a> From<&'a Symbol> for Symbol {
    fn from(sym: &'a Symbol) -> Symbol {
        sym.clone()
    }
}

impl From<Symbol> for Atom {
    fn from(sym: Symbol) -> Atom {
        sym.ident.into()
    }
}

impl<'a> From<&'a Symbol> for Atom {
    fn from(sym: &'a Symbol) -> Atom {
        sym.ident.atom().clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name {
    pub root: SymbolIndex,
    pub path: Path,
}

impl From<SymbolIndex> for Name {
    fn from(root: SymbolIndex) -> Self {
        Self {
            root,
            path: Default::default(),
        }
    }
}

impl From<Ident> for Name {
    fn from(ident: Ident) -> Self {
        Self {
            root: SymbolIndex::PUBLIC,
            path: im::vector![ident],
        }
    }
}

// seriously?
impl AsRef<Name> for Name {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<'a> From<&'a Ident> for Name {
    fn from(ident: &'a Ident) -> Self {
        Self::from(ident.clone())
    }
}

pub struct DisplayName<'a> {
    table: &'a SymbolTableInner,
    name: &'a Name,
}

impl<'a> fmt::Display for DisplayName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.table.display(self.name.root))?;
        for segment in &self.name.path {
            write!(f, "::{}", segment)?;
        }
        Ok(())
    }
}

pub type Path = Vector<Ident>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Var {
    Named(Ident),
    Anonymous,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Var::Named(name) => name.fmt(f),
            Var::Anonymous => "_".fmt(f),
        }
    }
}

/// Symbol tables allow us to use integers to identify atoms in our heap, and
/// merge the namespaces of multiple heaps so that integer identifiers that
/// might be different in each heap but refer to the same symbol are reconciled.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    inner: Arc<RwLock<SymbolTableInner>>,
    id: SymbolTableId,
}

impl PartialEq for SymbolTable {
    fn eq(&self, rhs: &Self) -> bool {
        ptr::eq(&*self.inner, &*rhs.inner)
    }
}

impl Eq for SymbolTable {}

impl Deref for SymbolTable {
    type Target = RwLock<SymbolTableInner>;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

#[derive(Debug)]
pub struct SymbolTableInner {
    id: SymbolTableId,
    _phantom: PhantomPinned,

    builtins_to_symbol: &'static [Symbol],
    to_symbol: Vec<Symbol>,

    builtins_to: &'static HashMap<Symbol, SymbolIndex>,
    to: HashMap<Symbol, SymbolIndex>,
}

impl SymbolTable {
    /// Make an empty symbol table.
    pub fn new() -> Self {
        let inner = SymbolTableInner {
            id: SymbolTableId(0),
            _phantom: PhantomPinned,

            builtins_to_symbol: &*BUILTINS_IDX_TO_SYM,
            to_symbol: Default::default(),

            builtins_to: &*BUILTINS_SYM_TO_IDX,
            to: Default::default(),
        };

        let arc = Arc::new(RwLock::new(inner));
        let arc_ptr = (&*arc) as *const _;
        let id = SymbolTableId(arc_ptr as usize);
        arc.write().id = id;

        Self { inner: arc, id }
    }

    pub fn id(&self) -> SymbolTableId {
        self.id
    }

    /// WARNING: This function assumes that you know this symbol is either
    /// a builtin or is in this table! If you don't know this then this function
    /// may cause strange behavior or panics elsewhere in your code!
    pub fn index(&self, i: usize) -> SymbolIndex {
        if i < NUM_BUILTINS {
            SymbolIndex(i, SymbolTableId::NULL)
        } else {
            SymbolIndex(i, self.id())
        }
    }
}

impl Index<SymbolIndex> for SymbolTableInner {
    type Output = Symbol;

    fn index(&self, idx: SymbolIndex) -> &Self::Output {
        self.lookup(idx).unwrap()
    }
}

impl SymbolTableInner {
    pub fn valid(&self, sym: SymbolIndex) -> bool {
        self.id == sym.1 || SymbolTableId::NULL == sym.1
    }

    pub fn lookup(&self, idx: SymbolIndex) -> Option<&Symbol> {
        if !self.valid(idx) {
            return None;
        }

        let sym = if idx.is_builtin() {
            &self.builtins_to_symbol[idx.0]
        } else {
            &self.to_symbol[idx.0 - NUM_BUILTINS]
        };

        if sym.ident == ident_internal!("super") {
            self.lookup(sym.parent)
        } else {
            Some(sym)
        }
    }

    pub fn lookup_name(&self, symbol: SymbolIndex) -> Name {
        let mut path = Vector::new();
        let mut root = symbol;
        loop {
            let scope_name = self[root].parent();

            if root == scope_name {
                break;
            }

            let prev_root = mem::replace(&mut root, scope_name);
            path.push_front(self[prev_root].ident().clone());
        }

        Name { path, root }
    }

    pub fn insert(&mut self, pre_ident: impl Into<Ident>) -> SymbolIndex {
        self.insert_with_parent(pre_ident, SymbolIndex::PUBLIC)
    }

    pub fn insert_with_parent(
        &mut self,
        pre_ident: impl Into<Ident>,
        parent: SymbolIndex,
    ) -> SymbolIndex {
        assert!(self.valid(parent));

        let ident = pre_ident.into();
        let symbol = Symbol { ident, parent };

        if let Some(idx) = self.get(&symbol) {
            return idx;
        }

        let idx = SymbolIndex(
            self.to_symbol.len() + self.builtins_to_symbol.len(),
            self.id,
        );

        self.to_symbol.push(symbol.clone());
        self.to.insert(symbol, idx);
        idx
    }

    pub fn insert_gensym_with_parent(&mut self, parent: SymbolIndex) -> SymbolIndex {
        assert!(self.valid(parent));
        self.insert_with_parent(Ident::gensym(), parent)
    }

    pub fn insert_name(&mut self, pre_name: impl AsRef<Name>) -> SymbolIndex {
        let name = pre_name.as_ref();
        name.path.iter().fold(name.root, |prev, seg| {
            if seg == &ident_internal!("super") {
                self[prev].parent()
            } else {
                self.insert_with_parent(seg, prev)
            }
        })
    }

    pub fn get(&self, symbol: &Symbol) -> Option<SymbolIndex> {
        if symbol.is_builtin() {
            Some(self.builtins_to[symbol])
        } else {
            self.to.get(symbol).copied()
        }
    }

    pub fn get_name(&self, name: &Name) -> Option<SymbolIndex> {
        name.path.iter().fold(Some(name.root), |maybe_prev, seg| {
            maybe_prev.and_then(|prev| {
                if seg == &ident_internal!("super") {
                    self.lookup(prev).map(Symbol::parent)
                } else {
                    self.get(&Symbol::new(seg.clone(), prev))
                }
            })
        })
    }

    pub fn display(&self, symbol: SymbolIndex) -> DisplaySymbol {
        DisplaySymbol {
            table: self,
            sym: &self[symbol],
        }
    }

    pub fn display_name<'a>(&'a self, name: &'a Name) -> DisplayName<'a> {
        DisplayName { table: self, name }
    }

    // pub fn try_scope<S>(&self, scope_sym: S) -> Option<SymbolIndex>
    // where
    //     S: Into<Symbol> + std::borrow::Borrow<Symbol>,
    // {
    //     if scope_sym.borrow().scope().is_reserved() {
    //         if let Some(scope) = self.reserved_scopes.get(scope_sym.borrow()) {
    //             return Some(*scope);
    //         }
    //     }

    //     if scope_sym.borrow().ident() == &ident_internal!("super") {
    //         let name = self.get_scope_metadata(scope_sym.borrow().scope()).name;
    //         return Some(self.lookup(name).scope());
    //     }

    //     let sym = self.resolve_symbol(scope_sym.borrow());
    //     self.scopes.get(&sym).copied()
    // }

    // pub fn scope(&mut self, scope_name: SymbolIndex) -> Scope {
    //     let scope_sym = self.lookup(scope_name);

    //     if scope_sym.scope().is_reserved() {
    //         if let Some(scope) = self.reserved_scopes.get(scope_name) {
    //             return *scope;
    //         }
    //     }

    //     if scope_name == Symbol::MOD_SUPER {
    //         return scope_sym.scope();
    //     }

    //     match self.scopes.get(&scope_name) {
    //         Some(scope) => *scope,
    //         None => {
    //             let new_scope = Scope(self.scopes.len() as u64 + Scope::NUM_RESERVED, self.pinned);
    //             self.scopes.insert(scope_name, new_scope);
    //             self.scope_metadata.push(ScopeMetadata { name: scope_name });
    //             new_scope
    //         }
    //     }
    // }

    // pub fn insert_unique_scope(&mut self, name: impl Into<Atom>) -> Scope {
    //     let new_scope = Scope(self.scopes.len() as u64 + Scope::NUM_RESERVED, self.pinned);
    //     let scope_name = new_scope.symbol(name.into());
    //     let sym = self.resolve_symbol(&scope_name);
    //     self.scopes.insert(sym, new_scope);
    //     self.scope_metadata.push(ScopeMetadata { name: sym });
    //     new_scope
    // }

    // pub fn get_scope_metadata(&self, scope: Scope) -> &ScopeMetadata {
    //     if scope.is_reserved() {
    //         &self.reserved_scope_metadata[scope.0 as usize]
    //     } else {
    //         assert!(self.is_owner_of(scope));
    //         &self.scope_metadata[(scope.0 - Scope::NUM_RESERVED) as usize]
    //     }
    // }

    // fn resolve_symbol(&mut self, symbol: &Symbol) -> SymbolIndex {
    //     assert!(symbol.scope().is_reserved() || self.is_owner_of(symbol.scope()));

    //     // If it's a reserved symbol, try the builtin hashmap first.
    //     if symbol.scope.is_reserved() {
    //         if let Some(idx) = self.builtins_to.get(symbol).copied() {
    //             return idx;
    //         }
    //     }

    //     let to = &mut self.to;
    //     let to_symbol = &mut self.to_symbol;

    //     *to.entry(symbol.clone()).or_insert_with(|| {
    //         let i = to_symbol.len() + NUM_BUILTINS;
    //         to_symbol.push(symbol.clone());
    //         SymbolIndex(i)
    //     })
    // }

    // fn try_resolve_symbol(&self, symbol: &Symbol) -> Option<SymbolIndex> {
    //     assert!(symbol.scope().is_reserved() || self.is_owner_of(symbol.scope()));

    //     // If it's a reserved symbol, try the builtin hashmap first.
    //     if symbol.scope.is_reserved() {
    //         if let Some(idx) = self.builtins_to.get(symbol).copied() {
    //             return Some(idx);
    //         }
    //     }

    //     self.to.get(&symbol).copied()
    // }

    // fn resolve_name(&mut self, name: Name) -> Symbol {
    //     name.path.into_iter().fold(name.root, |acc, seg| {
    //         let scope = self.scope(acc);
    //         Symbol::new(seg, scope)
    //     })
    // }

    // fn try_resolve_name(&self, name: Name) -> Option<Symbol> {
    //     name.path
    //         .into_iter()
    //         .fold(Some(name.root), |maybe_acc, seg| {
    //             let scope = self.try_scope(maybe_acc?)?;
    //             Some(Symbol::new(seg, scope))
    //         })
    // }

    // /// Alias one symbol to another.
    // ///
    // /// Panics if the symbol being aliased is already an alias to
    // /// a different symbol.
    // pub fn alias(&mut self, alias: Symbol, original: Symbol) {
    //     assert_eq!(
    //         {
    //             let resolved = self.resolve(&alias);
    //             &self.lookup(resolved)
    //         },
    //         &alias
    //     );

    //     let index = self.resolve_symbol(&original);
    //     self.to.insert(alias, index);
    // }

    // pub fn lookup(&self, idx: SymbolIndex) -> Symbol {
    //     if idx.0 < NUM_BUILTINS {
    //         self.builtins_to_symbol[idx.0].clone()
    //     } else {
    //         self.to_symbol[idx.0 - NUM_BUILTINS].clone()
    //     }
    // }

    // pub fn lookup_full(&self, idx: SymbolIndex) -> Name {
    //     self.get_full_name(self.lookup(idx))
    // }

    // fn get_full_name(&self, symbol: Symbol) -> Name {
    //     let mut path = Vector::new();
    //     let mut root = symbol;
    //     loop {
    //         let scope_name = self.get_scope_metadata(root.scope).name.clone();

    //         if root == scope_name {
    //             break;
    //         }

    //         let prev_root = mem::replace(&mut root, scope_name);
    //         path.push_front(prev_root.ident);
    //     }

    //     Name { path, root }
    // }

    // pub fn try_lookup(&self, idx: SymbolIndex) -> Option<Symbol> {
    //     if idx.0 < NUM_BUILTINS {
    //         Some(self.builtins_to_symbol[idx.0].clone())
    //     } else {
    //         self.to_symbol.get(idx.0 - NUM_BUILTINS).cloned()
    //     }
    // }

    // pub fn contains_scope(&self, scope: Scope) -> bool {
    //     scope.is_reserved() || self.is_owner_of(scope)
    // }

    // pub fn insert_anonymous_scope(&mut self) -> Scope {
    //     self.insert_unique_scope(atom!(""))
    // }

    // pub fn resolve<I: Identifier>(&mut self, ident: I) -> SymbolIndex {
    //     ident.resolve(self)
    // }

    // pub fn try_resolve<I: Identifier>(&self, ident: I) -> Option<SymbolIndex> {
    //     ident.try_resolve(self)
    // }

    // pub fn normalize<I: Identifier>(&mut self, ident: I) -> Symbol {
    //     ident.to_symbol(self)
    // }

    // pub fn normalize_full<I: Identifier>(&mut self, ident: I) -> Name {
    //     let symbol = ident.to_symbol(self);
    //     self.get_full_name(symbol)
    // }

    /// Iterate over all symbols in the table, including builtins, in order.
    pub fn iter(&self) -> SymbolIter {
        SymbolIter {
            inner: self,
            index: 0,
        }
    }

    /// Iterate over all symbols in the table, excluding builtins, in order.
    pub fn iter_non_builtins(&self) -> SymbolIter {
        SymbolIter {
            inner: self,
            index: NUM_BUILTINS,
        }
    }
}

// pub trait Identifier: Clone + Sized {
//     fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex;
//     fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex>;

//     fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol;
// }

// impl Identifier for SymbolIndex {
//     fn resolve(self, _symbol_table: &mut SymbolTableInner) -> SymbolIndex {
//         self
//     }

//     fn try_resolve(self, _symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
//         Some(self)
//     }

//     fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol {
//         symbol_table[self].clone()
//     }
// }

// impl Identifier for Symbol {
//     fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
//         symbol_table.resolve_symbol(&self)
//     }

//     fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
//         symbol_table.try_resolve_symbol(&self)
//     }

//     fn to_symbol(self, _symbol_table: &mut SymbolTableInner) -> Symbol {
//         self
//     }
// }

// impl<'a> Identifier for &'a Symbol {
//     fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
//         symbol_table.resolve_symbol(self)
//     }

//     fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
//         symbol_table.try_resolve(self)
//     }

//     fn to_symbol(self, _symbol_table: &mut SymbolTableInner) -> Symbol {
//         self.clone()
//     }
// }

// impl Identifier for Name {
//     fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
//         let symbol = symbol_table.resolve_name(self);
//         symbol.resolve(symbol_table)
//     }

//     fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
//         let symbol = symbol_table.try_resolve_name(self)?;
//         symbol.try_resolve(symbol_table)
//     }

//     fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol {
//         let index = self.resolve(symbol_table);
//         symbol_table.lookup(index)
//     }
// }

// impl<'a> Identifier for &'a Name {
//     fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
//         self.clone().resolve(symbol_table)
//     }

//     fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
//         self.clone().try_resolve(symbol_table)
//     }

//     fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol {
//         self.clone().to_symbol(symbol_table)
//     }
// }

#[derive(Debug)]
pub struct SymbolIter<'a> {
    inner: &'a SymbolTableInner,
    index: usize,
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = (SymbolIndex, &'a Symbol);

    fn next(&mut self) -> Option<Self::Item> {
        let maybe_sym = if self.index < NUM_BUILTINS + self.inner.to_symbol.len() {
            if self.index < NUM_BUILTINS {
                Some(SymbolIndex(self.index, SymbolTableId::NULL))
            } else {
                Some(SymbolIndex(self.index, self.inner.id))
            }
        } else {
            None
        };

        if let Some(sym_idx) = maybe_sym {
            self.index += 1;
            let sym = &self.inner[sym_idx];
            Some((sym_idx, sym))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use im::vector;

    #[test]
    fn super_scope() {
        let symbols = SymbolTable::new();
        let mut symbols_mut = symbols.write();
        let list_module = symbols_mut.insert_with_parent("list", SymbolIndex::MOD);
        let map_module = symbols_mut.insert_with_parent("map", list_module);
        let elem_module = symbols_mut.insert_with_parent("elem", list_module);

        let name = Name {
            root: map_module,
            path: vector![ident_internal!("super"), Ident::from("elem")],
        };

        let inserted = symbols_mut.get_name(&name).unwrap();

        assert_eq!(
            inserted,
            elem_module,
            "super scope not properly resolved: {} vs {}",
            symbols_mut.display(inserted),
            symbols_mut.display(elem_module),
        );
    }
}
