use ::{
    failure::Fail,
    im::Vector,
    parking_lot::RwLock,
    std::{collections::HashMap, fmt, marker::PhantomPinned, mem, ops::Deref, ptr, sync::Arc},
};

whisper_codegen::reserved_symbols! {
    symbol::Atom, atom!;

    // Reserved scopes
    pub "PUBLIC",
    pub "INTERNAL",
    pub "LOCAL",
    mod "mod",
    mod "super",
    mod "std",
    mod "self",

    // Keywords (only allowed in certain positions, parse error otherwise,
    // cause special goal behavior)
    pub "let",
    pub "is",
    pub "try",
    pub "cut",

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
    ScopeAlreadyDefined(Symbol),
}

/// Identifies a lexical scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(u64, usize);

unsafe impl Send for Scope {}
unsafe impl Sync for Scope {}

impl Scope {
    pub const PUBLIC: Scope = Scope(0, 0);
    pub const INTERNAL: Scope = Scope(1, 0);

    /// Special scope used during compilation to represent a term which is
    /// only available in the local scope.
    pub const LOCAL: Scope = Scope(2, 0);

    pub const MOD: Scope = Scope(3, 0);

    const NUM_RESERVED: u64 = 4;

    pub const fn is_reserved(&self) -> bool {
        self.0 < Self::NUM_RESERVED
    }

    pub const fn get_id(&self) -> u64 {
        self.0
    }

    #[doc(hidden)]
    pub const fn from_id(id: u64) -> Self {
        Scope(id, 0)
    }

    pub fn symbol(&self, ident: impl Into<Ident>) -> Symbol {
        Symbol::new(ident.into(), *self)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Scope::PUBLIC => write!(f, "PUBLIC"),
            Scope::INTERNAL => write!(f, "INTERNAL"),
            Scope::LOCAL => write!(f, "LOCAL"),
            Scope::MOD => write!(f, "MOD"),
            Scope(other, _table) => write!(f, "{}", other),
        }
    }
}

/// A scoped identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    ident: Ident,

    /// An integer ID manipulated to prevent name collisions.
    /// This does have special values! TODO: iron this out and put those here
    scope: Scope,
}

impl Symbol {
    pub const fn new(ident: Ident, scope: Scope) -> Self {
        Self { ident, scope }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn get_scope(&self) -> Scope {
        self.scope
    }

    pub fn get_scope_ref(&self) -> &Scope {
        &self.scope
    }

    pub fn is_scoped(&self) -> bool {
        !self.scope.is_reserved()
    }

    pub fn is_scopable(&self) -> bool {
        !self.scope.is_reserved() || self.scope == Scope::PUBLIC
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.scope == Scope::PUBLIC {
            write!(f, "{}", self.ident)
        } else {
            write!(f, "{}${}", self.ident, self.scope)
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
            scope: Scope::PUBLIC,
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
    pub root: Symbol,
    pub path: Path,
}

impl From<Symbol> for Name {
    fn from(root: Symbol) -> Self {
        Self {
            root,
            path: Default::default(),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.root)?;
        for segment in &self.path {
            write!(f, "::{}", segment)?;
        }
        Ok(())
    }
}

pub type Path = Vector<Ident>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone)]
pub struct ScopeMetadata {
    pub name: Symbol,
}

lazy_static::lazy_static! {
    static ref RESERVED_SCOPE_METADATA: &'static [ScopeMetadata] = {
        Box::leak(Box::from(vec![
            ScopeMetadata {
                name: Symbol::PUBLIC,
            },
            ScopeMetadata {
                name: Symbol::INTERNAL,
            },
            ScopeMetadata {
                name: Symbol::LOCAL
            },
            ScopeMetadata {
                name: Symbol::MOD
            },
        ]))
    };

    static ref RESERVED_SCOPES: &'static HashMap<Symbol, Scope> = {
        let mut map = HashMap::new();
        map.insert(Symbol::PUBLIC, Scope::PUBLIC);
        map.insert(Symbol::INTERNAL, Scope::INTERNAL);
        map.insert(Symbol::LOCAL, Scope::LOCAL);
        map.insert(Symbol::MOD, Scope::MOD);
        Box::leak(Box::from(map))
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIndex(pub usize);

/// Symbol tables allow us to use integers to identify atoms in our heap, and
/// merge the namespaces of multiple heaps so that integer identifiers that
/// might be different in each heap but refer to the same symbol are reconciled.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub(crate) inner: Arc<RwLock<SymbolTableInner>>,
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
    pinned: usize,
    _phantom: PhantomPinned,

    builtins_to_symbol: &'static [Symbol],
    to_symbol: Vec<Symbol>,

    builtins_to_index: &'static HashMap<Symbol, SymbolIndex>,
    to_index: HashMap<Symbol, SymbolIndex>,

    reserved_scopes: &'static HashMap<Symbol, Scope>,
    reserved_scope_metadata: &'static [ScopeMetadata],
    scopes: HashMap<Symbol, Scope>,
    scope_metadata: Vec<ScopeMetadata>,
}

impl SymbolTableInner {
    pub fn is_owner_of(&self, scope: Scope) -> bool {
        self.pinned == scope.1
    }

    pub fn try_scope<S>(&self, scope_sym: S) -> Option<Scope>
    where
        S: Into<Symbol> + std::borrow::Borrow<Symbol>,
    {
        if scope_sym.borrow().get_scope().is_reserved() {
            if let Some(scope) = self.reserved_scopes.get(scope_sym.borrow()) {
                return Some(*scope);
            }
        }

        if scope_sym.borrow().ident() == &ident_internal!("super") {
            return Some(
                self.get_scope_metadata(scope_sym.borrow().get_scope())
                    .name
                    .get_scope(),
            );
        }

        self.scopes.get(scope_sym.borrow()).copied()
    }

    pub fn scope<S>(&mut self, scope_sym: S) -> Scope
    where
        S: Into<Symbol> + std::borrow::Borrow<Symbol>,
    {
        if scope_sym.borrow().get_scope().is_reserved() {
            if let Some(scope) = self.reserved_scopes.get(scope_sym.borrow()) {
                return *scope;
            }
        }

        if scope_sym.borrow().ident() == &ident_internal!("super") {
            return self
                .get_scope_metadata(scope_sym.borrow().get_scope())
                .name
                .get_scope();
        }

        match self.scopes.get(scope_sym.borrow()) {
            Some(scope) => *scope,
            None => {
                let new_scope = Scope(self.scopes.len() as u64 + Scope::NUM_RESERVED, self.pinned);
                self.scopes.insert(scope_sym.borrow().clone(), new_scope);
                self.scope_metadata.push(ScopeMetadata {
                    name: scope_sym.into(),
                });
                new_scope
            }
        }
    }

    pub fn insert_unique_scope(&mut self, name: impl Into<Atom>) -> Scope {
        let new_scope = Scope(self.scopes.len() as u64 + Scope::NUM_RESERVED, self.pinned);
        let scope_name = new_scope.symbol(name.into());
        self.scopes.insert(scope_name.clone(), new_scope);
        self.scope_metadata.push(ScopeMetadata {
            name: scope_name.clone(),
        });
        new_scope
    }

    pub fn get_scope_metadata(&self, scope: Scope) -> &ScopeMetadata {
        if scope.is_reserved() {
            &self.reserved_scope_metadata[scope.0 as usize]
        } else {
            assert!(self.is_owner_of(scope));
            &self.scope_metadata[(scope.0 - Scope::NUM_RESERVED) as usize]
        }
    }

    fn resolve_symbol(&mut self, symbol: &Symbol) -> SymbolIndex {
        assert!(symbol.get_scope().is_reserved() || self.is_owner_of(symbol.get_scope()));

        // If it's a reserved symbol, try the builtin hashmap first.
        if symbol.scope.is_reserved() {
            if let Some(idx) = self.builtins_to_index.get(symbol).copied() {
                return idx;
            }
        }

        let to_index = &mut self.to_index;
        let to_symbol = &mut self.to_symbol;

        *to_index.entry(symbol.clone()).or_insert_with(|| {
            let i = to_symbol.len() + NUM_BUILTINS;
            to_symbol.push(symbol.clone());
            SymbolIndex(i)
        })
    }

    fn try_resolve_symbol(&self, symbol: &Symbol) -> Option<SymbolIndex> {
        assert!(symbol.get_scope().is_reserved() || self.is_owner_of(symbol.get_scope()));

        // If it's a reserved symbol, try the builtin hashmap first.
        if symbol.scope.is_reserved() {
            if let Some(idx) = self.builtins_to_index.get(symbol).copied() {
                return Some(idx);
            }
        }

        self.to_index.get(&symbol).copied()
    }

    fn resolve_name(&mut self, name: Name) -> Symbol {
        name.path.into_iter().fold(name.root, |acc, seg| {
            let scope = self.scope(acc);
            Symbol::new(seg, scope)
        })
    }

    fn try_resolve_name(&self, name: Name) -> Option<Symbol> {
        name.path
            .into_iter()
            .fold(Some(name.root), |maybe_acc, seg| {
                let scope = self.try_scope(maybe_acc?)?;
                Some(Symbol::new(seg, scope))
            })
    }

    /// Alias one symbol to another.
    ///
    /// Panics if the symbol being aliased is already an alias to
    /// a different symbol.
    pub fn alias(&mut self, alias: Symbol, original: Symbol) {
        assert_eq!(
            {
                let resolved = self.resolve(&alias);
                &self.lookup(resolved)
            },
            &alias
        );

        let index = self.resolve_symbol(&original);
        self.to_index.insert(alias, index);
    }

    pub fn lookup(&self, idx: SymbolIndex) -> Symbol {
        if idx.0 < NUM_BUILTINS {
            self.builtins_to_symbol[idx.0].clone()
        } else {
            self.to_symbol[idx.0 - NUM_BUILTINS].clone()
        }
    }

    pub fn lookup_full(&self, idx: SymbolIndex) -> Name {
        self.get_full_name(self.lookup(idx))
    }

    fn get_full_name(&self, symbol: Symbol) -> Name {
        let mut path = Vector::new();
        let mut root = symbol;
        loop {
            let scope_name = self.get_scope_metadata(root.scope).name.clone();

            if root == scope_name {
                break;
            }

            let prev_root = mem::replace(&mut root, scope_name);
            path.push_front(prev_root.ident);
        }

        Name { path, root }
    }

    pub fn try_lookup(&self, idx: SymbolIndex) -> Option<Symbol> {
        if idx.0 < NUM_BUILTINS {
            Some(self.builtins_to_symbol[idx.0].clone())
        } else {
            self.to_symbol.get(idx.0 - NUM_BUILTINS).cloned()
        }
    }

    pub fn contains_scope(&self, scope: Scope) -> bool {
        scope.is_reserved() || self.is_owner_of(scope)
    }

    pub fn insert_anonymous_scope(&mut self) -> Scope {
        self.insert_unique_scope(atom!(""))
    }

    pub fn resolve<I: Identifier>(&mut self, ident: I) -> SymbolIndex {
        ident.resolve(self)
    }

    pub fn try_resolve<I: Identifier>(&self, ident: I) -> Option<SymbolIndex> {
        ident.try_resolve(self)
    }

    pub fn normalize<I: Identifier>(&mut self, ident: I) -> Symbol {
        ident.to_symbol(self)
    }

    pub fn normalize_full<I: Identifier>(&mut self, ident: I) -> Name {
        let symbol = ident.to_symbol(self);
        self.get_full_name(symbol)
    }

    /// Iterate over all symbols in the table, including builtins, in order.
    pub fn iter(&mut self) -> SymbolIter {
        SymbolIter {
            inner: self,
            index: 0,
        }
    }

    /// Iterate over all symbols in the table, excluding builtins, in order.
    pub fn iter_non_builtins(&mut self) -> SymbolIter {
        SymbolIter {
            inner: self,
            index: NUM_BUILTINS,
        }
    }
}

impl SymbolTable {
    /// Make an empty symbol table.
    pub fn new() -> Self {
        let inner = SymbolTableInner {
            pinned: 0,
            _phantom: PhantomPinned,

            builtins_to_symbol: &*BUILTINS_IDX_TO_SYM,
            to_symbol: Default::default(),

            builtins_to_index: &*BUILTINS_SYM_TO_IDX,
            to_index: Default::default(),

            reserved_scopes: &*RESERVED_SCOPES,
            reserved_scope_metadata: RESERVED_SCOPE_METADATA.clone(),

            scopes: Default::default(),
            scope_metadata: Default::default(),
        };

        let arc = Arc::new(RwLock::new(inner));
        let arc_ptr = (&*arc) as *const _;
        arc.write().pinned = arc_ptr as usize;

        Self { inner: arc }
    }
}

pub trait Identifier: Clone + Sized {
    fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex;
    fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex>;

    fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol;
}

impl Identifier for Symbol {
    fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
        symbol_table.resolve_symbol(&self)
    }

    fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
        symbol_table.try_resolve_symbol(&self)
    }

    fn to_symbol(self, _symbol_table: &mut SymbolTableInner) -> Symbol {
        self
    }
}

impl<'a> Identifier for &'a Symbol {
    fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
        symbol_table.resolve_symbol(self)
    }

    fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
        symbol_table.try_resolve(self)
    }
    fn to_symbol(self, _symbol_table: &mut SymbolTableInner) -> Symbol {
        self.clone()
    }
}

impl Identifier for Name {
    fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
        let symbol = symbol_table.resolve_name(self);
        symbol.resolve(symbol_table)
    }

    fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
        let symbol = symbol_table.try_resolve_name(self)?;
        symbol.try_resolve(symbol_table)
    }

    fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol {
        let index = self.resolve(symbol_table);
        symbol_table.lookup(index)
    }
}

impl<'a> Identifier for &'a Name {
    fn resolve(self, symbol_table: &mut SymbolTableInner) -> SymbolIndex {
        self.clone().resolve(symbol_table)
    }

    fn try_resolve(self, symbol_table: &SymbolTableInner) -> Option<SymbolIndex> {
        self.clone().try_resolve(symbol_table)
    }

    fn to_symbol(self, symbol_table: &mut SymbolTableInner) -> Symbol {
        self.clone().to_symbol(symbol_table)
    }
}

#[derive(Debug)]
pub struct SymbolIter<'a> {
    inner: &'a mut SymbolTableInner,
    index: usize,
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = (SymbolIndex, Symbol);

    fn next(&mut self) -> Option<Self::Item> {
        let maybe_sym = self.inner.try_lookup(SymbolIndex(self.index));
        if let Some(sym) = maybe_sym {
            self.index += 1;
            let forward_index = self.inner.resolve_symbol(&sym);
            Some((forward_index, sym))
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
        let list_module = Scope::PUBLIC.symbol("list");
        let list_scope = symbols.write().scope(&list_module);
        let map_module = list_scope.symbol("map");
        let _map_scope = symbols.write().scope(&map_module);
        let elem_module = list_scope.symbol("elem");
        let _elem_scope = symbols.write().scope(&elem_module);

        let name = Name {
            root: map_module,
            path: vector![ident_internal!("super"), Ident::from("elem")],
        };

        assert_eq!(symbols.write().normalize(name), elem_module);
    }
}
