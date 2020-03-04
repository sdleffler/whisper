use crate::{
    heap::{Heap, PortableHeap, PortableSymbol},
    runtime::ExternModule,
    word::{Tag, Word},
    SymbolIndex, SymbolTable,
};

use ::{
    im::HashMap as ImHashMap,
    roaring::RoaringBitmap,
    serde::{de::DeserializeOwned, Deserialize, Serialize},
    smallvec::SmallVec,
    std::{
        collections::HashMap,
        ops::{Deref, Index, Range},
        sync::Arc,
    },
};

#[derive(Debug, Clone, Copy)]
pub struct RelationId(usize);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Relation {
    pub start: Word,
    pub end: usize,
    pub neck: usize,
    pub goals: SmallVec<[Word; 4]>,
}

impl Relation {
    /// Get the length of the slice containing the entirety of the relation.
    /// This is just a convenient shorthand for `self.end - self.start`.
    pub fn len(&self) -> usize {
        self.end - self.start.get_address()
    }

    /// Get the length of the slice containing the head of the relation. This
    /// is really just convenient shorthand for `self.neck - self.start`.
    pub fn head_len(&self) -> usize {
        self.neck - self.start.get_address()
    }

    /// Get the length of the slice containing the body of the relation. This
    /// is really just convenient shorthand for `self.end - self.neck`.
    pub fn body_len(&self) -> usize {
        self.end - self.neck
    }
}

pub type MatchesStorage = SmallVec<[RelationId; 4]>;

#[derive(Debug, Clone)]
pub struct Matches {
    ids: MatchesStorage,
    index: usize,
}

impl Matches {
    pub fn new(storage: MatchesStorage) -> Self {
        Self {
            ids: storage,
            index: 0,
        }
    }

    pub fn empty() -> Self {
        Self {
            ids: MatchesStorage::new(),
            index: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.ids.len()
    }

    pub fn cut(&mut self) {
        self.index = self.ids.len();
    }
}

impl Iterator for Matches {
    type Item = RelationId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.ids.len() {
            let id = self.ids[self.index];
            self.index += 1;
            Some(id)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.len();
        (size, Some(size))
    }
}

impl ExactSizeIterator for Matches {
    fn len(&self) -> usize {
        self.ids.len() - self.index
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    inner: Arc<ModuleInner>,
}

#[derive(Debug, Clone)]
struct ModuleInner {
    heap: Heap,
    const_sets: HashMap<(usize, Word), RoaringBitmap>,
    var_sets: Vec<RoaringBitmap>,
    relations: Vec<Relation>,
    name: SymbolIndex,
}

impl Index<RelationId> for Module {
    type Output = Relation;

    fn index(&self, idx: RelationId) -> &Self::Output {
        &self.inner.relations[idx.0]
    }
}

impl Index<Range<usize>> for Module {
    type Output = [Word];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        &self.inner.heap[range]
    }
}

impl Deref for Module {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        &self.inner.heap
    }
}

impl Module {
    pub fn symbols(&self) -> &SymbolTable {
        &self.inner.heap.symbols
    }

    pub fn name(&self) -> SymbolIndex {
        self.inner.name
    }

    pub fn from_mapped_heap(name: SymbolIndex, heap: Heap, relations: Vec<Relation>) -> Self {
        // println!("Building knowledge base from heap...");

        let mut const_sets = HashMap::new();
        let mut var_sets = Vec::new();

        for (relation_idx, relation) in relations.iter().enumerate() {
            let arity = heap[relation.start.get_address()].get_value() as usize;
            let base = relation.start.get_address() + 1;
            let mut buf = SmallVec::<[Word; 32]>::with_capacity(arity);
            for p in base..base + arity {
                buf.push(heap[p].generalize(&heap));
            }

            if var_sets.len() < buf.len() {
                var_sets.resize_with(buf.len(), || RoaringBitmap::new());
            }

            let relation_idx = relation_idx as u32;
            for (i, &w) in buf.iter().enumerate() {
                if w.is_indexable() {
                    const_sets
                        .entry((i, w))
                        .or_insert_with(RoaringBitmap::new)
                        .insert(relation_idx);
                } else if w.get_tag() == Tag::Var {
                    var_sets[i].insert(relation_idx);
                }
            }
        }

        Self {
            inner: Arc::new(ModuleInner {
                heap,
                const_sets,
                var_sets,
                relations,
                name,
            }),
        }
    }

    pub fn search(&self, heap: &Heap, head_ref: Word) -> Matches {
        // println!(
        //     "Searching knowledge base for matches against head `{}`. ",
        //     heap.display_word(head_ref)
        // );

        let head_addr = head_ref.get_address();
        let arity = heap[head_addr].get_value() as usize;
        let base = head_addr + 1;
        let mut buf = SmallVec::<[Word; 32]>::with_capacity(arity);
        for p in base..base + arity {
            buf.push(heap.chase(heap[p]).generalize(&heap));
        }

        // Look for the first word that's indexable
        let mut primary_idx = 0;
        let primary_set = loop {
            let idx = primary_idx;
            match buf.get(idx) {
                Some(&word) => {
                    primary_idx += 1;
                    if word.is_indexable() {
                        if let Some(sets) = self.inner.const_sets.get(&(idx, word)) {
                            // println!("Indexable word found: {}", heap.display_word(word));
                            break sets.iter().chain(self.inner.var_sets[idx].iter());
                        }
                    }
                }
                None => {
                    let matches = (0..self.inner.relations.len())
                        .map(RelationId)
                        .collect::<MatchesStorage>(); // This could match anything... collect *all* relations!
                    return Matches::new(matches);
                }
            };
        };

        let mut indices = MatchesStorage::new();
        'matching: for candidate in primary_set {
            // println!("Candidate {}", candidate);

            'checking: for pos in primary_idx..buf.len() {
                let w = buf[pos];
                if !w.is_indexable() {
                    continue 'checking;
                } else {
                    if let Some(set) = self.inner.const_sets.get(&(pos, w)) {
                        if set.contains(candidate) {
                            continue 'checking;
                        }
                    }

                    if let Some(set) = self.inner.var_sets.get(pos) {
                        if set.contains(candidate) {
                            continue 'checking;
                        }
                    }

                    continue 'matching;
                }
            }

            indices.push(RelationId(candidate as usize));

            // println!("Match: {:?}", &self.inner.relations[candidate as usize]);
        }

        // println!("{} matches found: {:?}", indices.len(), indices);

        Matches::new(indices)
    }
}

#[derive(Debug, Clone)]
pub enum KbEntry<E: ExternModule> {
    Dynamic(Module),
    Extern(E),
}

impl<E: ExternModule> From<Module> for KbEntry<E> {
    fn from(module: Module) -> Self {
        Self::Dynamic(module)
    }
}

#[derive(Debug, Clone)]
pub struct KnowledgeBase<E: ExternModule> {
    symbols: SymbolTable,
    modules: ImHashMap<SymbolIndex, KbEntry<E>>,
    root: SymbolIndex,
}

impl<E: ExternModule> KnowledgeBase<E> {
    pub fn new(symbols: SymbolTable, root: SymbolIndex) -> Self {
        Self {
            symbols,
            modules: ImHashMap::new(),
            root,
        }
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn get(&self, idx: SymbolIndex) -> Option<&KbEntry<E>> {
        self.modules.get(&idx)
    }

    pub fn root(&self) -> (SymbolIndex, &KbEntry<E>) {
        (SymbolIndex::MOD, self.get(SymbolIndex::MOD).unwrap())
    }

    pub fn insert(&mut self, name: SymbolIndex, module: impl Into<KbEntry<E>>) {
        // TODO: assert symbol table match
        self.modules.insert(name, module.into());
    }

    pub fn remove(&mut self, name: SymbolIndex) {
        self.modules.remove(&name);
    }

    pub fn import(&mut self, root: SymbolIndex, name: &PortableSymbol, module: &PortableKbEntry) {
        let name = name.into_symbol_with_root_module(self.symbols(), root);

        match module {
            PortableKbEntry::Dynamic(module) => {
                let module = module.into_module_with_root(self.symbols.clone(), root);
                //println!("Importing: {}", self.symbols.normalize_full(&module.name));
                self.insert(name, module);
            }
            PortableKbEntry::Extern(ext) => {
                let ext = E::from_bytes(self.symbols(), ext).expect("todo");
                self.insert(name, KbEntry::Extern(ext));
            }
        }
    }

    pub fn import_all(&mut self, root: SymbolIndex, portable_kb: &PortableKnowledgeBase) {
        for (portable_name, portable_module) in &portable_kb.modules {
            self.import(root, portable_name, portable_module);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &KbEntry<E>> {
        self.modules.values()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortableModule {
    heap: PortableHeap,
    const_sets: Vec<(usize, Word, Vec<u8>)>,
    var_sets: Vec<Vec<u8>>,
    relations: Vec<Relation>,
    name: PortableSymbol,
}

impl PortableModule {
    pub fn from_module(module: &Module) -> Self {
        let heap = PortableHeap::from_heap(&*module);

        let const_sets = module
            .inner
            .const_sets
            .iter()
            .map(|((idx, word), bitset)| {
                let mut bytes = Vec::new();
                bitset
                    .serialize_into(&mut bytes)
                    .expect("cannot fail to write to `Vec`");

                (*idx, *word, bytes)
            })
            .collect();

        let var_sets = module
            .inner
            .var_sets
            .iter()
            .map(|bitset| {
                let mut bytes = Vec::new();
                bitset
                    .serialize_into(&mut bytes)
                    .expect("cannot fail to write to `Vec`");

                bytes
            })
            .collect();

        let relations = module.inner.relations.clone();
        let name = PortableSymbol::from_symbol(module.symbols(), module.name());

        Self {
            heap,
            const_sets,
            var_sets,
            relations,
            name,
        }
    }

    pub fn into_module_with_root(&self, symbols: SymbolTable, root: SymbolIndex) -> Module {
        let const_sets = self
            .const_sets
            .iter()
            .map(|(idx, word, bytes)| {
                let mut word = *word;
                if word.get_tag() == Tag::Const {
                    let sym = self.heap.symbols[&word.get_value()]
                        .into_symbol_with_root_module(&symbols, root);
                    word = Word::r#const(sym.0);
                }

                (
                    (*idx, word),
                    RoaringBitmap::deserialize_from(&mut &bytes[..])
                        .expect("cannot fail to read from `Vec`"),
                )
            })
            .collect();

        let var_sets = self
            .var_sets
            .iter()
            .map(|bytes| {
                RoaringBitmap::deserialize_from(&mut &bytes[..])
                    .expect("cannot fail to read from `Vec`")
            })
            .collect();

        let relations = self.relations.clone();
        let name = self.name.into_symbol_with_root_module(&symbols, root);
        let heap = self.heap.into_heap_with_root_module(symbols, root);

        Module {
            inner: Arc::new(ModuleInner {
                heap,
                const_sets,
                var_sets,
                relations,
                name,
            }),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PortableKbEntry {
    Dynamic(PortableModule),
    Extern(Vec<u8>),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PortableKnowledgeBase {
    modules: Vec<(PortableSymbol, PortableKbEntry)>,
}

impl PortableKnowledgeBase {
    pub fn from_knowledge_base<E>(knowledge_base: &KnowledgeBase<E>) -> Self
    where
        E: ExternModule + Serialize,
    {
        Self {
            modules: knowledge_base
                .modules
                .iter()
                .map(|(name, entry)| {
                    let kb_entry = match entry {
                        KbEntry::Dynamic(module) => {
                            PortableKbEntry::Dynamic(PortableModule::from_module(&*module))
                        }
                        KbEntry::Extern(e) => PortableKbEntry::Extern(e.to_bytes().expect("todo")),
                    };
                    let name = PortableSymbol::from_symbol(knowledge_base.symbols(), *name);
                    (name, kb_entry)
                })
                .collect(),
        }
    }

    pub fn into_knowledge_base_with_root<E>(
        &self,
        symbols: SymbolTable,
        root: SymbolIndex,
    ) -> KnowledgeBase<E>
    where
        E: ExternModule + DeserializeOwned,
    {
        let modules = self
            .modules
            .iter()
            .map(|(name, module)| {
                let kb_entry = match module {
                    PortableKbEntry::Dynamic(pmod) => {
                        let module = pmod.into_module_with_root(symbols.clone(), root);
                        KbEntry::Dynamic(module)
                    }
                    PortableKbEntry::Extern(e) => {
                        KbEntry::Extern(E::from_bytes(&symbols, e).expect("todo"))
                    }
                };
                (name.into_symbol_with_root_module(&symbols, root), kb_entry)
            })
            .collect();

        KnowledgeBase {
            symbols,
            modules,
            root,
        }
    }
}

// The only purpose of this function is to fail compilation if these types do not
// implement `Send + Sync`.
#[allow(dead_code, unconditional_recursion)]
fn assert_send_sync<T: Send + Sync, E: ExternModule>() {
    assert_send_sync::<Module, E>();
    assert_send_sync::<KnowledgeBase<E>, E>();
    assert_send_sync::<PortableModule, E>();
    assert_send_sync::<PortableKnowledgeBase, E>();
}
