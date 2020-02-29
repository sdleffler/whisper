use crate::{
    knowledge_base::{Module, Relation},
    maybe_shared::MaybeShared,
    trans::{HeapReader, HeapWriter},
    word::{Address, Tag, UnpackedWord, Word, WordOffset},
    Ident, Name, SymbolIndex, SymbolTable,
};

use ::{
    itertools::Itertools,
    serde::{Deserialize, Serialize},
    std::{
        collections::HashMap,
        fmt,
        ops::{Deref, DerefMut},
    },
    whisper_ir::symbol::SymbolTableId,
};

pub type SharedHeap<'heap> = MaybeShared<'heap, Heap>;

/// Heaps are arbitrary memory containing structures encoded in [Word]s. They
/// also come with associated [`SymbolTable`]s, for mapping `Const` words back
/// to [`Symbol`]s.
///
/// [`Symbol`]: crate::symbol::Symbol
#[derive(Debug, Clone)]
pub struct Heap {
    pub(crate) symbols: SymbolTable,
    pub(crate) words: Vec<Word>,
}

impl Deref for Heap {
    type Target = [Word];

    fn deref(&self) -> &Self::Target {
        &self.words
    }
}

impl DerefMut for Heap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.words
    }
}

impl Heap {
    pub fn new(symbols: SymbolTable) -> Self {
        Self {
            symbols,
            words: Vec::with_capacity(1024),
        }
    }

    pub fn clear(&mut self) {
        self.words.clear();
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    /// "Chase" a word to ensure it's not an assigned variable.
    pub fn chase(&self, mut w: Word) -> Word {
        loop {
            if w.get_tag() == Tag::Var {
                let addr = w.get_address();
                let y = self.words[addr];
                if w != y {
                    w = y;
                    continue;
                }
            }

            return w;
        }
    }

    /// Copy a slice of words from another heap to the given address on this heap, with an offset
    /// applied to all words with addresses so that they properly relocate to this heap.
    pub fn copy_with_offset(&mut self, dst: Address, words: &[Word], offset: WordOffset) {
        for (dst, src) in self[dst..].iter_mut().zip(words.iter()) {
            *dst = src.relocate(offset);
        }
    }

    /// Copy only the head of a relation to the top of the heap for matching.
    pub fn copy_relation_head(&mut self, module: &Module, relation: &Relation) -> Word {
        let top = self.len();
        let start_address = relation.start.get_address();
        // Subtract `relation.start` from all addresses in the source heap, to relocate them
        // to starting the relation at address 0, then add the current top of the heap to get
        // them properly relocated to the top of our heap.
        let offset = WordOffset::new(top as isize - start_address as isize);
        self.words.resize(top + relation.head_len(), Word::BAD);
        // Dereffing `kb` to `Heap` and then to `[Word]` here.
        self.copy_with_offset(top, &module[start_address..relation.neck], offset);
        relation.start.with_address(top)
    }

    /// Copy only the body of a relation to the top of the heap. This is used
    /// when we've matched on the head of a relation successfully and want to
    /// load the body so we can begin trying subgoals.
    pub fn copy_relation_body(&mut self, module: &Module, relation: &Relation) {
        let top = self.len();
        // Unlike `copy_relation_head`, we already have the head on the heap, so top = previous top + relation.head_len().
        // To compensate for this we can subtract the neck address instead of the start address, as this will end up
        // taking out the extra relation.head_len() added to top.
        let offset = WordOffset::new(top as isize - relation.neck as isize);
        self.words.resize(top + relation.body_len(), Word::BAD);
        // Dereffing `kb` to `Heap` and then to `[Word]` here.
        self.copy_with_offset(top, &module[relation.neck..relation.end], offset);
    }

    pub fn read_at(&self, addr: Address) -> HeapReader {
        HeapReader::new(&self, &self[addr..])
    }

    pub fn write_top(&mut self) -> HeapWriter {
        HeapWriter::from(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayHeap<'heap> {
    heap: &'heap Heap,
}

impl<'heap> fmt::Display for DisplayHeap<'heap> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const CHUNK: usize = 8;
        writeln!(f, "[top] = {:04x}", self.heap.len())?;
        writeln!(f, "DATA:")?;

        for (i, chunk) in self.heap.words.chunks(CHUNK).enumerate() {
            write!(f, "[{:04x}]", CHUNK * i)?;
            for word in chunk.iter() {
                use UnpackedWord::*;
                " ".fmt(f)?;
                match word.unpack() {
                    UInt32(uint) => write!(f, "UInt:{:04x}", uint)?,
                    Int32(int) => write!(f, "Int: {:04x}", int)?,
                    Const(c) => write!(f, "Cnst:{:04x}", c)?,
                    Float32(n) => write!(f, "Fl32:{:04x}", n as u16)?,
                    StructArity(a) => write!(f, "StAr:{:04x}", a)?,
                    ExternArity(a) => write!(f, "ExAr:{:04x}", a)?,
                    BinaryArity(a) => write!(f, "BiAr:{:04x}", a)?,
                    OpaqueArity(a) => write!(f, "OpAr:{:04x}", a)?,
                    Var(v) => write!(f, "Var: {:04x}", v)?,
                    Tagged(a) => write!(f, "Tagd:{:04x}", a)?,
                    Cons(a) => write!(f, "Cons:{:04x}", a)?,
                    Cons2(a) => write!(f, "Con2:{:04x}", a)?,
                    StructRef(a) => write!(f, "StRf:{:04x}", a)?,
                    ExternRef(a) => write!(f, "ExRf:{:04x}", a)?,
                    BinaryRef(a) => write!(f, "BiRf:{:04x}", a)?,
                    OpaqueRef(a) => write!(f, "OpRf:{:04x}", a)?,
                }
            }
            if chunk.len() == CHUNK {
                write!(f, " |\n")?;
            }
        }

        if self.heap.words.len() % CHUNK != 0 {
            write!(f, "\n")?;
        }

        writeln!(f, "SYM:")?;
        for (i, symbol) in self.heap.symbols.write().iter().enumerate() {
            writeln!(f, "[{:04x}] {:?}", i, symbol)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayAt<'heap> {
    heap: &'heap Heap,
    addr: usize,
}

impl<'heap> fmt::Display for DisplayAt<'heap> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnpackedWord::*;
        match self.heap.chase(self.heap.words[self.addr]).unpack() {
            UInt32(i) => i.fmt(f),
            Int32(i) => i.fmt(f),
            Float32(v) => v.fmt(f),
            Const(c) => {
                let symbols = self.heap.symbols();
                let indexed = symbols.index(c);
                write!(f, "\"{}\"", symbols.read().display(indexed))
            }
            StructArity(arity) => {
                let display = self.heap.words[self.addr + 1..][..arity]
                    .iter()
                    .map(|&w| self.heap.display_word(w))
                    .format(" ");
                write!(f, "{}", display)
            }
            ExternArity(arity) => {
                let display = self.heap.words[self.addr + 1..][..arity]
                    .iter()
                    .map(|&w| self.heap.display_word(w))
                    .format(" ");
                write!(f, "extern {}", display)
            }
            BinaryArity(arity) => write!(f, "[Word; {}]", arity),
            OpaqueArity(arity) => {
                let display = self.heap.words[self.addr + 1..][..arity]
                    .iter()
                    .map(|&w| self.heap.display_word(w))
                    .format(" ");
                write!(f, "# {}", display)
            }
            Var(addr) if addr == self.addr => write!(f, "0x{:x}?", addr),
            Var(addr) => self.heap.display_at(addr).fmt(f),
            Tagged(addr) => write!(
                f,
                "({} : {})",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1)
            ),
            Cons(addr) => write!(
                f,
                "[{}|{}]",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1)
            ),
            Cons2(addr) => write!(
                f,
                "{{ {}: {} | {} }}",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1),
                self.heap.display_at(addr + 2)
            ),
            StructRef(addr) | ExternRef(addr) | OpaqueRef(addr) => {
                write!(f, "({})", self.heap.display_at(addr))
            }
            BinaryRef(addr) => self.heap.display_at(addr).fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayWord<'heap> {
    heap: &'heap Heap,
    word: Word,
}

impl<'heap> fmt::Display for DisplayWord<'heap> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnpackedWord::*;
        match self.word.unpack() {
            UInt32(i) => i.fmt(f),
            Int32(i) => i.fmt(f),
            Float32(v) => v.fmt(f),
            Const(c) => {
                let symbols = self.heap.symbols();
                let indexed = symbols.index(c);
                write!(f, "\"{}\"", symbols.read().display(indexed))
            }
            StructArity(arity) => write!(f, "StrA({})", arity),
            ExternArity(arity) => write!(f, "ExtA({})", arity),
            BinaryArity(arity) => write!(f, "BinA({})", arity),
            OpaqueArity(arity) => write!(f, "OpqA({})", arity),
            Var(addr) if self.heap[addr] == self.word => write!(f, "0x{:x}?", addr),
            Var(addr) => self.heap.display_at(addr).fmt(f),
            Tagged(addr) => write!(
                f,
                "({} : {})",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1)
            ),
            Cons(addr) => write!(
                f,
                "[{}|{}]",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1)
            ),
            Cons2(addr) => write!(
                f,
                "{{ {}: {} | {} }}",
                self.heap.display_at(addr),
                self.heap.display_at(addr + 1),
                self.heap.display_at(addr + 2)
            ),
            StructRef(addr) | ExternRef(addr) | OpaqueRef(addr) => {
                write!(f, "({})", self.heap.display_at(addr))
            }
            BinaryRef(addr) => self.heap.display_at(addr).fmt(f),
        }
    }
}

impl Heap {
    pub fn display(&self) -> DisplayHeap {
        DisplayHeap { heap: self }
    }

    pub fn display_at(&self, addr: usize) -> DisplayAt {
        DisplayAt { heap: self, addr }
    }

    pub fn display_word(&self, word: Word) -> DisplayWord {
        DisplayWord { heap: self, word }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortableSymbol {
    sym_id: u64,
    path: Vec<Ident>,
}

impl PortableSymbol {
    pub fn from_name(full_name: &Name) -> Self {
        assert!(full_name.root.is_builtin());
        //println!("Serializing name: {}", full_name);
        let path = full_name.path.iter().cloned().collect();
        let sym_id = full_name.root.0 as u64;
        PortableSymbol { sym_id, path }
    }

    pub fn from_symbol(symbols: &SymbolTable, symbol: SymbolIndex) -> Self {
        let full_name = symbols.read().lookup_name(symbol);
        Self::from_name(&full_name)
    }

    // pub fn into_name(&self, symbols: &SymbolTable) -> Name {
    //     let scope = Scope::from_id(self.scope_id);
    //     let root = symbols.get_scope_metadata(scope).name.clone();
    //     let path = self.path.iter().map(|s| Atom::from(&**s)).collect();
    //     Name { root, path }
    // }

    pub fn into_name_with_root_module(&self, root: SymbolIndex) -> Name {
        let scope = SymbolIndex(self.sym_id as usize, SymbolTableId::NULL);

        let root = if scope == SymbolIndex::MOD {
            root
        } else {
            scope
        };

        let path = self.path.iter().cloned().collect();
        let name = Name { root, path };

        //println!("Visiting: {}", symbols.normalize_full(&name));
        name
    }

    // pub fn into_symbol(&self, symbols: &SymbolTable) -> Symbol {
    //     symbols.normalize(self.into_name(symbols))
    // }

    pub fn into_symbol_with_root_module(
        &self,
        symbols: &SymbolTable,
        root: SymbolIndex,
    ) -> SymbolIndex {
        symbols
            .write()
            .insert_name(self.into_name_with_root_module(root))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortableHeap {
    pub symbols: HashMap<u64, PortableSymbol>,
    pub words: Vec<Word>,
}

impl PortableHeap {
    pub fn from_heap(heap: &Heap) -> Self {
        let mut symbols = HashMap::new();
        let words = heap.words.clone();

        for &word in words.iter() {
            if word.get_tag() == Tag::Const && !symbols.contains_key(&word.get_value()) {
                let full_name = heap
                    .symbols
                    .read()
                    .lookup_name(heap.symbols.index(word.get_value() as usize));
                symbols.insert(word.get_value(), PortableSymbol::from_name(&full_name));
            }
        }

        PortableHeap { symbols, words }
    }

    // pub fn into_heap(&self, symbols: SymbolTable) -> Heap {
    //     let mut words = self.words.clone();

    //     for word in words.iter_mut() {
    //         if word.get_tag() == Tag::Const {
    //             let sym_index =
    //                 symbols.resolve(self.symbols[&word.get_value()].into_name(&symbols));
    //             *word = Word::r#const(sym_index);
    //         }
    //     }

    //     Heap { symbols, words }
    // }

    pub fn into_heap_with_root_module(&self, symbols: SymbolTable, root: SymbolIndex) -> Heap {
        let mut words = self.words.clone();

        for word in words.iter_mut() {
            if word.get_tag() == Tag::Const {
                let sym_index =
                    self.symbols[&word.get_value()].into_symbol_with_root_module(&symbols, root);
                *word = Word::r#const(sym_index.0);
            }
        }

        Heap { symbols, words }
    }
}

// The only purpose of this function is to fail compilation if these types do not
// implement `Send + Sync`.
#[allow(dead_code, unconditional_recursion)]
fn assert_send_sync<T: Send + Sync>() {
    assert_send_sync::<Heap>();
    assert_send_sync::<PortableSymbol>();
    assert_send_sync::<PortableHeap>();
}
