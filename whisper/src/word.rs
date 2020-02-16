use ::{
    serde::{Deserialize, Serialize},
    std::fmt,
};

use crate::{heap::Heap, SymbolIndex};

pub type UWordBits = u64;

pub type IWordBits = i64;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Word(pub UWordBits);

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.unpack())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct WordOffset(UWordBits);

impl WordOffset {
    pub fn new(offset: isize) -> Self {
        WordOffset((offset as UWordBits) << Tag::NUM_BITS)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u64)]
pub enum Tag {
    /* 0b0000 */ UInt32 = 0,
    /* 0b0001 */ Int32 = 1,
    /* 0b0010 */ Const = 2,
    /* 0b0011 */ Float32 = 3,
    /* 0b0100 */ StructArity = 4,
    /* 0b0101 */ ExternArity = 5,
    /* 0b0110 */ BinaryArity = 6,
    /* 0b0111 */ OpaqueArity = 7,
    /* 0b1000 */ Var = 8,
    /* 0b1001 */ Tagged = 9,
    /* 0b1010 */ Cons = 10,
    /* 0b1011 */ Cons2 = 11,
    /* 0b1100 */ StructRef = 12,
    /* 0b1101 */ ExternRef = 13,
    /* 0b1110 */ BinaryRef = 14,
    /* 0b1111 */ OpaqueRef = 15,
}

impl Tag {
    pub const NUM_BITS: u32 = 4;
    pub const MASK_BITS: UWordBits = 0xf;

    pub const UINT32: UWordBits = Tag::UInt32 as UWordBits;
    pub const INT32: UWordBits = Tag::Int32 as UWordBits;
    pub const CONST: UWordBits = Tag::Const as UWordBits;
    pub const FLOAT32: UWordBits = Tag::Float32 as UWordBits;
    pub const STRUCT_ARITY: UWordBits = Tag::StructArity as UWordBits;
    pub const EXTERN_ARITY: UWordBits = Tag::ExternArity as UWordBits;
    pub const BINARY_ARITY: UWordBits = Tag::BinaryArity as UWordBits;
    pub const OPAQUE_ARITY: UWordBits = Tag::OpaqueArity as UWordBits;
    pub const VAR: UWordBits = Tag::Var as UWordBits;
    pub const TAGGED: UWordBits = Tag::Tagged as UWordBits;
    pub const CONS: UWordBits = Tag::Cons as UWordBits;
    pub const CONS2: UWordBits = Tag::Cons2 as UWordBits;
    pub const STRUCT_REF: UWordBits = Tag::StructRef as UWordBits;
    pub const EXTERN_REF: UWordBits = Tag::ExternRef as UWordBits;
    pub const BINARY_REF: UWordBits = Tag::BinaryRef as UWordBits;
    pub const OPAQUE_REF: UWordBits = Tag::OpaqueRef as UWordBits;

    #[inline]
    pub fn is_var(&self) -> bool {
        match self {
            Tag::Var => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_indirect(&self) -> bool {
        match self {
            Tag::Var
            | Tag::Tagged
            | Tag::Cons
            | Tag::Cons2
            | Tag::StructRef
            | Tag::ExternRef
            | Tag::BinaryRef
            | Tag::OpaqueRef => true,
            _ => false,
        }
    }
}

pub type Address = usize;

impl Word {
    /// The "zero" word has significant meaning! This should only be used when
    /// reinterpreting the heap as bytes for an `Object`.
    pub const ZERO: Word = Word(0);

    /// Use this whenever you're allocating more words on the heap to overwrite
    /// later.
    pub const BAD: Word = Word(Tag::VAR);

    #[inline]
    pub fn assert_tag(self, tag: Tag) -> Self {
        assert_eq!(self.get_tag(), tag);
        self
    }

    #[inline]
    pub fn debug_assert_tag(self, tag: Tag) -> Self {
        debug_assert_eq!(self.get_tag(), tag);
        self
    }

    #[inline]
    pub fn uint32(uint: u32) -> Word {
        UnpackedWord::UInt32(uint).pack()
    }

    #[inline]
    pub fn int32(int: i32) -> Word {
        UnpackedWord::Int32(int).pack()
    }

    #[inline]
    pub fn r#const(index: SymbolIndex) -> Word {
        UnpackedWord::Const(index).pack()
    }

    #[inline]
    pub fn float32(f: f32) -> Word {
        UnpackedWord::Float32(f).pack()
    }

    #[inline]
    pub fn struct_arity(arity: usize) -> Word {
        UnpackedWord::StructArity(arity).pack()
    }

    #[inline]
    pub fn extern_arity(arity: usize) -> Word {
        UnpackedWord::ExternArity(arity).pack()
    }

    #[inline]
    pub fn binary_arity(arity: usize) -> Word {
        UnpackedWord::BinaryArity(arity).pack()
    }

    #[inline]
    pub fn opaque_arity(arity: usize) -> Word {
        UnpackedWord::OpaqueArity(arity).pack()
    }

    #[inline]
    pub fn var(addr: Address) -> Word {
        UnpackedWord::Var(addr).pack()
    }

    #[inline]
    pub fn tagged(addr: Address) -> Word {
        UnpackedWord::Tagged(addr).pack()
    }

    #[inline]
    pub fn cons(addr: Address) -> Word {
        UnpackedWord::Cons(addr).pack()
    }

    #[inline]
    pub fn cons2(addr: Address) -> Word {
        UnpackedWord::Cons2(addr).pack()
    }

    #[inline]
    pub fn struct_ref(addr: Address) -> Word {
        UnpackedWord::StructRef(addr).pack()
    }

    #[inline]
    pub fn extern_ref(addr: Address) -> Word {
        UnpackedWord::ExternRef(addr).pack()
    }

    #[inline]
    pub fn binary_ref(addr: Address) -> Word {
        UnpackedWord::BinaryRef(addr).pack()
    }

    #[inline]
    pub fn opaque_ref(addr: Address) -> Word {
        UnpackedWord::OpaqueRef(addr).pack()
    }

    #[inline]
    pub fn unpack(&self) -> UnpackedWord {
        use self::UnpackedWord::*;
        let v = self.0 >> Tag::NUM_BITS;
        match self.0 & Tag::MASK_BITS {
            Tag::UINT32 => UInt32(v as u32),
            Tag::INT32 => Int32(v as i32),
            Tag::CONST => Const(SymbolIndex(v as usize)),
            Tag::FLOAT32 => Float32(f32::from_bits(v as u32)),
            Tag::STRUCT_ARITY => StructArity(v as usize),
            Tag::EXTERN_ARITY => ExternArity(v as usize),
            Tag::BINARY_ARITY => BinaryArity(v as usize),
            Tag::OPAQUE_ARITY => OpaqueArity(v as usize),
            Tag::VAR => Var(v as Address),
            Tag::TAGGED => Tagged(v as Address),
            Tag::CONS => Cons(v as Address),
            Tag::CONS2 => Cons2(v as Address),
            Tag::STRUCT_REF => StructRef(v as Address),
            Tag::EXTERN_REF => ExternRef(v as Address),
            Tag::BINARY_REF => BinaryRef(v as Address),
            Tag::OPAQUE_REF => OpaqueRef(v as Address),
            _ => panic!("Bad word tag!"), // This is truly unreachable, consider unreachable_unchecked
        }
    }

    #[inline]
    pub fn is_indirect(&self) -> bool {
        self.get_tag().is_indirect()
    }

    #[inline]
    pub fn is_header(&self) -> bool {
        use self::Tag::*;
        match self.get_tag() {
            StructArity | ExternArity | BinaryArity | OpaqueArity => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_header_address(&self) -> bool {
        use self::Tag::*;
        match self.get_tag() {
            StructRef | ExternRef | BinaryRef | OpaqueRef => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_direct(&self) -> bool {
        match self.get_tag() {
            Tag::UInt32
            | Tag::Int32
            | Tag::Const
            | Tag::Float32
            | Tag::StructArity
            | Tag::ExternArity
            | Tag::BinaryArity
            | Tag::OpaqueArity => true,
            _ => false,
        }
    }

    #[inline]
    pub fn get_address(&self) -> Address {
        debug_assert!(self.is_indirect());
        (self.0 >> Tag::NUM_BITS) as usize
    }

    #[inline]
    pub fn set_address(&mut self, addr: Address) {
        debug_assert!(self.is_indirect());
        self.0 = ((addr as UWordBits) << Tag::NUM_BITS) | (self.0 & Tag::MASK_BITS);
    }

    #[inline]
    pub fn with_address(self, addr: Address) -> Self {
        debug_assert!(self.is_indirect());
        Word(((addr as UWordBits) << Tag::NUM_BITS) | (self.0 & Tag::MASK_BITS))
    }

    #[inline]
    pub fn get_value(&self) -> UWordBits {
        debug_assert!(!self.is_indirect());
        self.0 >> Tag::NUM_BITS
    }

    #[inline]
    pub fn set_value(&mut self, value: UWordBits) {
        debug_assert!(self.is_direct());
        self.0 = (value << Tag::NUM_BITS) | (self.0 & Tag::MASK_BITS);
    }

    #[inline]
    pub fn with_value(self, value: UWordBits) -> Self {
        debug_assert!(self.is_direct());
        Word((value << Tag::NUM_BITS) | (self.0 & Tag::MASK_BITS))
    }

    #[inline]
    pub fn is_headerless_pointer(self) -> bool {
        match self.get_tag() {
            Tag::Cons | Tag::Cons2 | Tag::Tagged => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_indexable(self) -> bool {
        self.is_direct() || self.is_headerless_pointer()
    }

    #[inline]
    pub fn generalize(self, heap: &Heap) -> Word {
        if self.is_header_address() {
            heap[self.get_address()]
        } else if self.is_indirect() {
            self.with_address(0)
        } else if self.get_tag() == Tag::Float32 {
            let float = f32::from_bits(self.get_value() as u32);
            self.with_value(float.round().to_bits() as UWordBits)
        } else {
            self
        }
    }

    #[inline]
    pub fn with_displacement(&self, offset: usize) -> Word {
        if self.is_indirect() {
            Word(((offset as UWordBits) << Tag::NUM_BITS) + self.0)
        } else {
            *self
        }
    }

    #[inline]
    pub fn get_tag(&self) -> Tag {
        match self.0 & Tag::MASK_BITS {
            Tag::UINT32 => Tag::UInt32,
            Tag::INT32 => Tag::Int32,
            Tag::CONST => Tag::Const,
            Tag::FLOAT32 => Tag::Float32,
            Tag::STRUCT_ARITY => Tag::StructArity,
            Tag::EXTERN_ARITY => Tag::ExternArity,
            Tag::BINARY_ARITY => Tag::BinaryArity,
            Tag::OPAQUE_ARITY => Tag::OpaqueArity,
            Tag::VAR => Tag::Var,
            Tag::TAGGED => Tag::Tagged,
            Tag::CONS => Tag::Cons,
            Tag::CONS2 => Tag::Cons2,
            Tag::STRUCT_REF => Tag::StructRef,
            Tag::EXTERN_REF => Tag::ExternRef,
            Tag::BINARY_REF => Tag::BinaryRef,
            Tag::OPAQUE_REF => Tag::OpaqueRef,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn relocate(self, bias: WordOffset) -> Self {
        if self.is_indirect() {
            Word(self.0.wrapping_add(bias.0))
        } else {
            self
        }
    }

    #[inline]
    pub fn translate(self, trans: &[usize]) -> Self {
        match self.get_tag() {
            Tag::Const => {
                let sym = self.get_value() as usize;
                self.with_value(trans[sym] as UWordBits)
            }
            _ => self,
        }
    }

    #[inline]
    pub fn pointer_to(word: Self, address: Address) -> Self {
        match word.get_tag() {
            Tag::StructArity => Word::struct_ref(address),
            Tag::ExternArity => Word::extern_ref(address),
            Tag::BinaryArity => Word::binary_ref(address),
            Tag::OpaqueArity => Word::opaque_ref(address),
            Tag::Var => word,
            _ => Word::var(address),
        }
    }
}

impl From<UnpackedWord> for Word {
    #[inline]
    fn from(unpacked: UnpackedWord) -> Self {
        unpacked.pack()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnpackedWord {
    UInt32(u32),
    Int32(i32),
    Const(SymbolIndex),
    Float32(f32),
    StructArity(usize),
    ExternArity(usize),
    BinaryArity(usize),
    OpaqueArity(usize),
    Var(Address),
    Tagged(Address),
    Cons(Address),
    Cons2(Address),
    StructRef(Address),
    ExternRef(Address),
    BinaryRef(Address),
    OpaqueRef(Address),
}

impl UnpackedWord {
    #[inline]
    fn get_val(self) -> UWordBits {
        use self::UnpackedWord::*;
        let bits = match self {
            UInt32(uint) => uint as UWordBits,
            Int32(int) => int as UWordBits,
            Float32(f) => f.to_bits() as UWordBits,

            Const(SymbolIndex(n))
            | StructArity(n)
            | ExternArity(n)
            | BinaryArity(n)
            | OpaqueArity(n) => n as UWordBits,

            Var(a) | Tagged(a) | Cons(a) | Cons2(a) | StructRef(a) | ExternRef(a)
            | BinaryRef(a) | OpaqueRef(a) => a as UWordBits,
        };
        bits << Tag::NUM_BITS
    }

    #[inline]
    fn get_tag(self) -> UWordBits {
        use self::UnpackedWord::*;
        let tag = match self {
            UInt32(_) => Tag::UInt32,
            Int32(_) => Tag::Int32,
            Const(_) => Tag::Const,
            Float32(_) => Tag::Float32,
            StructArity(_) => Tag::StructArity,
            ExternArity(_) => Tag::ExternArity,
            BinaryArity(_) => Tag::BinaryArity,
            OpaqueArity(_) => Tag::OpaqueArity,
            Var(_) => Tag::Var,
            Tagged(_) => Tag::Tagged,
            Cons(_) => Tag::Cons,
            Cons2(_) => Tag::Cons2,
            StructRef(_) => Tag::StructRef,
            ExternRef(_) => Tag::ExternRef,
            BinaryRef(_) => Tag::BinaryRef,
            OpaqueRef(_) => Tag::OpaqueRef,
        };
        tag as UWordBits
    }

    #[inline]
    pub fn pack(&self) -> Word {
        let tag = self.get_tag();
        let val = self.get_val();
        Word(val | tag)
    }
}
