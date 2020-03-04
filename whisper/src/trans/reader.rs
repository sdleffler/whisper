use std::ops::Range;

use whisper_ir::{
    atom,
    graph::Blob,
    trans::{CompoundKind, TermReader, TermVisitor},
    Ident,
};

use crate::{
    heap::Heap,
    word::{Address, Tag, UnpackedNumber, UnpackedWord, Word},
};

#[derive(Debug, Clone, Copy)]
pub struct HeapReader<'heap> {
    heap: &'heap Heap,
    slice: &'heap [Word],
}

impl<'heap> HeapReader<'heap> {
    pub fn new(heap: &'heap Heap, slice: &'heap [Word]) -> Self {
        Self { heap, slice }
    }

    fn refocus(&self, range: Range<Address>) -> Self {
        Self {
            heap: self.heap,
            slice: &self.heap[range],
        }
    }

    fn read_tagged<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Tagged,
            self.refocus(addr..addr + 2),
        )
    }

    fn read_struct<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        let n = self.heap[addr].assert_tag(Tag::StructArity).get_value() as usize;
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Struct(n),
            self.refocus(addr + 1..addr + n + 1),
        )
    }

    fn read_extern<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        let n = self.heap[addr].assert_tag(Tag::ExternArity).get_value() as usize;
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Extern(n),
            self.refocus(addr + 1..addr + n + 1),
        )
    }

    fn read_opaque<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        let n = self.heap[addr].assert_tag(Tag::OpaqueArity).get_value() as usize;
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Opaque(n),
            self.refocus(addr + 1..addr + n + 1),
        )
    }

    fn read_binary<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        let n = self.heap[addr].assert_tag(Tag::BinaryArity).get_value() as usize;
        let words = &self.heap[addr + 1..addr + 1 + n];

        let blob = unsafe {
            let (empty, byte_slice, also_empty) = words.align_to();
            assert!(empty.is_empty() && also_empty.is_empty());
            Blob::from_bytes(byte_slice)
        };

        v.visit_blob(blob)
    }

    fn read_cons<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Cons,
            self.refocus(addr..addr + 2),
        )
    }

    fn read_cons2<V>(&mut self, v: V, addr: Address) -> V::Value
    where
        V: TermVisitor<'heap, Self>,
    {
        v.visit_compound(
            self.heap.symbols(),
            CompoundKind::Cons2,
            self.refocus(addr..addr + 3),
        )
    }
}

impl<'heap> TermReader<'heap> for HeapReader<'heap> {
    type View = Self;

    fn as_view_mut(&mut self) -> &mut Self {
        self
    }

    fn read_raw(&mut self) -> Option<u64> {
        let (h, t) = match self.slice.split_first() {
            Some(it) => it,
            None => return None,
        };

        self.slice = t;
        Some(h.0)
    }

    fn read<V>(&mut self, v: V) -> Option<V::Value>
    where
        V: TermVisitor<'heap, Self>,
    {
        let (h, t) = match self.slice.split_first() {
            Some(it) => it,
            None => return None,
        };

        self.slice = t;

        use UnpackedWord::*;
        match self.heap.chase(*h).unpack() {
            Number(n) => match n {
                UnpackedNumber::UInt32(u) => Some(v.visit_u32(u)),
                UnpackedNumber::Int32(i) => Some(v.visit_i32(i)),
                UnpackedNumber::Float32(f) => Some(v.visit_f32(f)),
            },
            Unused1(_) | Object(_) => unimplemented!("UNUSED"),
            Const(index) => {
                let symbols = self.heap.symbols();
                let localized = symbols.index(index);
                Some(v.visit_const(symbols, localized.into()))
            }
            StructArity(n) => Some(v.visit_compound(
                self.heap.symbols(),
                CompoundKind::Struct(n),
                HeapReader::new(self.heap, &t[..n]),
            )),
            ExternArity(n) => Some(v.visit_compound(
                self.heap.symbols(),
                CompoundKind::Extern(n),
                HeapReader::new(self.heap, &t[..n]),
            )),
            BinaryArity(_) => unreachable!("can't read binary without tag"),
            OpaqueArity(n) => Some(v.visit_compound(
                self.heap.symbols(),
                CompoundKind::Opaque(n),
                HeapReader::new(self.heap, &t[..n]),
            )),
            Var(addr) => Some(v.visit_var(whisper_ir::Var::Named(Ident::from_parts(
                atom!(""),
                addr as u64,
            )))),
            Tagged(addr) => Some(self.read_tagged(v, addr)),
            Cons(addr) => Some(self.read_cons(v, addr)),
            Cons2(addr) => Some(self.read_cons2(v, addr)),
            StructRef(addr) => Some(self.read_struct(v, addr)),
            ExternRef(addr) => Some(self.read_extern(v, addr)),
            OpaqueRef(addr) => Some(self.read_opaque(v, addr)),
            BinaryRef(addr) => Some(self.read_binary(v, addr)),
        }
    }
}
