use crate::{graph::Blob, trans::CompoundKind, Name, Scope, Symbol, SymbolIndex, SymbolTable, Var};

pub trait TermVisitor<'re, R: TermReader<'re, View = R>>: Sized {
    type Value;

    fn visit_var(self, _var: Var) -> Self::Value {
        todo!("visit_var");
    }

    fn visit_const(self, _symbol_table: &SymbolTable, _cst: Name) -> Self::Value {
        todo!("visit_const");
    }

    fn visit_i32(self, _i: i32) -> Self::Value {
        todo!("visit_i32");
    }

    fn visit_u32(self, _u: u32) -> Self::Value {
        todo!("visit_u32");
    }

    fn visit_f32(self, _f: f32) -> Self::Value {
        todo!("visit_f32");
    }

    fn visit_compound(
        self,
        _symbol_table: &SymbolTable,
        _kind: CompoundKind,
        _compound: R,
    ) -> Self::Value {
        todo!("visit_compound");
    }

    fn visit_blob(self, _blob: Blob) -> Self::Value {
        todo!("visit_blob")
    }

    fn visit_raw(self, _raw: u64) -> Self::Value {
        todo!("visit_raw")
    }

    fn visit<S>(self, reader: &mut S) -> Self::Value
    where
        S: TermReader<'re, View = R>,
    {
        reader.read(self).expect("todo: error handling")
    }
}

pub trait TermReader<'re>: Sized {
    type View: TermReader<'re, View = Self::View>;

    fn as_view_mut(&mut self) -> &mut Self::View;

    fn read<V>(&mut self, v: V) -> Option<V::Value>
    where
        V: TermVisitor<'re, Self::View>;

    fn read_raw(&mut self) -> Option<u64>;
}

impl<'a, 're, T: TermReader<'re, View = T>> TermReader<'re> for &'a mut T {
    type View = T::View;

    fn as_view_mut(&mut self) -> &mut Self::View {
        *self
    }

    fn read<V>(&mut self, v: V) -> Option<V::Value>
    where
        V: TermVisitor<'re, T::View>,
    {
        (**self).read(v)
    }

    fn read_raw(&mut self) -> Option<u64> {
        (**self).read_raw()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstByIndex(pub SymbolIndex);
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for ConstByIndex {
    type Value = ();

    fn visit_const(self, symbols: &SymbolTable, cst: Name) -> Self::Value {
        if symbols.write().resolve(cst) == self.0 {
            ()
        } else {
            todo!("error handling")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstByGlobal(pub &'static str);
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for ConstByGlobal {
    type Value = ();

    fn visit_const(self, symbols: &SymbolTable, cst: Name) -> Self::Value {
        let sym = symbols.write().normalize(cst);
        if sym.get_scope() == Scope::PUBLIC && sym.ident() == self.0 {
            ()
        } else {
            todo!("error handling")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NormalizedConst;
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for NormalizedConst {
    type Value = Symbol;

    fn visit_const(self, symbols: &SymbolTable, cst: Name) -> Self::Value {
        symbols.write().normalize(cst)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Const;
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for Const {
    type Value = Name;

    fn visit_const(self, _symbols: &SymbolTable, cst: Name) -> Self::Value {
        cst
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Int32;
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for Int32 {
    type Value = i32;

    fn visit_i32(self, i: i32) -> Self::Value {
        i
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UInt32;
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for UInt32 {
    type Value = u32;

    fn visit_u32(self, i: u32) -> Self::Value {
        i
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Float32;
impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for Float32 {
    type Value = f32;

    fn visit_f32(self, i: f32) -> Self::Value {
        i
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Struct<T>(pub T);

impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for Struct<()> {
    type Value = ();

    fn visit_compound(self, _: &SymbolTable, kind: CompoundKind, _compound: R) -> Self::Value {
        match kind {
            CompoundKind::Struct(0) => (),
            _ => todo!("error handling"),
        }
    }
}

impl<'re, R: TermReader<'re, View = R>, A> TermVisitor<'re, R> for Struct<(A,)>
where
    A: TermVisitor<'re, R>,
{
    type Value = (A::Value,);

    fn visit_compound(self, _: &SymbolTable, kind: CompoundKind, mut compound: R) -> Self::Value {
        match kind {
            CompoundKind::Struct(1) => ((self.0).0.visit(&mut compound),),
            _ => todo!("error handling"),
        }
    }
}

impl<'re, R: TermReader<'re, View = R>, A, B> TermVisitor<'re, R> for Struct<(A, B)>
where
    A: TermVisitor<'re, R>,
    B: TermVisitor<'re, R>,
{
    type Value = (A::Value, B::Value);

    fn visit_compound(self, _: &SymbolTable, kind: CompoundKind, mut compound: R) -> Self::Value {
        match kind {
            CompoundKind::Struct(2) => (
                (self.0).0.visit(&mut compound),
                (self.0).1.visit(&mut compound),
            ),
            _ => todo!("error handling"),
        }
    }
}

impl<'re, R: TermReader<'re, View = R>, A, B, C> TermVisitor<'re, R> for Struct<(A, B, C)>
where
    A: TermVisitor<'re, R>,
    B: TermVisitor<'re, R>,
    C: TermVisitor<'re, R>,
{
    type Value = (A::Value, B::Value, C::Value);

    fn visit_compound(self, _: &SymbolTable, kind: CompoundKind, mut compound: R) -> Self::Value {
        match kind {
            CompoundKind::Struct(3) => (
                (self.0).0.visit(&mut compound),
                (self.0).1.visit(&mut compound),
                (self.0).2.visit(&mut compound),
            ),
            _ => todo!("error handling"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Cons<A, B>(pub A, pub B);

impl<'re, R: TermReader<'re, View = R>, A, B> TermVisitor<'re, R> for Cons<A, B>
where
    A: TermVisitor<'re, R>,
    B: TermVisitor<'re, R>,
{
    type Value = (A::Value, B::Value);

    fn visit_compound(self, _: &SymbolTable, kind: CompoundKind, mut compound: R) -> Self::Value {
        match kind {
            CompoundKind::Cons => (self.0.visit(&mut compound), self.1.visit(&mut compound)),
            _ => todo!("error handling"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct List<H>(pub H);

impl<'re, R: TermReader<'re, View = R>, H> TermVisitor<'re, R> for List<H>
where
    H: TermVisitor<'re, R>,
{
    type Value = Option<(H::Value, R)>;

    fn visit_const(self, symbols: &SymbolTable, cst: Name) -> Self::Value {
        if symbols.write().resolve(cst) == Symbol::INTERNAL_LIST_NIL_INDEX {
            None
        } else {
            todo!("error handling");
        }
    }

    fn visit_compound(
        self,
        _symbols: &SymbolTable,
        kind: CompoundKind,
        mut compound: R,
    ) -> Self::Value {
        match kind {
            CompoundKind::Cons => Some((self.0.visit(&mut compound), compound)),
            _ => todo!("error handling"),
        }
    }
}
