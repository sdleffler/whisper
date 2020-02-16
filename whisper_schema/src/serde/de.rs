use ::{
    serde::de::{
        self,
        value::{MapDeserializer, SeqDeserializer},
        Deserialize, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
        VariantAccess, Visitor,
    },
    std::{convert::TryInto, iter, marker::PhantomData},
    whisper_ir::{
        graph::Blob,
        ident,
        trans::{CompoundKind, TermReader, TermVisitor},
        Ident, Name, Symbol, SymbolTable, Var,
    },
};

use crate::serde::{Error as ErrorKind, SerdeCompatError as Error};

pub struct Data;
pub enum Datum<R> {
    Var(Var),
    Const(Ident),
    Int32(i32),
    UInt32(u32),
    Float32(f32),
    Compound(CompoundKind, R),
    Blob(Blob),

    // Special types for compatibility with Serde data model.
    Unit,
    Bool(bool),
    None,
    ListNil,
    MapNil,
}

impl<'re, R: TermReader<'re, View = R>> TermVisitor<'re, R> for Data {
    type Value = Datum<R>;

    fn visit_var(self, var: Var) -> Self::Value {
        Datum::Var(var)
    }

    fn visit_const(self, symbols: &SymbolTable, name: Name) -> Self::Value {
        match symbols.write().normalize(name) {
            Symbol::UNIT => Datum::Unit,
            Symbol::NONE => Datum::None,
            Symbol::TRUE => Datum::Bool(true),
            Symbol::FALSE => Datum::Bool(false),
            Symbol::INTERNAL_LIST_NIL => Datum::ListNil,
            Symbol::INTERNAL_MAP_NIL => Datum::MapNil,
            other => Datum::Const(other.ident().clone()),
        }
    }

    fn visit_i32(self, i: i32) -> Self::Value {
        Datum::Int32(i)
    }

    fn visit_u32(self, u: u32) -> Self::Value {
        Datum::UInt32(u)
    }

    fn visit_f32(self, f: f32) -> Self::Value {
        Datum::Float32(f)
    }

    fn visit_compound(
        self,
        _symbols: &SymbolTable,
        kind: CompoundKind,
        compound: R,
    ) -> Self::Value {
        Datum::Compound(kind, compound)
    }

    fn visit_blob(self, blob: Blob) -> Self::Value {
        Datum::Blob(blob)
    }
}

pub struct Deserializer<'re, T: TermReader<'re>> {
    reader: T,
    phantom: PhantomData<&'re ()>,
}

impl<'re, T: TermReader<'re>> Deserializer<'re, T> {
    pub fn from_reader(it: T) -> Self {
        Self {
            reader: it,
            phantom: PhantomData,
        }
    }
}

pub fn from_reader<'re, R, T>(reader: R) -> Result<T, ErrorKind>
where
    R: TermReader<'re>,
    T: Deserialize<'re>,
{
    let mut deserializer = Deserializer::from_reader(reader);
    let t = T::deserialize(&mut deserializer)?;
    Ok(t)
}

impl<'de, 're, 'a, T: TermReader<'re>> de::Deserializer<'de> for &'a mut Deserializer<'re, T> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        use CompoundKind as Kind;
        match self.reader.read(Data).ok_or(ErrorKind::Invalid)? {
            Datum::Var(_var) => todo!(),
            Datum::Const(ident) => visitor.visit_enum(ident.str_ref().into_deserializer()),
            Datum::Int32(i) => visitor.visit_i32(i),
            Datum::UInt32(u) => visitor.visit_u32(u),
            Datum::Float32(f) => visitor.visit_f32(f),
            Datum::Blob(blob) => visitor.visit_bytes(blob.as_bytes()),
            Datum::Unit => visitor.visit_unit(),
            Datum::ListNil => visitor.visit_seq(SeqDeserializer::new(iter::empty::<()>())),
            Datum::MapNil => visitor.visit_map(MapDeserializer::new(iter::empty::<((), ())>())),
            Datum::Bool(b) => visitor.visit_bool(b),
            Datum::None => visitor.visit_none(),

            Datum::Compound(Kind::Cons, reader) => visitor.visit_seq(List::new(reader)),
            Datum::Compound(Kind::Cons2, reader) => visitor.visit_map(Map::new(reader)),
            Datum::Compound(Kind::Struct(n), reader) => visitor.visit_seq(Tuple::new(reader, n)),
            Datum::Compound(Kind::Extern(_), _reader) => todo!(),
            Datum::Compound(Kind::Opaque(_), _reader) => todo!(),

            Datum::Compound(Kind::Tagged, mut reader) => {
                match reader.read(Data).ok_or(ErrorKind::Invalid)? {
                    Datum::Var(_var) => todo!(),

                    Datum::Const(ident!("i64")) => {
                        visitor.visit_i64(reader.read_raw().unwrap() as i64)
                    }
                    Datum::Const(ident!("u64")) => visitor.visit_u64(reader.read_raw().unwrap()),
                    Datum::Const(ident!("f64")) => {
                        visitor.visit_f64(f64::from_bits(reader.read_raw().unwrap()))
                    }
                    Datum::Const(ident!("char")) => {
                        let bits = reader.read_raw().unwrap() as u32;
                        visitor.visit_char(bits.try_into().unwrap())
                    }
                    Datum::Const(ident!("str")) => {
                        match reader.read(Data).ok_or(ErrorKind::Invalid)? {
                            Datum::Blob(blob) => visitor.visit_string(blob.deserialize()),
                            _ => Err(ErrorKind::Invalid)?,
                        }
                    }
                    Datum::Const(ident!("byte array")) => {
                        match reader.read(Data).ok_or(ErrorKind::Invalid)? {
                            Datum::Blob(blob) => visitor.visit_bytes(blob.as_bytes()),
                            _ => Err(ErrorKind::Invalid)?,
                        }
                    }

                    Datum::Const(ident!("Some")) => {
                        visitor.visit_some(&mut Deserializer::from_reader(reader))
                    }

                    Datum::Const(tag) => visitor.visit_enum(Enum::new(tag, reader)),
                    _ => Err(ErrorKind::Invalid)?,
                }
            }
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char
        str string bytes byte_buf option
        unit unit_struct newtype_struct seq tuple tuple_struct map
        struct enum identifier
        ignored_any
    }
}

struct VisitDeserialize<'de, T: DeserializeSeed<'de>>(T, PhantomData<&'de ()>);

impl<'de, T: DeserializeSeed<'de>> VisitDeserialize<'de, T> {
    fn new(seed: T) -> Self {
        VisitDeserialize(seed, PhantomData)
    }
}

impl<'de, 're, T: DeserializeSeed<'de>, R: TermReader<'re, View = R>> TermVisitor<'re, R>
    for VisitDeserialize<'de, T>
{
    type Value = Result<T::Value, Error>;

    fn visit<S>(self, reader: &mut S) -> Self::Value
    where
        S: TermReader<'re, View = R>,
    {
        self.0
            .deserialize(&mut Deserializer::from_reader(reader.as_view_mut()))
    }
}

struct List<'re, R: TermReader<'re, View = R>> {
    state: Option<R>,
    _phantom: PhantomData<&'re ()>,
}

impl<'re, R: TermReader<'re, View = R>> List<'re, R> {
    fn new(state: R) -> Self {
        Self {
            state: Some(state),
            _phantom: PhantomData,
        }
    }
}

impl<'de, 're, R: TermReader<'re, View = R>> SeqAccess<'de> for List<'re, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let mut state = match self.state.take() {
            Some(it) => it,
            None => return Ok(None),
        };

        let elem = VisitDeserialize::new(seed).visit(&mut state)?;

        self.state = match Data.visit(&mut state) {
            Datum::Compound(CompoundKind::Cons, reader) => Some(reader),
            Datum::ListNil => None,
            _ => Err(ErrorKind::Invalid)?,
        };

        Ok(Some(elem))
    }
}

struct Map<'re, R: TermReader<'re, View = R>> {
    state: Option<R>,
    _phantom: PhantomData<&'re ()>,
}

impl<'re, R: TermReader<'re, View = R>> Map<'re, R> {
    fn new(state: R) -> Self {
        Self {
            state: Some(state),
            _phantom: PhantomData,
        }
    }
}

impl<'de, 're, R: TermReader<'re, View = R>> MapAccess<'de> for Map<'re, R> {
    type Error = Error;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        self.state
            .as_mut()
            .map(|state| VisitDeserialize::new(seed).visit(state))
            .transpose()
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let mut state = self.state.take().unwrap();
        let value = VisitDeserialize::new(seed).visit(&mut state)?;
        self.state = match Data.visit(&mut state) {
            Datum::Compound(CompoundKind::Cons2, reader) => Some(reader),
            Datum::MapNil => None,
            _ => Err(ErrorKind::Invalid)?,
        };

        Ok(value)
    }
}

struct Tuple<'re, R: TermReader<'re, View = R>> {
    state: R,
    index: usize,
    len: usize,
    _phantom: PhantomData<&'re ()>,
}

impl<'re, R: TermReader<'re, View = R>> Tuple<'re, R> {
    fn new(state: R, len: usize) -> Self {
        Self {
            state,
            index: 0,
            len,
            _phantom: PhantomData,
        }
    }
}

impl<'de, 're, R: TermReader<'re, View = R>> SeqAccess<'de> for Tuple<'re, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.index < self.len {
            self.index += 1;
            Ok(Some(VisitDeserialize::new(seed).visit(&mut self.state)?))
        } else {
            Ok(None)
        }
    }
}

struct Enum<'re, R: TermReader<'re, View = R>> {
    state: R,
    tag: Ident,
    _phantom: PhantomData<&'re ()>,
}

impl<'re, R: TermReader<'re, View = R>> Enum<'re, R> {
    fn new(tag: Ident, state: R) -> Self {
        Self {
            state,
            tag,
            _phantom: PhantomData,
        }
    }
}

impl<'de, 're, R: TermReader<'re, View = R>> EnumAccess<'de> for Enum<'re, R> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(self.tag.atom().into_deserializer())
            .map(|val| (val, self))
    }
}

impl<'de, 're, R: TermReader<'re, View = R>> VariantAccess<'de> for Enum<'re, R> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Err(ErrorKind::Invalid)?
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(&mut Deserializer::from_reader(self.state))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_tuple(
            &mut Deserializer::from_reader(self.state),
            len,
            visitor,
        )
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_map(&mut Deserializer::from_reader(self.state), visitor)
    }
}
