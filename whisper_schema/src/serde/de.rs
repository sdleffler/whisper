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
        symbol::Atom,
        trans::{CompoundKind, TermReader, TermVisitor},
        Ident, Name, SymbolIndex, SymbolTable, Var,
    },
};

use crate::serde::{Error as ErrorKind, SerdeCompatError as Error};

/// Configure how the deserializer responds to free variables.
#[derive(Debug, PartialEq)]
pub enum VariableConfig {
    /// Fail parsing if we encounter a free variable. This is
    /// the default, and is usually what you want.
    Fail,

    /// Parse possible locations of free variables as `Result`s.
    /// Given a `Result<T, E>`, we'll emit `Ok(T)` in the case
    /// that we don't have a free variable, and `Err(E)` in the
    /// case that we do. A free variable will be emitted as an
    /// `Option<(String, u64)>`; it will be `None` if the
    /// variable is anonymous, and `(String, u64)` otherwise,
    /// where the `String` corresponds to the string component of
    /// an `Ident` and the `u64` to the integer component.
    AsResult,

    /// Parse variables by stripping off the variable identifier
    /// and visiting the integer part as a `u64`. This is a very
    /// specific feature for a very specific use case: parsing
    /// variables off of a Whisper heap. In the case of free variables
    /// on a Whisper heap, variables are guaranteed to never have
    /// a string part, as they're encoded into `Ident`s by pairing
    /// their address as the integer part of the identifier with
    /// the empty string for the string component.
    AsU64,
}

impl Default for VariableConfig {
    fn default() -> Self {
        VariableConfig::Fail
    }
}

#[derive(Debug, Default)]
pub struct Config {
    variable_conf: VariableConfig,
}

impl Config {
    pub fn new() -> Self {
        Config::default()
    }

    pub fn variable(mut self, variable_conf: VariableConfig) -> Self {
        self.variable_conf = variable_conf;
        self
    }
}

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
        let mut symbols_mut = symbols.write();
        let name_index = symbols_mut.insert_name(name);
        match name_index {
            SymbolIndex::UNIT => Datum::Unit,
            SymbolIndex::NONE => Datum::None,
            SymbolIndex::TRUE => Datum::Bool(true),
            SymbolIndex::FALSE => Datum::Bool(false),
            SymbolIndex::INTERNAL_LIST_NIL => Datum::ListNil,
            SymbolIndex::INTERNAL_MAP_NIL => Datum::MapNil,
            other => Datum::Const(symbols_mut[other].ident().clone()),
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
    config: Config,
    phantom: PhantomData<&'re ()>,
}

impl<'re, T: TermReader<'re>> Deserializer<'re, T> {
    pub fn from_reader(it: T) -> Self {
        Self {
            reader: it,
            config: Config::default(),
            phantom: PhantomData,
        }
    }

    pub fn from_reader_with_config(it: T, config: Config) -> Self {
        Self {
            reader: it,
            config,
            phantom: PhantomData,
        }
    }

    fn visit_datum<'de, V>(&mut self, datum: Datum<T::View>, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        use CompoundKind as Kind;
        match datum {
            Datum::Var(var) if self.config.variable_conf == VariableConfig::AsU64 => match var {
                Var::Named(ident) => visitor.visit_u64(ident.id()),
                Var::Anonymous => Err(ErrorKind::AnonymousVariablesNotSupported)?,
            },

            Datum::Var(_) => Err(ErrorKind::UnexpectedVariable)?,
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
        let datum = self.reader.read(Data).ok_or(ErrorKind::Invalid)?;
        self.visit_datum(datum, visitor)
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.config.variable_conf == VariableConfig::AsResult && name == "Result" {
            match self.reader.read(Data).ok_or(ErrorKind::Invalid)? {
                Datum::Var(var) => visitor.visit_enum(VarMockEnum::new(var)),
                other => self.visit_datum(other, visitor),
            }
        } else {
            self.deserialize_any(visitor)
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char
        str string bytes byte_buf option
        unit unit_struct newtype_struct seq tuple tuple_struct map
        struct identifier
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

enum AtomOrU64 {
    Atom(Atom),
    U64(u64),
}

impl<'a, 'de> de::Deserializer<'de> for &'a AtomOrU64 {
    type Error = Error;

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            AtomOrU64::Atom(atom) => visitor.visit_str(atom.as_ref()),
            AtomOrU64::U64(u) => visitor.visit_u64(*u),
        }
    }
}

impl<'a, 'de> IntoDeserializer<'de, Error> for &'a AtomOrU64 {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}

struct IdentDeserializer {
    ident: Ident,
}

impl<'de> de::Deserializer<'de> for IdentDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(self.ident.atom().as_ref())
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u64(self.ident.id())
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let (atom, id) = self.ident.into_parts();
        let array = [AtomOrU64::Atom(atom), AtomOrU64::U64(id)];
        visitor.visit_seq(SeqDeserializer::new(array.iter()))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u128 f32 f64 char string
        bytes byte_buf option unit unit_struct newtype_struct map tuple
        tuple_struct struct enum identifier ignored_any
    }
}

struct VarMockEnum {
    var: Var,
}

impl VarMockEnum {
    fn new(var: Var) -> Self {
        Self { var }
    }
}

impl<'de> EnumAccess<'de> for VarMockEnum {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize("Err".into_deserializer())
            .map(|val| (val, self))
    }
}

impl<'de> VariantAccess<'de> for VarMockEnum {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Err(ErrorKind::Invalid)?
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.var {
            Var::Anonymous => seed.deserialize(().into_deserializer()),
            Var::Named(ident) => seed.deserialize(ident.to_string().into_deserializer()),
        }
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        Err(ErrorKind::Invalid)?
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        Err(ErrorKind::Invalid)?
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
