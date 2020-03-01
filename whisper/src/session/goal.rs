use ::{
    failure::{format_err, Error},
    serde::{de::DeserializeOwned, Deserialize, Serialize, Serializer},
    smallvec::{Array, SmallVec},
    whisper_ir::trans::{CompoundKind, TermEmitter, VarScopeId},
    whisper_schema::SchemaGraph,
};

use crate::{
    trans::{HeapReader, HeapWriter},
    word::{Address, Word},
};

fn variable_ser<S>(addr: &Address, ser: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    ser.serialize_newtype_struct("$Free", addr)
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename = "$Variable")]
enum VariableDe<T> {
    #[serde(rename = "$Free")]
    Free(Address),

    #[serde(rename = "$Bound")]
    Bound(T),
}

impl<T> From<VariableDe<T>> for Variable<T> {
    fn from(variable_de: VariableDe<T>) -> Self {
        match variable_de {
            VariableDe::Free(addr) => Variable::Free(addr),
            VariableDe::Bound(bound) => Variable::Bound(bound),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(from = "VariableDe<T>")]
#[serde(untagged)]
pub enum Variable<T> {
    #[serde(serialize_with = "variable_ser")]
    Free(Address),
    Bound(T),
}

impl<'a, T> Searchable for &'a (T,)
where
    T: Serialize + DeserializeOwned,
{
    type Output = (T,);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(1));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<'a, T, U> Searchable for &'a (T, U)
where
    T: Serialize + DeserializeOwned,
    U: Serialize + DeserializeOwned,
{
    type Output = (T, U);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(2));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.1)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<'a, T, U, V> Searchable for &'a (T, U, V)
where
    T: Serialize + DeserializeOwned,
    U: Serialize + DeserializeOwned,
    V: Serialize + DeserializeOwned,
{
    type Output = (T, U, V);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(3));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.1)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.2)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<'a, T, U, V, W> Searchable for &'a (T, U, V, W)
where
    T: Serialize + DeserializeOwned,
    U: Serialize + DeserializeOwned,
    V: Serialize + DeserializeOwned,
    W: Serialize + DeserializeOwned,
{
    type Output = (T, U, V, W);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(4));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.1)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.2)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.3)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<'a, A, B, C, D, E> Searchable for &'a (A, B, C, D, E)
where
    A: Serialize + DeserializeOwned,
    B: Serialize + DeserializeOwned,
    C: Serialize + DeserializeOwned,
    D: Serialize + DeserializeOwned,
    E: Serialize + DeserializeOwned,
{
    type Output = (A, B, C, D, E);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(5));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.1)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.2)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.3)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.4)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<'a, A, B, C, D, E, F> Searchable for &'a (A, B, C, D, E, F)
where
    A: Serialize + DeserializeOwned,
    B: Serialize + DeserializeOwned,
    C: Serialize + DeserializeOwned,
    D: Serialize + DeserializeOwned,
    E: Serialize + DeserializeOwned,
    F: Serialize + DeserializeOwned,
{
    type Output = (A, B, C, D, E, F);

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        writer: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error> {
        let goal = writer.begin_compound(CompoundKind::Struct(6));
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.0)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.1)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.2)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.3)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.4)?;
        whisper_schema::serde::ser::to_emitter_with_scope(writer, var_scope, &self.5)?;
        writer.end_compound();
        Ok(goal)
    }

    fn reconstruct(reader: HeapReader) -> Result<Self::Output, Error> {
        Ok(whisper_schema::serde::de::from_reader(reader)?)
    }
}

impl<T> Goal for (T,)
where
    T: Searchable,
{
    type Output = (T::Output,);

    fn emit<A>(
        &self,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
        goals: &mut SmallVec<A>,
    ) -> Result<(), Error>
    where
        A: Array<Item = Word>,
    {
        let var_scope = emitter.insert_fresh_scope();
        goals.push(self.0.deconstruct(var_scope, emitter)?);
        emitter.resolve_forwards();
        Ok(())
    }

    fn extract<'heap, I>(rs: &mut I) -> Result<(T::Output,), Error>
    where
        I: Iterator<Item = HeapReader<'heap>>,
    {
        Ok((T::reconstruct(
            rs.next().ok_or_else(|| format_err!("malformed goal"))?,
        )?,))
    }
}

impl<T, U> Goal for (T, U)
where
    T: Searchable,
    U: Searchable,
{
    type Output = (T::Output, U::Output);

    fn emit<A>(
        &self,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
        goals: &mut SmallVec<A>,
    ) -> Result<(), Error>
    where
        A: Array<Item = Word>,
    {
        let var_scope = emitter.insert_fresh_scope();
        goals.push(self.0.deconstruct(var_scope, emitter)?);
        goals.push(self.1.deconstruct(var_scope, emitter)?);
        emitter.resolve_forwards();
        Ok(())
    }

    fn extract<'heap, I>(rs: &mut I) -> Result<(T::Output, U::Output), Error>
    where
        I: Iterator<Item = HeapReader<'heap>>,
    {
        Ok((
            T::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            U::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
        ))
    }
}

impl<T, U, V> Goal for (T, U, V)
where
    T: Searchable,
    U: Searchable,
    V: Searchable,
{
    type Output = (T::Output, U::Output, V::Output);

    fn emit<A>(
        &self,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
        goals: &mut SmallVec<A>,
    ) -> Result<(), Error>
    where
        A: Array<Item = Word>,
    {
        let var_scope = emitter.insert_fresh_scope();
        goals.push(self.0.deconstruct(var_scope, emitter)?);
        goals.push(self.1.deconstruct(var_scope, emitter)?);
        goals.push(self.2.deconstruct(var_scope, emitter)?);
        emitter.resolve_forwards();
        Ok(())
    }

    fn extract<'heap, I>(rs: &mut I) -> Result<(T::Output, U::Output, V::Output), Error>
    where
        I: Iterator<Item = HeapReader<'heap>>,
    {
        Ok((
            T::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            U::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            V::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
        ))
    }
}

impl<T, U, V, W> Goal for (T, U, V, W)
where
    T: Searchable,
    U: Searchable,
    V: Searchable,
    W: Searchable,
{
    type Output = (T::Output, U::Output, V::Output, W::Output);

    fn emit<A>(
        &self,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
        goals: &mut SmallVec<A>,
    ) -> Result<(), Error>
    where
        A: Array<Item = Word>,
    {
        let var_scope = emitter.insert_fresh_scope();
        goals.push(self.0.deconstruct(var_scope, emitter)?);
        goals.push(self.1.deconstruct(var_scope, emitter)?);
        goals.push(self.2.deconstruct(var_scope, emitter)?);
        goals.push(self.3.deconstruct(var_scope, emitter)?);
        emitter.resolve_forwards();
        Ok(())
    }

    fn extract<'heap, I>(rs: &mut I) -> Result<(T::Output, U::Output, V::Output, W::Output), Error>
    where
        I: Iterator<Item = HeapReader<'heap>>,
    {
        Ok((
            T::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            U::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            V::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
            W::reconstruct(rs.next().ok_or_else(|| format_err!("malformed goal"))?)?,
        ))
    }
}

pub trait Searchable {
    type Output;

    fn deconstruct(
        &self,
        var_scope: VarScopeId,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
    ) -> Result<Word, Error>;

    fn reconstruct(readers: HeapReader) -> Result<Self::Output, Error>;
}

pub trait Goal {
    type Output;

    fn emit<A>(
        &self,
        emitter: &mut TermEmitter<HeapWriter, SchemaGraph>,
        goals: &mut SmallVec<A>,
    ) -> Result<(), Error>
    where
        A: Array<Item = Word>;

    fn extract<'heap, I>(readers: &mut I) -> Result<Self::Output, Error>
    where
        I: Iterator<Item = HeapReader<'heap>>;
}
