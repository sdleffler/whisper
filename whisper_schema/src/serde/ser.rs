use ::{
    serde::{ser, Serialize},
    std::iter,
    whisper_ir::{
        atom,
        graph::Blob,
        trans::{TermEmitter, TermWriter},
    },
};

use crate::{
    serde::SerdeCompatError as Error, SchemaArena, SchemaCompound, SchemaGraph, SchemaNode,
};

pub struct Serializer<'arena, 'w, W: TermWriter> {
    emitter: &'w mut TermEmitter<W, SchemaGraph<'arena>>,
}

pub fn to_writer<W, S>(writer: &mut W, schema: &S) -> Result<W::Placement, Error>
where
    W: TermWriter,
    S: Serialize,
{
    let schema_arena = SchemaArena::new();
    let mut serializer = Serializer {
        emitter: &mut TermEmitter::new(
            SchemaGraph {
                arena: &schema_arena,
            },
            writer,
        ),
    };

    let schema = SchemaCompound::Tagged(
        "Schema",
        serializer
            .get_arena()
            .alloc(schema.serialize(&mut serializer)?),
    );

    let emitter = serializer.emitter;
    let var_scope = emitter.insert_fresh_scope();
    Ok(emitter.emit_full(var_scope, schema))
}

pub fn to_emitter<'arena, W, S>(
    emitter: &mut TermEmitter<W, SchemaGraph<'arena>>,
    schema: &S,
) -> Result<(), Error>
where
    W: TermWriter,
    S: Serialize,
{
    let mut serializer = Serializer { emitter };
    let node = schema.serialize(&mut serializer)?;
    let mut emitter = serializer.emitter;
    let var_scope = emitter.insert_fresh_scope();
    node.visit(&mut emitter, var_scope);
    Ok(())
}

impl<'arena, 'w, W: TermWriter> Serializer<'arena, 'w, W> {
    fn get_arena(&self) -> &'arena SchemaArena<'arena> {
        &self.emitter.get_graph().arena
    }
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::Serializer for &'ser mut Serializer<'arena, 'w, B> {
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    type SerializeSeq = SeqSerializer<'ser, 'arena, 'w, B>;
    type SerializeTuple = TupleSerializer<'ser, 'arena, 'w, B>;
    type SerializeTupleStruct = TupleStructSerializer<'ser, 'arena, 'w, B>;
    type SerializeTupleVariant = TupleVariantSerializer<'ser, 'arena, 'w, B>;
    type SerializeMap = MapSerializer<'ser, 'arena, 'w, B>;
    type SerializeStruct = StructSerializer<'ser, 'arena, 'w, B>;
    type SerializeStructVariant = StructVariantSerializer<'ser, 'arena, 'w, B>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Error> {
        let s = match v {
            true => "true",
            false => "false",
        };

        Ok(SchemaNode::const_from(s))
    }

    // Serializing i8, i16, and i32 are all the same; all will fit into the
    // Whisper `Int` heap representation. i64, however, might not since the
    // heap representation really only has 60 bits for the integer. So we'll
    // have to bincode it into an `Object`.
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Error> {
        self.serialize_i32(v as i32)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Error> {
        self.serialize_i32(v as i32)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::Int32(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Error> {
        let raw_node = self.get_arena().alloc(SchemaNode::Raw(v as u64));
        Ok(SchemaNode::tagged("i64", raw_node))
    }

    // u8, u16, and u32 are much like i8, i16, and i32, fitting into the `UInt`
    // word. u64, like i64, has to go into a `Blob`.
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Error> {
        self.serialize_u32(v as u32)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Error> {
        self.serialize_u32(v as u32)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::UInt32(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Error> {
        let raw_node = self.get_arena().alloc(SchemaNode::Raw(v));
        Ok(SchemaNode::tagged("u64", raw_node))
    }

    // For floats, Whisper supports f32s natively but f64s once again must be
    // `Blob`'d.
    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::Float32(v.into()))
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Error> {
        let raw_node = self.get_arena().alloc(SchemaNode::Raw(v.to_bits()));
        Ok(SchemaNode::tagged("f64", raw_node))
    }

    // Rust `char`s are a little complex, so we just Blob 'em for now.
    fn serialize_char(self, v: char) -> Result<Self::Ok, Error> {
        let raw_node = self.get_arena().alloc(SchemaNode::Raw(v as u64));
        Ok(SchemaNode::tagged("char", raw_node))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::const_from(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Error> {
        let blob_node = SchemaNode::Blob(Blob::from_bytes(v));
        let node_ref = self.get_arena().alloc(blob_node);
        Ok(SchemaNode::tagged("byte array", node_ref))
    }

    fn serialize_none(self) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::Const(atom!("None")))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Error>
    where
        T: ?Sized + Serialize,
    {
        let some_value = self.get_arena().alloc(value.serialize(&mut *self)?);
        Ok(SchemaNode::tagged("Some", some_value))
    }

    fn serialize_unit(self) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::Const(atom!("()")))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Error> {
        self.serialize_unit()
    }

    // TODO: track variants for generating predicates, so that we can store the variant index here
    // instead of making the name a Const.
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Error> {
        Ok(SchemaNode::const_from(variant))
    }

    // TODO: do we need the name? Serde docs says serializers are encouraged to just serialize the
    // underlying value...
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Self::Ok, Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Error>
    where
        T: ?Sized + Serialize,
    {
        let node = self.get_arena().alloc(value.serialize(self)?);
        Ok(SchemaNode::tagged(variant, node))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Error> {
        let len = len.expect("sequence must have length");
        let elems = iter::repeat(SchemaNode::Raw(0)).take(len);
        let slice = self.get_arena().alloc_extend(elems);

        Ok(SeqSerializer {
            serializer: self,
            elems: slice,
            index: 0,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Error> {
        let elems = iter::repeat(SchemaNode::Raw(0)).take(len);
        let slice = self.get_arena().alloc_extend(elems);

        Ok(TupleSerializer {
            serializer: self,
            elems: slice,
            index: 0,
        })
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Error> {
        let elems = iter::repeat(SchemaNode::Raw(0)).take(len);
        let slice = self.get_arena().alloc_extend(elems);

        Ok(TupleStructSerializer {
            serializer: self,
            elems: slice,
            index: 0,
        })
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Error> {
        let elems = iter::repeat(SchemaNode::Raw(0)).take(len);
        let slice = self.get_arena().alloc_extend(elems);

        Ok(TupleVariantSerializer {
            serializer: self,
            variant,
            elems: slice,
            index: 0,
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Error> {
        let len = len.expect("map must have length");
        let pairs = iter::repeat(SchemaNode::Raw(0)).take(len * 2);
        let slice = self.get_arena().alloc_extend(pairs);

        Ok(MapSerializer {
            serializer: self,
            elems: slice,
            index: 0,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Error> {
        let fields = iter::repeat(SchemaNode::Raw(0)).take(len * 2);
        let slice = self.get_arena().alloc_extend(fields);

        Ok(StructSerializer {
            serializer: self,
            fields: slice,
            index: 0,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Error> {
        let fields = iter::repeat(SchemaNode::Raw(0)).take(len * 2);
        let slice = self.get_arena().alloc_extend(fields);

        Ok(StructVariantSerializer {
            serializer: self,
            variant,
            fields: slice,
            index: 0,
        })
    }
}

pub struct SeqSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    elems: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeSeq for SeqSerializer<'ser, 'arena, 'w, B> {
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = value.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        match self.elems.split_first() {
            Some((h, t)) => Ok(SchemaNode::Compound(SchemaCompound::Seq(h, t))),
            None => Ok(SchemaNode::ListNil),
        }
    }
}

pub struct TupleSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    elems: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeTuple for TupleSerializer<'ser, 'arena, 'w, B> {
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = value.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        Ok(SchemaNode::Compound(SchemaCompound::Tuple(self.elems)))
    }
}

pub struct TupleStructSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    elems: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeTupleStruct
    for TupleStructSerializer<'ser, 'arena, 'w, B>
{
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = value.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        Ok(SchemaNode::Compound(SchemaCompound::Tuple(self.elems)))
    }
}

pub struct TupleVariantSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    variant: &'static str,
    elems: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeTupleVariant
    for TupleVariantSerializer<'ser, 'arena, 'w, B>
{
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = value.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        Ok(SchemaNode::Compound(SchemaCompound::TupleVariant(
            self.variant,
            self.elems,
        )))
    }
}

pub struct MapSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    elems: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeMap for MapSerializer<'ser, 'arena, 'w, B> {
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = key.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.elems[self.index] = value.serialize(&mut *self.serializer)?;
        self.index += 1;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        if self.elems.is_empty() {
            Ok(SchemaNode::MapNil)
        } else {
            let k = &self.elems[0];
            let v = &self.elems[1];
            let t = &self.elems[2..];

            Ok(SchemaNode::Compound(SchemaCompound::Map(k, v, t)))
        }
    }
}

pub struct StructSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    fields: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeStruct
    for StructSerializer<'ser, 'arena, 'w, B>
{
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.fields[self.index + 0] = SchemaNode::const_from(key);
        self.fields[self.index + 1] = value.serialize(&mut *self.serializer)?;
        self.index += 2;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        if self.fields.is_empty() {
            Ok(SchemaNode::MapNil)
        } else {
            let k = &self.fields[0];
            let v = &self.fields[1];
            let t = &self.fields[2..];

            Ok(SchemaNode::Compound(SchemaCompound::Map(k, v, t)))
        }
    }
}

pub struct StructVariantSerializer<'ser, 'arena, 'w, B: TermWriter> {
    serializer: &'ser mut Serializer<'arena, 'w, B>,
    variant: &'static str,
    fields: &'arena mut [SchemaNode<'arena>],
    index: usize,
}

impl<'ser, 'arena, 'w, B: TermWriter> ser::SerializeStructVariant
    for StructVariantSerializer<'ser, 'arena, 'w, B>
{
    type Ok = SchemaNode<'arena>;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.fields[self.index + 0] = SchemaNode::const_from(key);
        self.fields[self.index + 1] = value.serialize(&mut *self.serializer)?;
        self.index += 2;
        Ok(())
    }

    fn end(self) -> Result<SchemaNode<'arena>, Error> {
        Ok(SchemaNode::Compound(SchemaCompound::StructVariant(
            self.variant,
            self.fields,
        )))
    }
}
