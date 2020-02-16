use ::{
    derive_more::From,
    failure::Fail,
    std::{fs::File, io::Read, path::Path},
    whisper::{knowledge_base::PortableKnowledgeBase, prelude::*},
};

#[derive(Debug, Fail, From)]
pub enum ErrorKind {
    #[fail(display = "IO error: `{}`", _0)]
    Io(#[cause] std::io::Error),

    #[fail(display = "error constructing Whisper IR: `{}`", _0)]
    Ir(#[cause] whisper::ir::IrError),

    #[fail(display = "error serializing/deserializing: `{}`", _0)]
    Bincode(#[cause] bincode::Error),
}

pub fn compile_knowledge_base<P, Q>(input: &P, output: &Q) -> Result<(), ErrorKind>
where
    P: AsRef<Path> + ?Sized,
    Q: AsRef<Path> + ?Sized,
{
    let mut string = String::new();
    let mut file = File::open(input.as_ref())?;
    file.read_to_string(&mut string)?;

    let symbols = SymbolTable::new();
    let mut terms = IrTermGraph::new(symbols.clone());
    let ir_kb = terms.parse_knowledge_base_str(&string)?;

    let compiled_kb = whisper::trans::knowledge_base(&terms, &ir_kb);
    let mut file = File::create(output.as_ref())?;
    let serialized_kb = PortableKnowledgeBase::from_knowledge_base(&compiled_kb);
    bincode::serialize_into(&mut file, &serialized_kb)?;

    Ok(())
}
