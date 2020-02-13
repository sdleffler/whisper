use ::{
    failure::Error,
    std::{env, path::PathBuf},
};

fn main() -> Result<(), Error> {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    whisper_util::compile_knowledge_base("src/whisper/list.wh", &out_dir.join("list.kb"))?;
    whisper_util::compile_knowledge_base("src/whisper/map.wh", &out_dir.join("map.kb"))?;
    Ok(())
}
