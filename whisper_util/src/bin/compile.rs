use ::{failure::Error, std::path::PathBuf, structopt::StructOpt};

#[derive(Debug, StructOpt)]
#[structopt(
    name = "whisperc",
    about = "Compile Whisper source to serialized bytecode format."
)]
struct CompileOpt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    #[structopt(parse(from_os_str))]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Error> {
    let opts = CompileOpt::from_args();
    let output = match opts.output {
        Some(it) => it,
        None => {
            let mut input_path = opts.input.clone();
            input_path.set_extension("kb");
            input_path
        }
    };
    let input = opts.input;

    if input == output {
        failure::bail!("Output and input paths must be different!");
    } else {
        whisper_util::compile_knowledge_base(&input, &output)?;
        Ok(())
    }
}
