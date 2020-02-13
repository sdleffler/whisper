use ::{failure::Error, serde::Deserialize, whisper_validate::Validator};

whisper::knowledge_base! {
    fn foo_schema();

    // TOML doesn't define an order for its tables, so we can't just match
    // on a map, since maps in Whisper are association lists. For now, we'll
    // go through field-by-field. There are more efficient ways to do this
    // but this is the simplest.
    field Field of { Field: Value | _    } matches Value;
    field Field of { _    : _     | More } matches Value if
        field Field of More matches Value;

    // If TLS is disabled, we dgaf about the key/cert. In Whisper, we can give
    // patterns (`valid Foo`) subgoals which have to be proven for the pattern
    // to fully match. It's sorta like match guards in Rust, but more powerful.
    valid Foo if
        field tls_enabled of Foo matches false;

    // If TLS is enabled, we want to be sure that key/cert are both `Some`.
    // In Whisper, we can have multiple goals for one match; for the match
    // to succeed, all the goals have to be proven true. Subgoals are proven
    // in the order they're written; this is important if you're doing things
    // with side effects (like, emitting errors, maybe?) but otherwise doesn't
    // usually matter.
    valid Foo if
        field tls_enabled of Foo matches true,
        field tls of Foo matches Tls,
        field cert of Tls matches _,
        field key of Tls matches _;

    // Eventually we'll have some stuff for error reporting in the stdlib.
    // There are a number of ways to get data out of Whisper in a way that's
    // convenient for aggregating errors, but for now imagine this is a println.
    valid Foo if
        failed to validate Foo in extern,
        error "TODO: add some stdlib stuff so that we can report errors!";
}

#[derive(Debug, Clone, Deserialize)]
pub struct TlsConfig {
    cert: Option<String>,
    key: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FooConfig {
    tls: Option<TlsConfig>,
    tls_enabled: bool,
}

fn main() -> Result<(), Error> {
    // First, we construct our validator from the knowledge base we made above.
    let mut validator = Validator::from_embedded(foo_schema);

    // Now we can try to validate a few configs! Normally you'd be reading things
    // in from a file or something but in the interest of simplicity here we'll
    // just `include_str!` them.
    let foo_config_a = include_str!("foo_schema/foo_ok.toml");
    let foo_config_b = include_str!("foo_schema/foo_bad.toml");
    let foo_config_c = include_str!("foo_schema/foo_also_ok.toml");

    // Cool... Let's try to deserialize these! We'll start with `foo_config_a`.
    let val = validator
        .deserialize_validated::<FooConfig, _>(&mut toml::Deserializer::new(foo_config_a))?;
    println!("Valid: {:#?}", val);

    // Okay... so far so good, let's try `foo_config_b`.
    match validator
        .deserialize_validated::<FooConfig, _>(&mut toml::Deserializer::new(foo_config_b))
    {
        Ok(val) => println!("All good! Also valid: {:#?}", val),
        Err(err) => println!("Oh no! {}", err),
    }

    // Oops, that last one failed! Alright, last but not least, let's try `foo_config_c`.
    let val = validator
        .deserialize_validated::<FooConfig, _>(&mut toml::Deserializer::new(foo_config_c))?;
    println!("Valid: {:#?}", val);

    // And that one passes with flying colors.

    Ok(())
}
