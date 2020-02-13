**Whisper is very much work-in-progress and still being designed. It is not in any state for contributions (probably?) except maybe by chatting with me cuz that sounds like fun.**

# Whisper: a Logic Programming DSL for Rust

Whisper is a small, embeddable logic programming language in pure Rust. The `whisper` and `whisper_ir` crates
include functionality for easily constructing Whisper syntax and compiling it into bytecode
for execution. It supports a simplistic foreign function interface for calling into external
code, as well as facilities for supporting backtracking for external goals. This makes it
possible to use Whisper for doing a sort of traced proof-search, where if Whisper finds a
solution, you can use external goals to track what steps it took to get there, and react
accordingly.

Additionally through Serde, Whisper supports converting Rust structs to and from its internal term
representation. This means Whisper can be used for doing unification and general reasoning about
Rust terms, and then extracting the result back into native Rust.

Due to its structure as a logic programming language, it can also work well as an embedded read-heavy
database. Writing would require recompiling a knowledge base, which is currently not terribly
performant since emitting a compiled heap means reading through the entire knowledge base. This could
be improved through incremental compilation but is not a high priority.

## Motivation

Logic programming is a very convenient system for problems which need inference, but mixing
multiple programming languages can be very inconvenient. Whisper is intended to serve as a
simple, embeddable, and usably performant solution for embedding logic programs in Rust.

Whisper is *not* intended as:
- A super-fast, feature-rich Pro/Hilog implementation which just happens to have good Rust interop.
- A database capable of streaming to/from disk.
- A super-small interpreter with a tiny dependency footprint.
- A fully-fledged language of its own, which can run without any need for a host program.

## Features

- Simple, human-readable syntax, with limited support for scoping constants
- A simple, easy to build and modify intermediate representation (IR)
- Macros for generating Whisper syntax programmatically, with quasiquoting support
- A parser for parsing files containing Whisper syntax
- Simple but powerful interface for external goals written in Rust, with support for properly handling
  backtracking
- Simple but powerful interface for external datatypes, with support for handling unifying two external
  values against each other

## Example: Typechecker for the Simply Typed Lambda Calculus

Taken from `whisper/examples/stlc.rs`.

```rust
use ::{
    serde::{Deserialize, Serialize},
    whisper::{
        builder::QueryBuilder,
        ir::{IrNode, IrTermGraph},
        session::DebugHandler,
        Heap, Session, Symbol,
    },
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Base(i32),
    Fun(Box<(Type, Type)>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Term {
    Const(i32, Type),
    Var(i32),
    Lam(i32, Type, Box<Term>),
    App(Box<(Term, Term)>),
}

whisper::module! {
    fn stlc();

    context Gamma proves Term is_type Sigma if
        context Gamma proves Term is_type Sigma in extern,
        fail;

    // Rule 1.) Variables can be typed if they are in the context.
    context { X: Sigma | Gamma } proves ("Var" : X) is_type Sigma;
    context { Y: Tau   | Gamma } proves ("Var" : X) is_type Sigma if
        context Gamma proves ("Var" : X) is_type Sigma;

    // Rule 2.) Constants are automatically typed.
    context Gamma proves ("Const" : (C Tau)) is_type Tau;

    // Rule 3.) If adding `X: Sigma` to the context gives `E` some type `Tau`,
    // then the lambda expression `\X. E` can be typed as a function
    // `Sigma -> Tau`.
    context Gamma proves ("Lam" : (X Sigma E)) is_type ("Fun" : (Sigma Tau)) if
        context { X: Sigma | Gamma } proves E is_type Tau;

    // Rule 4.) If `E1` is a function `Sigma -> Tau` and `E2` is `Sigma`, then
    // applying `E1` to `E2` gets us a term of type `Tau`.
    context Gamma proves ("App" : (E1 E2)) is_type Tau if
        context Gamma proves E1 is_type ("Fun" : (Sigma Tau)),
        context Gamma proves E2 is_type Sigma;

    context Gamma proves Term is_type Sigma if
        context Gamma proves Term is_type Sigma failed in extern,
        fail;
}

whisper::query! {
    fn stlc_infer(term: &IrNode);

    // Note that if the line below weren't commented out, #term is
    // parsed as `(#term)` because it is of type `IrNode` and
    // therefore `query!` has to wrap it in a single-arity tuple.
    //
    // where Term is #term,
    context { 1: Ligma } proves #term is_type Tau;
}

#[derive(Debug)]
pub struct Typechecker {
    terms: IrTermGraph,
    root: Symbol,
    session: Session<DebugHandler>,
}

impl Typechecker {
    pub fn new() -> Self {
        use whisper::{ir::IrKnowledgeBase, SymbolTable};
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let mut modules = IrKnowledgeBase::new(symbols.clone());

        let stlc_module = modules.new_named_module_with_root(Symbol::PUBLIC);
        stlc(&mut terms, &mut modules, stlc_module);
        modules.link(&mut terms, stlc_module);

        let root = modules[stlc_module].get_root().clone();

        let stlc_kb = whisper::trans::knowledge_base(&terms, &modules);
        println!(
            "Compiled knowledge base:\n{}",
            stlc_kb.get(Symbol::PUBLIC_INDEX).unwrap().display()
        );
        let stlc_session = Session::new(symbols.clone(), stlc_kb.into());

        Typechecker {
            terms,
            root,
            session: stlc_session,
        }
    }

    pub fn infer(&mut self, term: &Term) -> Option<Type> {
        let mut builder = QueryBuilder::new(Heap::new(self.terms.symbol_table().clone()));
        let term_addr = builder.bind(term);
        let ir_query = stlc_infer(&mut self.terms, &self.root, &term_addr);
        let query = builder.finish(&self.terms, &ir_query);

        self.session.load(query.into());

        if self.session.resume() {
            let addr = self.session.query_vars()[&"Tau".into()];
            Some(whisper_schema::serde::de::from_reader(self.session.heap().read_at(addr)).unwrap())
        } else {
            None
        }
    }
}

fn main() {
    let mut typechecker = Typechecker::new();
    let term = Term::App(Box::new((
        Term::Lam(0, Type::Base(132), Box::new(Term::Var(0))),
        Term::Var(1),
    )));
    let ty = typechecker.infer(&term);

    println!("Whisper input: {:?}", term);
    println!("Whisper output: {:?}", ty);
}
```

## Example: Configuration Validator

See `whisper_std/tests/foo_schema.rs` for the test files as well.

```rust
use ::{
    failure::Error,
    im,
    serde::{
        de::{DeserializeOwned, Deserializer},
        Deserialize,
    },
    serde_json::Value,
    std::marker::PhantomData,
    whisper::{prelude::*, session::DebugHandler},
};

#[derive(Debug, Clone)]
pub struct Schema(SharedKnowledgeBase);

impl Schema {
    pub fn from_str<S: AsRef<str> + ?Sized>(string: &S) -> Self {
        let mut terms = IrTermGraph::new(SymbolTable::new());
        let ir_kb = terms.parse_knowledge_base_str(string).expect("oops");
        Self(whisper::trans::knowledge_base(&terms, &ir_kb).into())
    }

    pub fn from_embedded(embedded: fn(&mut IrTermGraph) -> IrKnowledgeBase) -> Self {
        let mut terms = IrTermGraph::new(SymbolTable::new());
        let ir_kb = embedded(&mut terms);
        Self(whisper::trans::knowledge_base(&terms, &ir_kb).into())
    }
}

whisper::query! {
    fn validator_query(input: IrNode);

    valid #input;
}

#[derive(Debug)]
pub struct Validator<T: DeserializeOwned> {
    symbols: SymbolTable,
    session: Session<DebugHandler>,
    _phantom: PhantomData<T>,
}

impl<T: DeserializeOwned> Validator<T> {
    pub fn new(schema: &Schema) -> Self {
        Self {
            symbols: schema.0.symbol_table().clone(),
            session: Session::new(schema.0.symbol_table().clone(), schema.0.clone()),
            _phantom: PhantomData,
        }
    }

    fn build_validator_query(&self, value: &Value) -> SharedQuery {
        let mut terms = IrTermGraph::new(self.symbols.clone());
        let mut builder = QueryBuilder::new(Heap::new(self.symbols.clone()));
        let bound = builder.bind(value);
        let ir_query = validator_query(&mut terms, &Symbol::MOD, bound);
        SharedQuery::from(builder.finish(&terms, &ir_query))
    }

    pub fn deserialize_validated<'de, D: Deserializer<'de>>(
        &mut self,
        deserializer: D,
    ) -> Result<T, Error> {
        let serializer = serde_json::value::Serializer;
        let json_value = serde_transcode::transcode(deserializer, serializer).unwrap();
        self.session.load(self.build_validator_query(&json_value));

        if self.session.resume() {
            Ok(serde_json::from_value(json_value)?)
        } else {
            failure::bail!("Failed to validate!");
        }
    }
}

whisper::knowledge_base! {
    fn foo_schema();

    // If TLS is disabled, we dgaf about the key/cert.
    valid Foo if
        Foo matches { tls_enabled: false | _ } in std::map::match;

    // If TLS is enabled, we want to be sure that key/cert are both `Some`.
    valid Foo if
        Foo matches {
            tls_enabled: true,
            tls: {
                cert: _,
                key: _,
            }
        } in std::map::match;

    // Eventually we'll have some stuff for error reporting in the stdlib.
    // There are a number of ways to get data out of Whisper in a way that's
    // convenient for aggregating errors, but for now imagine this is a println.
    valid Foo if
        "failed to validate!" Foo in extern,
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

#[test]
fn validate_foo() -> Result<(), Error> {
    // First, we construct our validator from the knowledge base we made above.
    let mut schema = Schema::from_embedded(foo_schema);

    let import_point = schema.0.symbol_table().normalize(Name {
        root: Symbol::MOD,
        path: im::vector![Atom::from("std"), Atom::from("map")],
    });

    schema
        .0
        .to_mut()
        .import_serialized(&import_point, whisper_std::map());

    let mut validator = Validator::<FooConfig>::new(&schema);

    // Now we can try to validate a few configs! Normally you'd be reading things
    // in from a file or something but in the interest of simplicity here we'll
    // just `include_str!` them.
    let foo_config_a = include_str!("foo_schema/foo_ok.toml");
    let foo_config_b = include_str!("foo_schema/foo_bad.toml");
    let foo_config_c = include_str!("foo_schema/foo_also_ok.toml");

    // Cool... Let's try to deserialize these! We'll start with `foo_config_a`.
    let val = validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_a))?;
    println!("Valid: {:#?}", val);

    // Okay... so far so good, let's try `foo_config_b`.
    match validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_b)) {
        Ok(val) => println!("All good! Also valid: {:#?}", val),
        Err(err) => println!("Oh no! {}", err),
    }

    // Oops, that last one failed! Alright, last but not least, let's try `foo_config_c`.
    let val = validator.deserialize_validated(&mut toml::Deserializer::new(foo_config_c))?;
    println!("Valid: {:#?}", val);

    // And that one passes with flying colors.

    Ok(())
}
```

## TODO

Whisper is extremely a work in progress. It's still being designed and syntax and semantics are changing
regularly, though never too drastically. Documentation is in flux and may be old/wrong. There are a lot
of things that need to be done before a release can even be considered:

- Documentation; an eventual goal is `#[deny(missing_docs)]`.
- Language improvements:
    - Figure out a standard library which assists with reasoning about Rust structures
    - Implement `let X = <EXPRESSION>` evaluation (currently panics)
    - Error handling: lots of unwraps and `todo!()`s. Should be replaced with `failure`
        derived error types
    - Figure out a way to make imports easy from the point of view of someone integrating
        Whisper with their program
- I'm sure there's more but I can't think of it at the moment.
