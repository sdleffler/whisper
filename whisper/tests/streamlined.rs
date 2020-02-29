use ::{
    failure::Error,
    serde::{Deserialize, Serialize},
    whisper::{constant, constants, prelude::*, session::DebugHandler},
};

whisper::knowledge_base! {
    fn foo_schema();

    mod without {
        {K: V | KVs} without_key K with_value V gives KVs;
        {L: Q | LQs} without_key K with_value V gives {L: Q | KVs} if
            LQs without_key K with_value V gives KVs in self;
    }

    mod match {
        {K: V | KVs} matches OtherKVs if
            KVs matches KVs_ in self,
            K matches K_ in self,
            V matches V_ in self,
            OtherKVs without_key K_ with_value V_ gives KVs_ in super::without;
        X matches X;
    }

    // If TLS is disabled, we dgaf about the key/cert.
    valid Foo if
        Foo matches { tls_enabled: false | _ } in match;

    // If TLS is enabled, we want to be sure that key/cert are both `Some`.
    valid Foo if
        Foo matches {
            tls_enabled: true,
            tls: ("Some" : {
                cert: ("Some" : _),
                key: ("Some" : _),
            })
        } in match;

    // Eventually we'll have some stuff for error reporting in the stdlib.
    // There are a number of ways to get data out of Whisper in a way that's
    // convenient for aggregating errors, but for now imagine this is a println.
    valid Foo if
        "failed to validate!" Foo in extern,
        error "TODO: add some stdlib stuff so that we can report errors!";
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TlsConfig {
    cert: Option<String>,
    key: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FooConfig {
    tls: Option<TlsConfig>,
    tls_enabled: bool,
}

constants! {
    enum Keywords {
        valid,
    }
}

#[test]
fn query_foo() -> Result<(), Error> {
    let symbol_table = SymbolTable::new();
    let mut terms = IrTermGraph::new(symbol_table.clone());
    let modules = foo_schema(&mut terms);

    let mut machine = Machine::new();
    let mut heap = Heap::new(symbol_table.clone());
    let mut handler = DebugHandler::default();
    let resolver = whisper::trans::knowledge_base(&terms, &modules);
    machine.init(&resolver);

    let solution = {
        let mut runtime = Runtime::new(&mut machine, &mut heap, &mut handler, &resolver);
        runtime.solve_once((&(
            constant!(valid),
            FooConfig {
                tls: None,
                tls_enabled: false,
            },
        ),))?
    };

    assert!(solution.is_some());

    let solution = {
        let mut runtime = Runtime::new(&mut machine, &mut heap, &mut handler, &resolver);
        runtime.solve_once((&(
            Keywords::valid,
            FooConfig {
                tls: Some(TlsConfig {
                    cert: Some("cert!".to_string()),
                    key: None,
                }),
                tls_enabled: false,
            },
        ),))?
    };

    assert!(solution.is_some());

    let solution = {
        let mut runtime = Runtime::new(&mut machine, &mut heap, &mut handler, &resolver);
        runtime.solve_once((&(
            constant!(valid),
            FooConfig {
                tls: Some(TlsConfig {
                    cert: Some("cert!".to_string()),
                    key: None,
                }),
                tls_enabled: true,
            },
        ),))?
    };

    assert!(solution.is_none());

    let solution = {
        let mut runtime = Runtime::new(&mut machine, &mut heap, &mut handler, &resolver);
        runtime.solve_once((&(
            constant!(valid),
            FooConfig {
                tls: Some(TlsConfig {
                    cert: Some("cert!".to_string()),
                    key: Some("key!".to_string()),
                }),
                tls_enabled: true,
            },
        ),))?
    };

    assert!(solution.is_some());
    Ok(())
}
