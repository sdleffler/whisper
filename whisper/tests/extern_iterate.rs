use ::{
    failure::Fail,
    std::sync::{Arc, Mutex},
    whisper::{
        knowledge_base::KbEntry,
        prelude::*,
        runtime::{ExternContext, ExternModule, Unfolded},
    },
};

whisper::knowledge_base! {
    fn test();

    iterate if
        in(iterator) foo,
        bar;

    bar if
        in(*) println "bar";
}

#[derive(Debug)]
struct IterateState {
    i: u32,
}

#[derive(Debug, Default, Clone)]
struct IterateHandler(Arc<Mutex<Vec<u32>>>);

impl ExternModule for IterateHandler {
    type State = IterateState;
    type Error = Box<dyn Fail>;

    fn init_state<R>(&self, _ctx: ExternContext<Self, R>) -> Self::State
    where
        R: Resolver<Self>,
    {
        IterateState { i: 0 }
    }

    fn handle_goal<R>(&self, ctx: ExternContext<Self, R>, state: &mut IterateState) -> Unfolded
    where
        R: Resolver<Self>,
    {
        if state.i < 10 {
            println!("Iterating: {}", state.i);
            self.0.lock().unwrap().push(state.i);
            state.i += 1;
            Unfolded::Succeed {
                next: ctx.trail[ctx.goal].next,
                empty: false,
            }
        } else {
            Unfolded::Fail
        }
    }
}

#[test]
fn iterate() {
    let mut session = SimpleSession::<IterateHandler>::new(SymbolTable::new(), test);
    let sneaky = Arc::new(Mutex::new(Vec::new()));
    let name = session
        .heap
        .symbols()
        .write()
        .insert_with_parent("iterator", SymbolIndex::MOD);
    let iterator_mod = IterateHandler(sneaky.clone());
    session
        .knowledge_base
        .insert(name, KbEntry::Extern(iterator_mod));
    session
        .run()
        .solve_all((&(String::from("iterate"),),))
        .expect("oops");
    assert_eq!(*sneaky.lock().unwrap(), (0..10).collect::<Vec<_>>());
}
