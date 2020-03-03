use whisper::{
    prelude::*,
    session::{ExternContext, ExternHandler, Unfolded},
};

whisper::knowledge_base! {
    fn test();

    iterate if
        foo in extern;
}

struct IterateState {
    i: u32,
}

#[derive(Default)]
struct IterateHandler(Vec<u32>);

impl ExternHandler for IterateHandler {
    type State = IterateState;
    type Resolver = KnowledgeBase;

    fn init_state(&mut self, _ctx: ExternContext<Self::Resolver>) -> Self::State {
        IterateState { i: 0 }
    }

    fn handle_goal(
        &mut self,
        _ctx: ExternContext<Self::Resolver>,
        state: &mut IterateState,
    ) -> Unfolded {
        if state.i < 10 {
            println!("Iterating: {}", state.i);
            self.0.push(state.i);
            state.i += 1;
            Unfolded::Succeed {
                next: None,
                empty: false,
            }
        } else {
            Unfolded::Succeed {
                next: None,
                empty: true,
            }
        }
    }
}

#[test]
fn iterate() {
    let mut session = SimpleSession::<IterateHandler>::new(SymbolTable::new(), test);
    session
        .run()
        .solve_all((&(String::from("iterate"),),))
        .expect("oops");
    assert_eq!(session.handler.0, (0..10).collect::<Vec<_>>());
}
