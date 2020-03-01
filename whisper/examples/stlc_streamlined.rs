use ::{
    serde::{Deserialize, Serialize},
    whisper::{constants, prelude::*, session::DebugHandler},
};

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

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

whisper::knowledge_base! {
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

constants! {
    pub enum Kw {
        context,
        proves,
        is_type,
    }
}

// whisper::query! {
//     fn stlc_infer(term: &IrNode);

//     // Note that if the line below weren't commented out, #term is
//     // parsed as `(#term)` because it is of type `IrNode` and
//     // therefore `query!` has to wrap it in a single-arity tuple.
//     //
//     // where Term is #term,
//     context { 1: Ligma } proves #term is_type Tau;
// }

#[derive(Debug)]
pub struct Typechecker {
    machine: Machine<()>,
    heap: Heap,
    knowledge_base: KnowledgeBase,
    handler: DebugHandler<KnowledgeBase>,
}

impl Typechecker {
    pub fn new() -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let modules = stlc(&mut terms);
        let knowledge_base = whisper::trans::knowledge_base(&terms, &modules);
        println!(
            "Compiled knowledge base:\n{}",
            knowledge_base.root().display()
        );

        let mut machine = Machine::new();
        machine.init(&knowledge_base);

        let heap = Heap::new(symbols);
        let handler = DebugHandler::default();

        Typechecker {
            machine,
            heap,
            knowledge_base,
            handler,
        }
    }

    pub fn infer(&mut self, term: &Term) -> Option<Type> {
        let mut runtime = Runtime::new(
            &mut self.machine,
            &mut self.heap,
            &mut self.handler,
            &self.knowledge_base,
        );

        let maybe_solution = runtime
            .solve_once((&(
                Kw::context,
                map!(1 => Variable::<Type>::Free(0)),
                Kw::proves,
                term.clone(),
                Kw::is_type,
                Variable::<Type>::Free(1),
            ),))
            .expect("fuck");
        println!("hell yea {:?}", maybe_solution);
        let ((_, _, _, _, _, tau),) = maybe_solution?;

        match tau {
            Variable::Bound(it) => Some(it),
            _ => None,
        }
    }
}

fn main() {
    let mut typechecker = Typechecker::new();
    let term = Term::App(Box::new((
        Term::Lam(0, Type::Base(132), Box::new(Term::Var(0))),
        Term::Var(1),
    )));

    println!("Whisper input: {:?}", term);
    let ty = typechecker.infer(&term);
    println!("Whisper output: {:?}", ty);
}
