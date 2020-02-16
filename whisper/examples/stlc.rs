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
        let mut builder = QueryBuilder::new(Heap::new(self.terms.symbols().clone()));
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
