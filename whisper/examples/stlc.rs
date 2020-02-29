use ::{
    serde::{Deserialize, Serialize},
    whisper::{prelude::*, session::DebugHandler},
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
    knowledge_base: KnowledgeBase,
    module_cache: ModuleCache,
    query: Query,
    session: Session<()>,
}

impl Typechecker {
    pub fn new() -> Self {
        let symbols = SymbolTable::new();
        let mut terms = IrTermGraph::new(symbols.clone());
        let mut modules = IrKnowledgeBase::new(symbols.clone());

        let stlc_module = modules.module(SymbolIndex::MOD);
        stlc(&mut terms, &mut modules, stlc_module);

        let knowledge_base = whisper::trans::knowledge_base(&terms, &modules);
        println!(
            "Compiled knowledge base:\n{}",
            knowledge_base.root().display()
        );
        let session = Session::new(symbols.clone());
        let mut module_cache = ModuleCache::new();
        module_cache.init(&knowledge_base);
        let query = Query::new(symbols.clone());

        Typechecker {
            terms,
            knowledge_base,
            module_cache,
            query,
            session,
        }
    }

    pub fn infer(&mut self, term: &Term) -> Option<Type> {
        self.query.clear();
        let mut builder = QueryBuilder::from(&mut self.query);
        let term_addr = builder.bind(term);
        let ir_query = stlc_infer(&mut self.terms, SymbolIndex::MOD, &term_addr);
        builder.push(&self.terms, &ir_query);
        builder.finish();

        self.session.load_with_extern_state_and_reuse_query(
            &mut self.query,
            &self.module_cache,
            (),
        );

        if self
            .session
            .resume_with_context(
                &mut self.module_cache,
                &mut DebugHandler::default(),
                &self.knowledge_base,
            )
            .is_solution()
        {
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

    println!("Whisper input: {:?}", term);
    let ty = typechecker.infer(&term);
    println!("Whisper output: {:?}", ty);
}
