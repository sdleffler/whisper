use ::{
    derive_more::Display,
    failure::Fail,
    im::Vector,
    proc_macro2::Span,
    std::{cell::RefCell, collections::HashMap, mem},
    syn::{
        braced, bracketed,
        ext::IdentExt,
        parenthesized,
        parse::{discouraged::Speculative, Lookahead1, Parse, ParseStream},
        token::{Brace, Bracket, Paren},
        Error, Ident as SynIdent, LitFloat, LitInt, LitStr, Result, Token, Type,
    },
};

use crate::{
    graph::{
        IrCompoundKind, IrGoal, IrKnowledgeBase, IrModuleEntry, IrModuleRef, IrNode, IrQuery,
        IrRef, IrRelation, IrTermGraph,
    },
    Ident, Name, SymbolIndex, SymbolTable, Var,
};

#[cfg(test)]
mod tests;

#[derive(Debug, Fail)]
#[fail(display = "parse error: {}", syn_err)]
pub struct ParseError {
    pub src: String,
    pub syn_err: syn::Error,
}

// Grrrrrr, `syn::parse::Parse` doesn't let you keep local state. I'll get you for this some day,
// dtolnay. Just you wait. >:V
//
// (not really, but if you're reading this i'll bribe you with food/drink to implement this or add a
// public interface to constructing a `ParseStream` from a `TokenStream`... please? let me know
// and i'll treat you at the next Rust meetup ;_;)
std::thread_local! {
    static FN_IDENT: RefCell<SynIdent> = RefCell::new(SynIdent::new("AllCatsAreBeautiful", Span::call_site()));
    static QUASIS: RefCell<HashMap<SynIdent, QuasiTypeKind>> = RefCell::new(HashMap::new());
    static GRAPHS: RefCell<(IrKnowledgeBase, IrTermGraph)> = {
        let symbol_table = SymbolTable::new();
        RefCell::new((IrKnowledgeBase::new(symbol_table.clone()), IrTermGraph::new(symbol_table.clone())))
    };
    static MODULE: RefCell<Option<IrModuleRef>> = RefCell::new(None);
}

fn with_graphs<F, T>(f: F) -> T
where
    F: FnOnce(&mut IrKnowledgeBase, &mut IrTermGraph) -> T,
{
    GRAPHS.with(|refcell| {
        let (ref mut modules, ref mut terms) = &mut *refcell.borrow_mut();
        f(modules, terms)
    })
}

pub fn use_graphs(new_modules: IrKnowledgeBase, new_terms: IrTermGraph) {
    with_graphs(|modules, terms| {
        *modules = new_modules;
        *terms = new_terms;
    })
}

pub fn swap_graphs() -> (IrKnowledgeBase, IrTermGraph) {
    with_graphs(|modules, terms| {
        let new_terms = IrTermGraph::new(terms.symbols().clone());
        let new_modules = IrKnowledgeBase::new(modules.symbols().clone());

        (
            mem::replace(modules, new_modules),
            mem::replace(terms, new_terms),
        )
    })
}

pub fn swap_terms() -> IrTermGraph {
    with_graphs(|_, terms| {
        let new_terms = IrTermGraph::new(terms.symbols().clone());
        mem::replace(terms, new_terms)
    })
}

pub fn swap_knowledge_base() -> IrKnowledgeBase {
    with_graphs(|modules, _| {
        let new_modules = IrKnowledgeBase::new(modules.symbols().clone());
        mem::replace(modules, new_modules)
    })
}

fn new_compound(kind: IrCompoundKind, args: impl Into<Vector<IrNode>>) -> IrRef {
    with_graphs(|_, terms| terms.new_compound(kind, args))
}

fn new_let(lhs: IrRef, rhs: IrRef) -> IrRef {
    with_graphs(|_, terms| {
        terms.new_opaque(im::vector![
            IrNode::Const(SymbolIndex::LET.into()),
            IrNode::Ref(lhs),
            IrNode::Ref(rhs),
        ])
    })
}

fn new_is(lhs: IrRef, rhs: IrRef) -> IrRef {
    with_graphs(|_, terms| {
        terms.new_opaque(im::vector![
            IrNode::Const(SymbolIndex::IS.into()),
            IrNode::Ref(lhs),
            IrNode::Ref(rhs),
        ])
    })
}

fn new_try_in(term: IrRef, module: IrNode) -> IrRef {
    with_graphs(|_, terms| {
        terms.new_opaque(im::vector![
            IrNode::Const(SymbolIndex::TRY.into()),
            IrNode::Ref(term),
            module,
        ])
    })
}

fn new_cut() -> IrRef {
    with_graphs(|_, terms| terms.new_opaque(im::vector![IrNode::Const(SymbolIndex::CUT.into())]))
}

pub(crate) fn set_module(ir_mod: IrModuleRef) {
    MODULE.with(|module| module.replace(Some(ir_mod)));
}

fn get_module() -> IrModuleRef {
    MODULE.with(|module| module.borrow().as_ref().copied().unwrap())
}

fn is_keyword(lookahead: &Lookahead1) -> bool {
    lookahead.peek(Token![if])
        || lookahead.peek(kw::is)
        || lookahead.peek(Token![use])
        || lookahead.peek(Token![in])
        || lookahead.peek(Token![extern])
        || lookahead.peek(Token![yield])
        || lookahead.peek(Token![mod])
        || lookahead.peek(Token![self])
        || lookahead.peek(Token![super])
}

fn is_name_start(lookahead: &Lookahead1) -> bool {
    (lookahead.peek(SynIdent::peek_any)
        && (!is_keyword(lookahead)
            || lookahead.peek(Token![self])
            || lookahead.peek(Token![super])))
        || lookahead.peek(LitStr)
        || lookahead.peek(Token![<])
}

fn is_term_start(lookahead: &Lookahead1) -> bool {
    is_name_start(lookahead)
        || lookahead.peek(LitInt)
        || lookahead.peek(Paren)
        || lookahead.peek(Token![#])
        || lookahead.peek(Token![-])
        || lookahead.peek(Bracket)
        || lookahead.peek(Brace)
        || lookahead.peek(Token![!])
}

fn is_compound_start(lookahead: &Lookahead1) -> bool {
    is_term_start(lookahead)
}

fn is_goal_start(lookahead: &Lookahead1) -> bool {
    is_compound_start(lookahead)
}

fn is_head_start(lookahead: &Lookahead1) -> bool {
    is_compound_start(lookahead)
}

mod kw {
    syn::custom_keyword!(is);
    syn::custom_keyword!(i);
    syn::custom_keyword!(u);
    syn::custom_keyword!(f);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum QuasiTypeKind {
    #[display(fmt = "IrNode")]
    Term,
    #[display(fmt = "IrGoal")]
    Goal,
    #[display(fmt = "IrRelation")]
    Relation,
    #[display(fmt = "Scope")]
    Scope,
}

#[derive(Clone)]
pub struct QuasiArg {
    pub ident: SynIdent,
    pub ty: Type,
    pub kind: QuasiTypeKind,
}

impl Parse for QuasiArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse()?;
        let kind = {
            // It's time to get to the bottom of this.
            let mut unwrapped_ty = &ty;
            while let Type::Reference(ty_ref) = unwrapped_ty {
                unwrapped_ty = &ty_ref.elem;
            }

            match unwrapped_ty {
                Type::Path(t) if t.path.is_ident("IrNode") => QuasiTypeKind::Term,
                Type::Path(t) if t.path.is_ident("IrGoal") => QuasiTypeKind::Goal,
                Type::Path(t) if t.path.is_ident("IrRelation") => QuasiTypeKind::Relation,
                Type::Path(t) if t.path.is_ident("Scope") => QuasiTypeKind::Scope,
                Type::Path(t) if t.path.get_ident().is_some() => {
                    let ident = t.path.get_ident().unwrap();
                    return Err(Error::new_spanned(&ident, format!("expected `IrNode`, `IrGoal`, or `IrRelation`; `{}` is not a valid type for quasiquoting", ident)));
                }
                Type::Reference(_) => unreachable!("dude, we just got rid of you."),
                _ => {
                    return Err(Error::new_spanned(
                        &ty,
                        format!("unable to parse quasiquoted argument type `{:?}`", ty),
                    ))
                }
            }
        };

        Ok(Self { ident, ty, kind })
    }
}

// TODO: Figure out how expensive cloning an `Ident` is.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QuasiVar(pub SynIdent);

#[doc(hidden)]
impl Parse for Name {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if is_name_start(&lookahead) {
            let expect_end_bracket;
            if lookahead.peek(Token![<]) {
                input.parse::<Token![<]>()?;
                expect_end_bracket = true;
            } else {
                expect_end_bracket = false;
            };

            let lookahead = input.lookahead1();
            let mut name = if lookahead.peek(Token![self]) {
                input.parse::<Token![self]>()?;
                let root = with_graphs(|modules, _| modules[get_module()].root().clone());
                Name {
                    root,
                    path: Vector::new(),
                }
            } else if lookahead.peek(Token![super]) {
                input.parse::<Token![super]>()?;
                let root = with_graphs(|modules, _| modules[get_module()].root().clone());
                Name {
                    root,
                    path: im::vector![ident_internal!("super")],
                }
            } else {
                let ident = if lookahead.peek(LitStr) {
                    let lit_str = input.parse::<LitStr>()?;
                    lit_str.value().into()
                } else {
                    Ident::from(&*input.call(SynIdent::parse_any)?.to_string())
                };

                Name {
                    root: SymbolIndex::PUBLIC,
                    path: im::vector![ident],
                }
            };

            while input.peek(Token![::]) {
                input.parse::<Token![::]>()?;

                let ident = if input.peek(LitStr) {
                    let lit_str = input.parse::<LitStr>()?;
                    lit_str.value().into()
                } else {
                    Ident::from(&*input.call(SynIdent::parse_any)?.to_string())
                };

                name.path.push_back(ident);
            }

            if expect_end_bracket {
                input.parse::<Token![>]>()?;
            }

            Ok(name)
        } else {
            Err(input.error("expected a name"))
        }
    }
}

#[doc(hidden)]
impl Parse for IrNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if input.peek(Token![#]) {
            input.parse::<Token![#]>()?;
            let ident = input.call(SynIdent::parse)?;
            QUASIS.with(|quasis| match quasis.borrow().get(&ident) {
                Some(QuasiTypeKind::Term) => Ok(IrNode::Quasi(QuasiVar(ident))),
                Some(other) => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "quasiquotation variable `{}` is not a term, but an `{}`.",
                        ident, other
                    ),
                )),
                None => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "can't find quasiquotation variable with identifier `{}`",
                        ident
                    ),
                )),
            })
        } else if lookahead.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            Ok(IrNode::Ref(new_cut()))
        } else if lookahead.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            Ok(IrNode::Var(Var::Anonymous))
        } else if is_name_start(&lookahead) {
            let forked = input.fork();
            if forked.peek(SynIdent::peek_any) {
                let ident = Ident::from(&*forked.call(SynIdent::parse_any)?.to_string());
                if ident.str_ref().starts_with(char::is_uppercase) {
                    input.advance_to(&forked);
                    return Ok(IrNode::Var(Var::Named(ident)));
                }
            }

            let name = input.parse::<Name>()?;
            Ok(IrNode::Const(name))
        } else if lookahead.peek(LitInt) || lookahead.peek(Token![-]) || lookahead.peek(LitFloat) {
            // BIG todo: fix number parsing so that suffixes are checked/ensured not there.
            let negative = if lookahead.peek(Token![-]) {
                Some(input.parse::<Token![-]>()?)
            } else {
                None
            };

            let lookahead = input.lookahead1();
            if lookahead.peek(LitFloat) {
                let float = input.parse::<LitFloat>()?;
                if !(float.suffix() == "f32" || float.suffix() == "") {
                    Err(input.error(&format!(
                        "expected `f` suffix or no suffix, found `{}`",
                        float.suffix()
                    )))?;
                }

                Ok(IrNode::Float32(float.base10_parse()?))
            } else {
                let int = input.parse::<LitInt>()?;
                if !(int.suffix().is_empty()
                    || int.suffix() == "i"
                    || int.suffix() == "u"
                    || int.suffix() == "f")
                {
                    Err(Error::new_spanned(&int, format!("look, i'm running out of caffeine here. \
                        the suffixes are i, u, or f, not i32, u32, or f32 cuz they're all fucking 32-bit, \
                        okay? not, `{}`. what is this shit?", int.suffix())))?;
                }

                let lookahead = input.lookahead1();
                if lookahead.peek(kw::u) {
                    input.parse::<kw::u>()?;
                    if let Some(tok) = negative {
                        Err(syn::Error::new_spanned(
                            tok,
                            "negative unsigned integer makes no fucking sense at all",
                        ))?;
                    }

                    Ok(IrNode::UInt32(int.base10_parse()?))
                } else if lookahead.peek(kw::f) {
                    input.parse::<kw::f>()?;
                    Ok(IrNode::Float32(int.base10_parse()?))
                } else {
                    if lookahead.peek(kw::i) {
                        input.parse::<kw::i>()?;
                    }

                    let mut v = int.base10_parse()?;
                    if negative.is_some() {
                        v *= -1;
                    }

                    Ok(IrNode::Int32(v))
                }
            }
        } else if lookahead.peek(Paren) {
            let compound;
            parenthesized!(compound in input);

            // Try to parse as a tagged value, and if we fail, parse as a compound.
            // TODO: make this more efficient.
            if !compound.is_empty() {
                let forked = compound.fork();

                let maybe_tagged = forked
                    .parse::<IrNode>()
                    .and_then(|tag| {
                        forked.parse::<Token![:]>()?;
                        Ok(tag)
                    })
                    .and_then(|tag| {
                        let val = forked.parse::<IrNode>()?;
                        if forked.is_empty() {
                            compound.advance_to(&forked);
                            Ok((tag, val))
                        } else {
                            Err(forked.error("expected end of tagged value"))
                        }
                    });

                match maybe_tagged {
                    Ok((tag, val)) => {
                        with_graphs(|_, terms| Ok(IrNode::Ref(terms.new_tagged(tag, val))))
                    }
                    Err(err) => Ok(IrNode::Ref(compound.parse().map_err(|mut comp_err| {
                        comp_err.combine(err);
                        comp_err
                    })?)),
                }
            } else {
                Ok(IrNode::Ref(compound.parse()?))
            }
        } else if lookahead.peek(Bracket) {
            let list_terms;
            bracketed!(list_terms in input);

            let mut heads = Vec::new();
            let mut tail = None;
            loop {
                if list_terms.is_empty() {
                    let consed = with_graphs(|_, terms| {
                        let tail =
                            tail.unwrap_or(IrNode::Const(SymbolIndex::INTERNAL_LIST_NIL.into()));
                        heads
                            .into_iter()
                            .rev()
                            .fold(tail, |acc, head| IrNode::Ref(terms.new_cons(head, acc)))
                    });
                    break Ok(consed);
                }

                heads.push(list_terms.parse()?);

                if list_terms.peek(Token![,]) {
                    list_terms.parse::<Token![,]>()?;
                }

                if list_terms.peek(Token![|]) {
                    list_terms.parse::<Token![|]>()?;
                    tail = Some(list_terms.parse()?);
                }
            }
        } else if lookahead.peek(Brace) {
            let map_terms;
            braced!(map_terms in input);

            let mut heads = Vec::new();
            let mut tail = None;
            loop {
                if map_terms.is_empty() {
                    let consed = with_graphs(|_, terms| {
                        let tail =
                            tail.unwrap_or(IrNode::Const(SymbolIndex::INTERNAL_MAP_NIL.into()));
                        heads.into_iter().rev().fold(tail, |acc, (key, val)| {
                            IrNode::Ref(terms.new_cons2(key, val, acc))
                        })
                    });
                    break Ok(consed);
                }

                let key = map_terms.parse()?;
                map_terms.parse::<Token![:]>()?;
                let val = map_terms.parse()?;

                heads.push((key, val));

                if map_terms.peek(Token![,]) {
                    map_terms.parse::<Token![,]>()?;
                }

                if map_terms.peek(Token![|]) {
                    map_terms.parse::<Token![|]>()?;
                    tail = Some(map_terms.parse()?);
                }
            }
        } else {
            Err(input
                .error("expected a Whisper variable, constant, integer, struct, or extern term"))
        }
    }
}

#[doc(hidden)]
impl Parse for IrRef {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![let]) {
            input.parse::<Token![let]>()?;
            let lhs = input.parse()?;
            input.parse::<Token![=]>()?;
            let rhs = input.parse()?;
            return Ok(new_let(lhs, rhs));
        } else if lookahead.peek(Token![try]) {
            input.parse::<Token![try]>()?;
            let term = input.parse()?;
            input.parse::<Token![=]>()?;
            let rhs = input.parse()?;
            return Ok(new_is(term, rhs));
        }

        let mut args = Vector::<IrNode>::new();
        while is_term_start(&input.lookahead1()) {
            args.push_back(input.parse()?);
        }

        if input.peek(Token![in]) {
            input.parse::<Token![in]>()?;

            if input.peek(Token![extern]) {
                input.parse::<Token![extern]>()?;

                Ok(new_compound(IrCompoundKind::Extern, args))
            } else {
                let forked = input.fork();
                if forked.peek(SynIdent::peek_any) {
                    let ident = Ident::from(&*forked.call(SynIdent::parse_any)?.to_string());
                    if ident.str_ref().starts_with(char::is_uppercase) {
                        input.advance_to(&forked);
                        let var = IrNode::Var(Var::Named(ident));
                        return Ok(new_try_in(new_compound(IrCompoundKind::Struct, args), var));
                    }
                }

                let mut kb = input.parse::<Name>()?;
                if kb.root == SymbolIndex::PUBLIC {
                    kb.root = SymbolIndex::MOD;
                }

                Ok(new_try_in(
                    new_compound(IrCompoundKind::Struct, args),
                    IrNode::Const(kb),
                ))
            }
        } else {
            Ok(new_compound(IrCompoundKind::Struct, args))
        }
    }
}

#[doc(hidden)]
impl Parse for IrGoal {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut maybe_qv_err = None;
        if input.peek(Token![#]) {
            let forked = input.fork();

            forked.parse::<Token![#]>()?;
            let ident = forked.call(SynIdent::parse)?;
            let qv_res = QUASIS.with(|quasis| match quasis.borrow().get(&ident) {
                Some(QuasiTypeKind::Goal) => Ok(IrGoal::Quasi(QuasiVar(ident))),
                Some(other) => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "quasiquotation variable `{}` is not an `IrGoal`, but an `{}`.",
                        ident, other
                    ),
                )),
                _ => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "can't find quasiquotation variable with identifier `{}` and type `IrGoal`",
                        ident,
                    ),
                )),
            });

            match qv_res {
                Ok(qv) => {
                    input.advance_to(&forked);
                    return Ok(qv);
                }
                Err(qv_err) => maybe_qv_err = Some(qv_err),
            }
        }

        match (input.parse::<IrRef>().map(IrGoal::from), maybe_qv_err) {
            (Err(mut err), Some(qv_err)) => {
                err.combine(qv_err);
                Err(err)
            }
            (other, _) => other,
        }
    }
}

#[doc(hidden)]
impl Parse for IrRelation {
    fn parse(input: ParseStream) -> Result<Self> {
        let head = input.parse()?;
        let mut body = Vector::new();

        if input.peek(Token![if]) {
            input.parse::<Token![if]>()?;
            loop {
                if is_goal_start(&input.lookahead1()) {
                    body.push_back(input.parse()?);
                }

                if input.peek(Token![;]) {
                    break;
                } else {
                    input.parse::<Token![,]>()?;
                }
            }
        }

        input.parse::<Token![;]>().map_err(|_err| {
            with_graphs(|_, terms| {
                panic!("gotchu. {:?} if {:?}", terms.debug_ref(head), body);
            });
            _err
        })?;

        Ok(IrRelation { head, body })
    }
}

#[doc(hidden)]
impl Parse for IrModuleEntry {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut maybe_qv_err = None;
        if input.peek(Token![#]) {
            let forked = input.fork();

            forked.parse::<Token![#]>()?;
            let ident = forked.call(SynIdent::parse)?;
            let qv_res = QUASIS.with(|quasis| match quasis.borrow().get(&ident) {
                Some(QuasiTypeKind::Relation) => {
                    Ok(IrModuleEntry::Quasi(QuasiVar(ident)))
                }
                Some(other) => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "quasiquotation variable `{}` is not an `IrRelation`, but an `{}`.",
                        ident, other
                    ),
                )),
                _ => Err(Error::new_spanned(
                    &ident,
                    format!(
                        "can't find quasiquotation variable with identifier `{}` and type `IrRelation`",
                        ident,
                    ),
                )),
            });

            match qv_res {
                Ok(qv) => {
                    input.advance_to(&forked);
                    return Ok(qv);
                }
                Err(qv_err) => maybe_qv_err = Some(qv_err),
            }
        }

        let relation_res = input.parse().map(IrModuleEntry::Relation);
        match (relation_res, maybe_qv_err) {
            (Err(mut err), Some(qv_err)) => {
                err.combine(qv_err);
                Err(err)
            }
            (other, _) => other,
        }
    }
}

#[doc(hidden)]
impl Parse for IrQuery {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut goals = Vector::new();
        while is_compound_start(&input.lookahead1()) {
            goals.push_back(input.parse::<IrGoal>()?);

            if !input.peek(Token![,]) {
                break;
            } else {
                input.parse::<Token![,]>()?;
            }
        }

        if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;

            if !input.is_empty() {
                return Err(input.error("unexpected token after query"));
            }
        } else if !input.is_empty() {
            return Err(input.error("unexpected token in query"));
        }

        Ok(IrQuery::from(goals))
    }
}

#[doc(hidden)]
impl Parse for IrModuleRef {
    fn parse(input: ParseStream) -> Result<Self> {
        let module = get_module();
        // let mut exports = HashSet::new();
        let mut entries = Vector::new();

        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![pub]) {
                todo!("no longer accepting module export syntax");
            // input.parse::<Token![pub]>()?;

            // let root = if input.peek(Token![::]) {
            //     input.parse::<Token![::]>()?;
            //     Symbol::PUBLIC
            // } else {
            //     with_graphs(|modules, _| modules[module].root().clone())
            // };

            // let initial = input.parse::<UseTree>()?;
            // let mut stack = vec![(initial, Name::from(root))];
            // while let Some((node, mut name)) = stack.pop() {
            //     match node {
            //         UseTree::Path(UsePath { ident, tree, .. }) => {
            //             name.path.push_back(ident.to_string().into());
            //             stack.push((*tree, name));
            //         }
            //         UseTree::Name(UseName { ident }) => {
            //             name.path.push_back(ident.to_string().into());
            //             exports.insert(name);
            //         }
            //         UseTree::Group(UseGroup { items, .. }) => {
            //             for node in items {
            //                 stack.push((node, name.clone()));
            //             }
            //         }
            //         _ => panic!(),
            //     }
            // }

            // input.parse::<Token![;]>()?;
            } else if is_head_start(&lookahead) {
                entries.push_back(input.parse::<IrModuleEntry>()?);
            } else if lookahead.peek(Token![mod]) {
                input.parse::<Token![mod]>()?;
                let mut name = input.parse::<Name>()?;
                if name.root == SymbolIndex::PUBLIC {
                    name.root = SymbolIndex::MOD;
                }

                let child_module = with_graphs(|kb, _| {
                    name.root = kb[module].root();
                    let normalized_root = kb.symbols().write().insert_name(name);
                    kb.module(normalized_root)
                });

                let module_stream;
                braced!(module_stream in input);
                set_module(child_module);
                module_stream.parse::<IrModuleRef>()?;
            } else {
                break;
            }
        }

        with_graphs(|modules, _| {
            let module_mut = &mut modules[module];
            module_mut.entries = entries;
            // module_mut.exports = exports;
        });

        if !input.is_empty() {
            Err(input.error("unexpected token"))
        } else {
            Ok(module)
        }
    }
}

#[doc(hidden)]
impl Parse for IrKnowledgeBase {
    fn parse(input: ParseStream) -> Result<Self> {
        let module_ref = with_graphs(|kb, _| kb.module(SymbolIndex::MOD));

        while !input.is_empty() {
            set_module(module_ref);
            input.parse::<IrModuleRef>()?;
        }

        Ok(swap_knowledge_base())
    }
}

#[doc(hidden)]
pub struct QuasiquoteInput<P> {
    pub fn_ident: SynIdent,
    pub args: Vec<QuasiArg>,
    pub parsed: P,
}

#[doc(hidden)]
impl<P: Parse> Parse for QuasiquoteInput<P> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![fn]>()?;

        let fn_ident = input.parse::<SynIdent>()?;
        FN_IDENT.with(|old_fn_ident| old_fn_ident.replace(fn_ident.clone()));

        let content;
        parenthesized!(content in input);
        let args = content
            .parse_terminated::<_, Token![,]>(QuasiArg::parse)?
            .into_iter()
            .collect::<Vec<_>>();

        let arg_map = args.iter().fold(Ok(HashMap::new()), |map_res, quasi_arg| {
            let mut map = map_res?;
            if map.contains_key(&quasi_arg.ident) {
                Err(Error::new(
                    quasi_arg.ident.span(),
                    format!("multiple quasiquotation args named `{}`", quasi_arg.ident),
                ))
            } else {
                map.insert(quasi_arg.ident.clone(), quasi_arg.kind);
                Ok(map)
            }
        })?;

        QUASIS.with(|old_args| old_args.replace(arg_map));
        input.parse::<Token![;]>()?;
        let parsed: P = input.parse()?;

        Ok(QuasiquoteInput {
            fn_ident,
            args,
            parsed,
        })
    }
}
