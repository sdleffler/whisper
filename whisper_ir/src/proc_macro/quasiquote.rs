use ::{
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    syn::{Ident as SynIdent, Result},
};

use crate::{
    graph::{
        IrCompoundKind, IrGoal, IrKnowledgeBase, IrModule, IrModuleEntry, IrModuleRef, IrNode,
        IrQuery, IrRef, IrRelation, IrTermGraph,
    },
    parse::{self, QuasiArg, QuasiVar, QuasiquoteInput},
    Ident, Name, Symbol, SymbolTable, Var,
};

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let str_component = self.str_ref();
        let id_component = self.id();
        quote!(Ident::from_parts(#str_component.into(), #id_component)).to_tokens(tokens);
    }
}

impl ToTokens for Symbol {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        assert!(
            self.get_scope().is_reserved(),
            "tried to emit non-builtin scope `{}`",
            self
        );

        let ident = self.ident();
        let scope_id = self.get_scope().get_id();
        quote!(Scope::from_id(#scope_id).symbol(#ident)).to_tokens(tokens);
    }
}

impl ToTokens for Name {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let root = &self.root;
        let segments = self.path.iter();

        let quoted = quote! {
            Name {
                root: #root,
                path: vector![#(#segments),*],
            }
        };

        quoted.to_tokens(tokens);
    }
}

impl ToTokens for Var {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Var::Named(ident) => quote!(Var::Named(#ident)).to_tokens(tokens),
            Var::Anonymous => quote!(Var::Anonymous).to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    KnowledgeBase,
    Module,
    Term,
}

struct SyntaxCtx {
    terms_ident: SynIdent,
    modules_ident: SynIdent,
    module_ident: SynIdent,
    scope_ident: SynIdent,
    root_ident: SynIdent,
    mode: Mode,
}

impl SyntaxCtx {
    fn new(mode: Mode) -> Self {
        Self {
            terms_ident: SynIdent::new("__ir_terms", Span::call_site()),
            modules_ident: SynIdent::new("__ir_modules", Span::call_site()),
            module_ident: SynIdent::new("__ir_module", Span::call_site()),
            scope_ident: SynIdent::new("__ir_scope", Span::call_site()),
            root_ident: SynIdent::new("__ir_root", Span::call_site()),
            mode,
        }
    }
}

impl ToTokens for QuasiArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        let ty = &self.ty;
        quote!(#ident: #ty).to_tokens(tokens);
    }
}

impl ToTokens for QuasiVar {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

impl IrNode {
    fn to_syntax(&self, ir: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        use IrNode::*;

        let SyntaxCtx { root_ident, .. } = ctx;

        match self {
            Const(name) => {
                let segments = name.path.iter();
                match name.root {
                    Symbol::LOCAL => quote!(
                        IrNode::Const(Name {
                            root: #root_ident.clone(),
                            path: vector![#(#segments),*],
                        })
                    ),
                    ref s if s.get_scope().is_reserved() => quote!(IrNode::Const(#name)),
                    ref s => panic!("root {} shouldn't be here.", s),
                }
            }
            Var(var) => quote!(IrNode::Var(#var)),
            Int32(i) => quote!(IrNode::Int32(#i)),
            UInt32(i) => quote!(IrNode::UInt32(#i)),
            Float32(f) => quote!(IrNode::Float32(#f)),
            Ref(r) => {
                let r_syn = r.to_syntax(ir, ctx);
                quote!({
                    let ir_ref = (#r_syn);
                    IrNode::Ref(ir_ref)
                })
            }
            Blob(_) => todo!(),
            Raw(raw) => quote!(IrNode::Raw(#raw)),
            Quasi(qv) => {
                let q = qv.into_token_stream();
                quote!(IrNode::from(#q))
            }
        }
    }
}

impl IrRef {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let args = terms[*self]
            .args
            .iter()
            .map(|node| node.to_syntax(terms, ctx));

        let kind = match &terms[*self].kind {
            IrCompoundKind::Tagged => quote!(IrCompoundKind::Tagged),
            IrCompoundKind::Cons => quote!(IrCompoundKind::Cons),
            IrCompoundKind::Cons2 => quote!(IrCompoundKind::Cons2),
            IrCompoundKind::Struct => quote!(IrCompoundKind::Struct),
            IrCompoundKind::Extern => quote!(IrCompoundKind::Extern),
            IrCompoundKind::Opaque => quote!(IrCompoundKind::Opaque),
        };

        let terms_ident = &ctx.terms_ident;
        quote! {
            {
                let mut args = vector![#(#args),*];
                #terms_ident.new_compound(#kind, args)
            }
        }
    }
}

impl IrGoal {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        match self {
            IrGoal::Quasi(qv) => quote!(#qv),
            IrGoal::Goal(compound) => {
                let syntax = compound.to_syntax(terms, ctx);
                quote!(IrGoal::from(#syntax))
            }
        }
    }
}

impl IrRelation {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let head = self.head.to_syntax(terms, ctx);
        let body = self.body.iter().map(|g| g.to_syntax(terms, ctx));

        quote! {
            IrRelation {
                head: #head,
                body: vector![#(#body),*],
            }
        }
    }
}

impl IrModuleEntry {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let rel_syntax = match self {
            IrModuleEntry::Relation(relation) => relation.to_syntax(terms, ctx),
            IrModuleEntry::Quasi(qv) => qv.to_token_stream(),
        };
        quote!(IrModuleEntry::Relation(#rel_syntax))
    }
}

impl IrQuery {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let args = self.goals.iter().map(|g| g.to_syntax(terms, ctx));

        quote! {
            {
                let goals = vector![#(#args),*];
                IrQuery { goals }
            }
        }
    }
}

impl IrModule {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let modules_ident = &ctx.modules_ident;
        let module_ident = &ctx.module_ident;
        let scope_ident = &ctx.scope_ident;
        let root_ident = &ctx.root_ident;

        let entries = self.entries.iter().map(|r| r.to_syntax(terms, ctx));

        quote! {
            {
                let #scope_ident = #modules_ident[#module_ident].get_scope();
                let #root_ident = #modules_ident[#module_ident].get_root();
                let entries = vector![#(#entries),*];
                #modules_ident[#module_ident].entries = entries;
                #module_ident
            }
        }
    }
}

impl IrKnowledgeBase {
    fn to_syntax(&self, terms: &IrTermGraph, ctx: &SyntaxCtx) -> TokenStream {
        let terms_ident = &ctx.terms_ident;
        let modules_ident = &ctx.modules_ident;
        let module_ident = &ctx.module_ident;

        let modules = self.iter().map(|m| {
            let module_toks = m.to_syntax(terms, ctx);

            // TODO: do we need a writer lock here? Will a reader lock + try_normalize_full
            // + unwrap work?
            let module_name = terms
                .symbol_table()
                .write()
                .normalize_full(m.get_root().clone());

            assert!(
                module_name.root.get_scope().is_reserved(),
                "module name `{}` is not fully normalized (from `{}`)",
                module_name,
                m.get_root(),
            );

            quote! {{
                let root_sym = #terms_ident.symbol_table().write().normalize(#module_name);
                let #module_ident = #modules_ident.new_named_module_with_root(root_sym);
                let _ = #module_toks;
            }}
        });

        quote! {
            {
                let mut #modules_ident =
                    IrKnowledgeBase::new(#terms_ident.symbol_table().clone());
                #(#modules)*

                #modules_ident
            }
        }
    }
}

fn prelude_and_wrapper(
    fn_ident: &SynIdent,
    ctx: &SyntaxCtx,
    args: &[QuasiArg],
    output_ty: &TokenStream,
    output: &TokenStream,
) -> TokenStream {
    let terms_ident = &ctx.terms_ident;
    let modules_ident = &ctx.modules_ident;
    let module_ident = &ctx.module_ident;
    let scope_ident = &ctx.scope_ident;
    let root_ident = &ctx.root_ident;

    let imports = quote! {
        use whisper_ir::{
            graph::{
                IrNode, IrModule, IrRef, IrTermGraph, IrQuery,
                IrRelation, IrCompound, IrCompoundKind, IrGoal,
                IrModuleEntry, IrKnowledgeBase,
            },
            Ident, Symbol, Scope, Name, Var,
            vector, hashset,
        };
    };

    let args_insert = match ctx.mode {
        Mode::KnowledgeBase => quote! {
            #terms_ident: &mut whisper_ir::graph::IrTermGraph,
        },
        Mode::Module => quote! {
            #terms_ident: &mut whisper_ir::graph::IrTermGraph,
            #modules_ident: &mut whisper_ir::graph::IrKnowledgeBase,
            #module_ident: whisper_ir::graph::IrModuleRef,
        },
        Mode::Term => quote! {
            #terms_ident: &mut whisper_ir::graph::IrTermGraph,
            #root_ident: &whisper_ir::Symbol,
        },
    };

    let body_insert = match ctx.mode {
        Mode::KnowledgeBase => TokenStream::new(),
        // Module syntax generator will generate #scope_ident and #root_ident
        Mode::Module => TokenStream::new(),
        Mode::Term => quote! {
            let #scope_ident = #root_ident.get_scope();
        },
    };

    quote! {
        fn #fn_ident(#args_insert #(#args),*) -> #output_ty {
            #imports
            #body_insert
            #output
        }
    }
}

pub fn knowledge_base(input: TokenStream) -> Result<TokenStream> {
    let symbols = SymbolTable::new();
    parse::use_graphs(
        IrKnowledgeBase::new(symbols.clone()),
        IrTermGraph::new(symbols.clone()),
    );

    let input: QuasiquoteInput<IrKnowledgeBase> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::KnowledgeBase);
    let args = input.args;

    let (_, terms) = parse::swap_graphs();
    let output = input.parsed.to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrKnowledgeBase),
        &output,
    ))
}

pub fn module(input: TokenStream) -> Result<TokenStream> {
    let symbols = SymbolTable::new();
    let mut modules = IrKnowledgeBase::new(symbols.clone());
    let module = modules.new_anonymous_module();
    parse::use_graphs(modules, IrTermGraph::new(symbols.clone()));
    parse::set_module(module);

    let input: QuasiquoteInput<IrModuleRef> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::Module);
    let args = input.args;

    let (modules, terms) = parse::swap_graphs();
    let output = modules[input.parsed].to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrModuleRef),
        &output,
    ))
}

pub fn query(input: TokenStream) -> Result<TokenStream> {
    let input: QuasiquoteInput<IrQuery> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::Term);
    let args = input.args;

    let (_, terms) = parse::swap_graphs();
    let output = input.parsed.to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrQuery),
        &output,
    ))
}

pub fn term(input: TokenStream) -> Result<TokenStream> {
    let input: QuasiquoteInput<IrNode> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::Term);
    let args = input.args;

    let (_, terms) = parse::swap_graphs();
    let output = input.parsed.to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrNode),
        &output,
    ))
}

pub fn goal(input: TokenStream) -> Result<TokenStream> {
    let input: QuasiquoteInput<IrGoal> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::Term);
    let args = input.args;

    let (_, terms) = parse::swap_graphs();
    let output = input.parsed.to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrGoal),
        &output,
    ))
}

pub fn relation(input: TokenStream) -> Result<TokenStream> {
    let input: QuasiquoteInput<IrRelation> = syn::parse2(input)?;

    let fn_ident = input.fn_ident;
    let ctx = SyntaxCtx::new(Mode::Term);
    let args = input.args;

    let (_, terms) = parse::swap_graphs();
    let output = input.parsed.to_syntax(&terms, &ctx);

    Ok(prelude_and_wrapper(
        &fn_ident,
        &ctx,
        &args,
        &quote!(whisper_ir::graph::IrRelation),
        &output,
    ))
}
