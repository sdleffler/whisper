use super::*;
use crate::{Scope, SymbolTable};
use ::{egress::Artifact, std::mem, syn::parse::Parse};

struct TestContext<'art> {
    modules: IrKnowledgeBase,
    terms: IrTermGraph,
    artifact: &'art mut Artifact,
}

impl<'art> TestContext<'art> {
    fn new(artifact: &'art mut Artifact) -> Self {
        let symbols = SymbolTable::new();
        Self {
            modules: IrKnowledgeBase::new(symbols.clone()),
            terms: IrTermGraph::new(symbols.clone()),
            artifact,
        }
    }

    fn parse_str_as<T: Parse>(&mut self, s: &str) -> Result<T> {
        let dummy = SymbolTable::new();
        let swapped_modules = mem::replace(&mut self.modules, IrKnowledgeBase::new(dummy.clone()));
        let swapped_terms = mem::replace(&mut self.terms, IrTermGraph::new(dummy.clone()));
        super::use_graphs(swapped_modules, swapped_terms);
        let out = syn::parse_str(s);
        let (recovered_modules, recovered_terms) = super::swap_graphs();
        self.modules = recovered_modules;
        self.terms = recovered_terms;
        out
    }

    fn parse_node_artifact(&mut self, s: &str) -> Result<()> {
        let t = self.parse_str_as::<IrNode>(s)?;
        self.artifact.insert_json(s, t.to_json(&self.terms));
        Ok(())
    }

    fn parse_ref_artifact(&mut self, s: &str) -> Result<()> {
        let t = self.parse_str_as::<IrRef>(s)?;
        self.artifact.insert_json(s, t.to_json(&self.terms));
        Ok(())
    }

    fn parse_mod_artifact(&mut self, module: IrModuleRef, s: &str) -> Result<()> {
        self.terms
            .parse_module_str(&mut self.modules, module, s)
            .map_err(|err| syn::Error::new(proc_macro2::Span::call_site(), err))?;

        let key = self.modules[module].get_root().to_string();
        self.artifact
            .insert_json(&key, module.to_json(&self.terms, &self.modules));

        Ok(())
    }

    fn parse_kb_artifact(&mut self, key: &str, s: &str) -> Result<IrKnowledgeBase> {
        let kb = self
            .terms
            .parse_knowledge_base_str(s)
            .map_err(|err| syn::Error::new(proc_macro2::Span::call_site(), err))?;

        self.artifact.insert_json(&key, kb.to_json(&self.terms));

        Ok(kb)
    }
}

#[test]
fn absolute_path() -> Result<()> {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("absolute_path");
    let mut ctx = TestContext::new(artifact);

    ctx.parse_node_artifact("<abc>")?;
    ctx.parse_node_artifact("<abc::def>")?;
    ctx.parse_ref_artifact("valid_field <otherwise> <true>")?;

    egress.close_and_assert_unregressed().unwrap();
    Ok(())
}

#[test]
fn association_list() -> Result<()> {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("association_list");
    let mut ctx = TestContext::new(artifact);

    ctx.parse_ref_artifact("{ foo: bar, baz: quux | Rest }")?;
    ctx.parse_ref_artifact("{}")?;
    ctx.parse_ref_artifact("{ (fiddle dee): bar, baz: 3 }")?;

    egress.close_and_assert_unregressed().unwrap();
    Ok(())
}

#[test]
fn nested_modules_as_module() -> Result<()> {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("nested_modules_as_module");
    let mut ctx = TestContext::new(artifact);

    let module = ctx
        .modules
        .new_named_module_with_root(Scope::MOD.symbol("tests/nested_modules.wh (as modules)"));
    ctx.parse_mod_artifact(module, include_str!("tests/nested_modules.wh"))?;

    egress.close_and_assert_unregressed().unwrap();
    Ok(())
}

#[test]
fn nested_modules_as_knowledge_base() -> Result<()> {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("nested_modules_as_kb");
    let mut ctx = TestContext::new(artifact);

    ctx.parse_kb_artifact(
        "tests/nested_modules.wh (as knowledge base)",
        include_str!("tests/nested_modules.wh"),
    )?;

    egress.close_and_assert_unregressed().unwrap();
    Ok(())
}

#[test]
fn super_scopes() -> Result<()> {
    let mut egress = egress::egress!();
    let artifact = egress.artifact("super_scopes");
    let mut ctx = TestContext::new(artifact);

    ctx.parse_kb_artifact(
        "tests/super_scopes.wh",
        include_str!("tests/super_scopes.wh"),
    )?;

    egress.close_and_assert_unregressed().unwrap();
    Ok(())
}
