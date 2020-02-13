//! Syntax tree traversal for visiting nodes of a Whisper IR object.
//!
//! This does not allow mutation, as the Whisper IR is immutable. For updates, see
//! the [`fold`] module and [`Fold`] trait.
//!
//! Every method on the [`Visit`] trait is essentially a hook into the traversal process; they
//! have sensible default implementations which call the corresponding *function* in the [`visit`]
//! module. It is highly recommended to look at the source code before you implement a hook to
//! understand what is going on.
//!
//! **TODO: add example.**
//!
//! [`fold`]: crate::ir::fold
//! [`Fold`]: crate::ir::fold::Fold
//! [`visit`]: crate::ir::visit

use crate::{
    graph::{IrGoal, IrModule, IrModuleEntry, IrNode, IrRef, IrRelation, IrTermGraph},
    Ident, Name, Scope, Symbol, Var,
};

pub trait Visit<'graph> {
    fn visit_symbol(&mut self, ir_graph: &'graph IrTermGraph, symbol: &'graph Symbol) {
        visit_symbol(self, ir_graph, symbol);
    }

    fn visit_name(&mut self, ir_graph: &'graph IrTermGraph, name: &'graph Name) {
        visit_name(self, ir_graph, name);
    }

    fn visit_const(&mut self, ir_graph: &'graph IrTermGraph, cst: &'graph Name) {
        visit_const(self, ir_graph, cst);
    }

    fn visit_var(&mut self, ir_graph: &'graph IrTermGraph, var: &'graph Var) {
        visit_var(self, ir_graph, var);
    }

    fn visit_ident(&mut self, _ir_graph: &'graph IrTermGraph, _ident: &'graph Ident) {}
    fn visit_scope(&mut self, _ir_graph: &'graph IrTermGraph, _scope: &'graph Scope) {}
    fn visit_int32(&mut self, _ir_graph: &'graph IrTermGraph, _int: &'graph i32) {}
    fn visit_uint32(&mut self, _ir_graph: &'graph IrTermGraph, _int: &'graph u32) {}
    fn visit_float32(&mut self, _ir_graph: &'graph IrTermGraph, _int: &'graph f32) {}

    fn visit_compound(&mut self, ir_graph: &'graph IrTermGraph, ir_ref: &'graph IrRef) {
        visit_compound(self, ir_graph, ir_ref);
    }

    fn visit_goal(&mut self, ir_graph: &'graph IrTermGraph, goal: &'graph IrGoal) {
        visit_goal(self, ir_graph, goal);
    }

    fn visit_node(&mut self, ir_graph: &'graph IrTermGraph, node: &'graph IrNode) {
        visit_node(self, ir_graph, node);
    }

    fn visit_relation(&mut self, ir_graph: &'graph IrTermGraph, relation: &'graph IrRelation) {
        visit_relation(self, ir_graph, relation);
    }

    fn visit_module_entry(&mut self, ir_graph: &'graph IrTermGraph, entry: &'graph IrModuleEntry) {
        visit_module_entry(self, ir_graph, entry);
    }
}

pub fn visit_symbol<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, symbol: &'graph Symbol)
where
    V: Visit<'graph> + ?Sized,
{
    v.visit_ident(ir_graph, symbol.ident());
    v.visit_scope(ir_graph, symbol.get_scope_ref());
}

pub fn visit_name<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, name: &'graph Name)
where
    V: Visit<'graph> + ?Sized,
{
    v.visit_symbol(ir_graph, &name.root);
    for segment in &name.path {
        v.visit_ident(ir_graph, segment);
    }
}

pub fn visit_const<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, cst: &'graph Name)
where
    V: Visit<'graph> + ?Sized,
{
    v.visit_name(ir_graph, cst);
}

pub fn visit_var<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, var: &'graph Var)
where
    V: Visit<'graph> + ?Sized,
{
    match var {
        Var::Named(ident) => v.visit_ident(ir_graph, ident),
        Var::Anonymous => {}
    }
}

pub fn visit_compound<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, ir_ref: &'graph IrRef)
where
    V: Visit<'graph> + ?Sized,
{
    for node in ir_graph[*ir_ref].args.iter() {
        v.visit_node(ir_graph, node);
    }
}

pub fn visit_goal<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, goal: &'graph IrGoal)
where
    V: Visit<'graph> + ?Sized,
{
    match goal {
        IrGoal::Quasi(_) => todo!(),
        IrGoal::Goal(goal) => v.visit_compound(ir_graph, goal),
    }
}

pub fn visit_node<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, node: &'graph IrNode)
where
    V: Visit<'graph> + ?Sized,
{
    match node {
        IrNode::Const(name) => v.visit_const(ir_graph, name),
        IrNode::Var(var) => v.visit_var(ir_graph, var),
        IrNode::Int32(i) => v.visit_int32(ir_graph, i),
        IrNode::UInt32(i) => v.visit_uint32(ir_graph, i),
        IrNode::Float32(f) => v.visit_float32(ir_graph, f),
        IrNode::Ref(ir_ref) => v.visit_compound(ir_graph, ir_ref),
        IrNode::Blob(_) => todo!(),
        IrNode::Raw(_) => todo!(),
        IrNode::Quasi(_) => todo!(),
    }
}

pub fn visit_relation<'graph, V>(
    v: &mut V,
    ir_graph: &'graph IrTermGraph,
    relation: &'graph IrRelation,
) where
    V: Visit<'graph> + ?Sized,
{
    v.visit_compound(ir_graph, &relation.head);
    for goal in &relation.body {
        v.visit_goal(ir_graph, goal);
    }
}

pub fn visit_module_entry<'graph, V>(
    v: &mut V,
    ir_graph: &'graph IrTermGraph,
    relation_entry: &'graph IrModuleEntry,
) where
    V: Visit<'graph> + ?Sized,
{
    match relation_entry {
        IrModuleEntry::Relation(relation) => v.visit_relation(ir_graph, relation),
        IrModuleEntry::Quasi(_) => todo!(),
    }
}

pub fn visit_module<'graph, V>(v: &mut V, ir_graph: &'graph IrTermGraph, clause: &'graph IrModule)
where
    V: Visit<'graph> + ?Sized,
{
    for entry in clause.entries.iter() {
        v.visit_module_entry(ir_graph, entry);
    }
}
