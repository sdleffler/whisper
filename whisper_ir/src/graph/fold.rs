//! Syntax tree traversal for building a new syntax tree. Since the Whisper IR is immutable,
//! we have no mutable [`Visit`]. [`Fold`] serves this purpose instead.
//!
//! Every method on the [`Fold`] trait is essentially a hook into the traversal process; they
//! have sensible default implementations which call the corresponding *function* in the [`fold`] module.
//! It is highly recommended to look at the source code before you implement a hook to
//! understand what is going on.
//!
//! **TODO: add example.**
//!
//! [`Visit`]: crate::ir::visit::Visit
//! [`fold`]: crate::ir::fold

use crate::{
    graph::{IrGoal, IrModuleEntry, IrNode, IrQuery, IrRef, IrRelation, IrTermGraph},
    Ident, Name, SymbolIndex, Var,
};

pub trait Fold {
    fn fold_symbol(&mut self, ir_graph: &mut IrTermGraph, sym: SymbolIndex) -> Option<SymbolIndex> {
        fold_symbol(self, ir_graph, sym)
    }

    fn fold_name(&mut self, ir_graph: &mut IrTermGraph, name: &Name) -> Option<Name> {
        fold_name(self, ir_graph, name)
    }

    fn fold_const(&mut self, ir_graph: &mut IrTermGraph, cst: &Name) -> Option<Name> {
        fold_const(self, ir_graph, cst)
    }

    fn fold_var(&mut self, ir_graph: &mut IrTermGraph, var: &Var) -> Option<Var> {
        fold_var(self, ir_graph, var)
    }

    fn fold_ident(&mut self, _ir_graph: &mut IrTermGraph, _ident: &Ident) -> Option<Ident> {
        None
    }

    fn fold_int32(&mut self, _ir_graph: &mut IrTermGraph, _int: &i32) -> Option<i32> {
        None
    }

    fn fold_uint32(&mut self, _ir_graph: &mut IrTermGraph, _int: &u32) -> Option<u32> {
        None
    }

    fn fold_float32(&mut self, _ir_graph: &mut IrTermGraph, _int: &f32) -> Option<f32> {
        None
    }

    fn fold_compound(&mut self, ir_graph: &mut IrTermGraph, ir_ref: &IrRef) -> Option<IrRef> {
        fold_compound(self, ir_graph, ir_ref)
    }

    fn fold_goal(&mut self, ir_graph: &mut IrTermGraph, goal: &IrGoal) -> Option<IrGoal> {
        fold_goal(self, ir_graph, goal)
    }

    fn fold_node(&mut self, ir_graph: &mut IrTermGraph, node: &IrNode) -> Option<IrNode> {
        fold_node(self, ir_graph, node)
    }

    fn fold_relation(
        &mut self,
        ir_graph: &mut IrTermGraph,
        relation: &IrRelation,
    ) -> Option<IrRelation> {
        fold_relation(self, ir_graph, relation)
    }

    fn fold_module_entry(
        &mut self,
        ir_graph: &mut IrTermGraph,
        entry: &IrModuleEntry,
    ) -> Option<IrModuleEntry> {
        fold_module_entry(self, ir_graph, entry)
    }
}

pub fn fold_symbol<V>(
    _v: &mut V,
    _ir_graph: &mut IrTermGraph,
    _sym: SymbolIndex,
) -> Option<SymbolIndex>
where
    V: Fold + ?Sized,
{
    None
}

pub fn fold_name<V>(v: &mut V, ir_graph: &mut IrTermGraph, name: &Name) -> Option<Name>
where
    V: Fold + ?Sized,
{
    let maybe_root = v.fold_symbol(ir_graph, name.root);
    let mut path = name.path.clone();
    let mut focus = path.focus_mut();
    for i in 0..focus.len() {
        if let Some(updated_segment) = v.fold_ident(ir_graph, focus.get(i).unwrap()) {
            *focus.index_mut(i) = updated_segment;
        }
    }

    // See `fold_relation` for an explanation of this trick.
    if maybe_root.is_some() || !path.ptr_eq(&name.path) {
        Some(Name {
            root: maybe_root.unwrap_or(name.root),
            path,
        })
    } else {
        None
    }
}

pub fn fold_const<V>(v: &mut V, ir_graph: &mut IrTermGraph, cst: &Name) -> Option<Name>
where
    V: Fold + ?Sized,
{
    v.fold_name(ir_graph, cst)
}

pub fn fold_var<V>(v: &mut V, ir_graph: &mut IrTermGraph, var: &Var) -> Option<Var>
where
    V: Fold + ?Sized,
{
    match var {
        Var::Named(ident) => v.fold_ident(ir_graph, ident).map(Var::Named),
        Var::Anonymous => None,
    }
}

pub fn fold_compound<V>(v: &mut V, ir_graph: &mut IrTermGraph, ir_ref: &IrRef) -> Option<IrRef>
where
    V: Fold + ?Sized,
{
    let mut args = ir_graph[*ir_ref].args.clone();
    let mut focus = args.focus_mut();
    for i in 0..focus.len() {
        if let Some(updated_node) = v.fold_node(ir_graph, focus.get(i).unwrap()) {
            *focus.index_mut(i) = updated_node;
        }
    }

    if !args.ptr_eq(&ir_graph[*ir_ref].args) {
        let kind = ir_graph[*ir_ref].kind.clone();
        Some(ir_graph.new_compound(kind, args))
    } else {
        None
    }
}

pub fn fold_goal<V>(v: &mut V, ir_graph: &mut IrTermGraph, goal: &IrGoal) -> Option<IrGoal>
where
    V: Fold + ?Sized,
{
    match goal {
        IrGoal::Quasi(_) => todo!(),
        IrGoal::Goal(goal) => v.fold_compound(ir_graph, goal).map(IrGoal::from),
    }
}

pub fn fold_int32<V>(v: &mut V, ir_graph: &mut IrTermGraph, int: &i32) -> Option<i32>
where
    V: Fold + ?Sized,
{
    v.fold_int32(ir_graph, int)
}

pub fn fold_uint32<V>(v: &mut V, ir_graph: &mut IrTermGraph, int: &u32) -> Option<u32>
where
    V: Fold + ?Sized,
{
    v.fold_uint32(ir_graph, int)
}

pub fn fold_float32<V>(v: &mut V, ir_graph: &mut IrTermGraph, float: &f32) -> Option<f32>
where
    V: Fold + ?Sized,
{
    v.fold_float32(ir_graph, float)
}

pub fn fold_node<V>(v: &mut V, ir_graph: &mut IrTermGraph, node: &IrNode) -> Option<IrNode>
where
    V: Fold + ?Sized,
{
    match node {
        IrNode::Const(name) => v.fold_const(ir_graph, name).map(IrNode::Const),
        IrNode::Var(var) => v.fold_var(ir_graph, var).map(IrNode::Var),
        IrNode::Int32(i) => v.fold_int32(ir_graph, i).map(IrNode::Int32),
        IrNode::UInt32(i) => v.fold_uint32(ir_graph, i).map(IrNode::UInt32),
        IrNode::Float32(i) => v.fold_float32(ir_graph, i).map(IrNode::Float32),
        IrNode::Ref(ir_ref) => v.fold_compound(ir_graph, ir_ref).map(IrNode::Ref),
        IrNode::Blob(_) => todo!(),
        IrNode::Raw(_) => todo!(),
        IrNode::Quasi(_) => todo!(),
    }
}

pub fn fold_relation<V>(
    v: &mut V,
    ir_graph: &mut IrTermGraph,
    relation: &IrRelation,
) -> Option<IrRelation>
where
    V: Fold + ?Sized,
{
    let maybe_head = v.fold_compound(ir_graph, &relation.head);
    let mut body = relation.body.clone();
    let mut focus = body.focus_mut();
    for i in 0..focus.len() {
        if let Some(updated_goal) = v.fold_goal(ir_graph, focus.get(i).unwrap()) {
            *focus.index_mut(i) = updated_goal;
        }
    }

    // If there's an updated `head` or the body is updated (and is no longer
    // the same immutable vector as before) then we return `Some`, else `None`.
    if maybe_head.is_some() || !body.ptr_eq(&relation.body) {
        Some(IrRelation {
            head: maybe_head.unwrap_or(relation.head),
            body,
        })
    } else {
        None
    }
}

pub fn fold_module_entry<V>(
    v: &mut V,
    ir_graph: &mut IrTermGraph,
    entry: &IrModuleEntry,
) -> Option<IrModuleEntry>
where
    V: Fold + ?Sized,
{
    match entry {
        IrModuleEntry::Relation(relation) => v
            .fold_relation(ir_graph, relation)
            .map(IrModuleEntry::Relation),
        IrModuleEntry::Quasi(_) => todo!(),
    }
}

pub fn fold_query<V>(v: &mut V, ir_graph: &mut IrTermGraph, query: &IrQuery) -> Option<IrQuery>
where
    V: Fold,
{
    let mut goals = query.goals.clone();
    let mut focus = goals.focus_mut();
    for i in 0..focus.len() {
        if let Some(updated_goal) = v.fold_goal(ir_graph, focus.get(i).unwrap()) {
            *focus.index_mut(i) = updated_goal;
        }
    }

    if !goals.ptr_eq(&query.goals) {
        Some(IrQuery { goals })
    } else {
        None
    }
}
