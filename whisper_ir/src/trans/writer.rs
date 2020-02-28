use ::std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    mem,
};

use crate::{
    graph::Blob,
    trans::{CompoundKind, Ref},
    Name, Var,
};

pub trait TermGraph: Sized {
    /// The type of an identifier of a "compound" node in a term graph,
    /// a node which contains other nodes. This is the type of note that
    /// a `TermWriter` cannot emit directly without extra information
    /// taken from traversing the graph it comes from.
    ///
    /// For an `IrTermGraph`, this is an `IrRef`. For a `Heap`, this is
    /// a reference `Word`, either a `Cons`, `StructRef`, `ExternRef`,
    /// `BlobRef`, or `OpaqueRef`.
    type Id: Ref;

    fn visit_compound<B>(
        emitter: &mut TermEmitter<B, Self>,
        var_scope: VarScopeId,
        id: &Self::Id,
    ) -> B::Placement
    where
        B: TermWriter;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarScopeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ForwardRef<T: Ref> {
    Compound(VarScopeId, T),
    Blob(Blob),
}

pub trait TermWriter: Sized {
    type VarScope: Default;

    type Hole;
    type Placement;

    fn write_var(&mut self, scope: &mut Self::VarScope, var: &Var);
    fn write_const(&mut self, name: &Name);
    fn write_i32(&mut self, i: i32);
    fn write_u32(&mut self, u: u32);
    fn write_f32(&mut self, f: f32);
    fn write_forward(&mut self) -> Self::Hole;
    fn write_ref(&mut self, placement: &Self::Placement);

    fn push_box(&mut self) -> Self::Placement;
    fn pop_box(&mut self);

    fn push_compound(&mut self, kind: CompoundKind) -> Self::Placement;
    fn pop_compound(&mut self);

    fn emit_blob(&mut self, blob: &Blob) -> Self::Placement;
    fn write_raw(&mut self, raw: u64);

    fn fill(&mut self, hole: Self::Hole, placement: &Self::Placement);

    type Output;
    fn get(&mut self, placement: &Self::Placement) -> Self::Output;
}

impl<'a, T: TermWriter> TermWriter for &'a mut T {
    type VarScope = T::VarScope;
    type Hole = T::Hole;
    type Placement = T::Placement;

    fn write_var(&mut self, scope: &mut Self::VarScope, var: &Var) {
        (*self).write_var(scope, var)
    }

    fn write_const(&mut self, name: &Name) {
        (*self).write_const(name)
    }

    fn write_i32(&mut self, i: i32) {
        (*self).write_i32(i)
    }

    fn write_u32(&mut self, u: u32) {
        (*self).write_u32(u)
    }

    fn write_f32(&mut self, f: f32) {
        (*self).write_f32(f)
    }

    fn write_forward(&mut self) -> Self::Hole {
        (*self).write_forward()
    }

    fn write_ref(&mut self, placement: &Self::Placement) {
        (*self).write_ref(placement)
    }

    fn push_box(&mut self) -> Self::Placement {
        (*self).push_box()
    }

    fn pop_box(&mut self) {
        (*self).pop_box()
    }

    fn push_compound(&mut self, kind: CompoundKind) -> Self::Placement {
        (*self).push_compound(kind)
    }

    fn pop_compound(&mut self) {
        (*self).pop_compound()
    }

    fn emit_blob(&mut self, blob: &Blob) -> Self::Placement {
        (*self).emit_blob(blob)
    }

    fn write_raw(&mut self, raw: u64) {
        (*self).write_raw(raw)
    }

    fn fill(&mut self, hole: Self::Hole, placement: &Self::Placement) {
        (*self).fill(hole, placement)
    }

    type Output = T::Output;
    fn get(&mut self, placement: &Self::Placement) -> Self::Output {
        (*self).get(placement)
    }
}

pub struct TermEmitter<B: TermWriter, T: TermGraph> {
    graph: T,
    builder: B,
    forwards: VecDeque<(B::Hole, ForwardRef<T::Id>)>,
    placements: HashMap<ForwardRef<T::Id>, B::Placement>,
    scopes: Vec<B::VarScope>,
}

impl<B: TermWriter, T: TermGraph> TermEmitter<B, T> {
    pub fn new(graph: T, builder: B) -> Self {
        Self {
            graph,
            builder,
            forwards: VecDeque::new(),
            placements: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn get_graph(&self) -> &T {
        &self.graph
    }

    pub fn get_builder(&self) -> &B {
        &self.builder
    }

    pub fn get_builder_mut(&mut self) -> &mut B {
        &mut self.builder
    }

    pub fn emit_var(&mut self, scope: VarScopeId, name: Var) {
        self.builder.write_var(&mut self.scopes[scope.0], &name);
    }

    pub fn emit_const(&mut self, name: Name) {
        self.builder.write_const(&name);
    }

    pub fn emit_i32(&mut self, i: i32) {
        self.builder.write_i32(i);
    }

    pub fn emit_u32(&mut self, u: u32) {
        self.builder.write_u32(u);
    }

    pub fn emit_f32(&mut self, f: f32) {
        self.builder.write_f32(f);
    }

    pub fn emit_compound(&mut self, var_scope: VarScopeId, id: T::Id) {
        let forward = ForwardRef::Compound(var_scope, id);
        match self.placements.get(&forward) {
            Some(placement) => self.builder.write_ref(placement),
            None => {
                let hole = self.builder.write_forward();
                self.forwards.push_back((hole, forward));
            }
        }
    }

    pub fn emit_blob(&mut self, blob: Blob) {
        let forward = ForwardRef::Blob(blob);
        match self.placements.get(&forward) {
            Some(placement) => self.builder.write_ref(placement),
            None => {
                let hole = self.builder.write_forward();
                self.forwards.push_back((hole, forward));
            }
        }
    }

    pub fn emit_raw(&mut self, raw: u64) {
        self.builder.write_raw(raw);
    }

    pub fn emit_forward(&mut self) -> B::Hole {
        self.builder.write_forward()
    }

    pub fn emit_full(&mut self, var_scope: VarScopeId, id: T::Id) -> B::Placement {
        let placement = T::visit_compound(self, var_scope, &id);
        self.resolve_forwards();
        placement
    }

    pub fn fill(&mut self, hole: B::Hole, placement: &B::Placement) {
        self.builder.fill(hole, placement);
    }

    pub fn insert_fresh_scope(&mut self) -> VarScopeId {
        self.push_scope(B::VarScope::default())
    }

    pub fn push_scope(&mut self, scope: B::VarScope) -> VarScopeId {
        let id = VarScopeId(self.scopes.len());
        self.scopes.push(scope);
        id
    }

    pub fn begin_box(&mut self) -> B::Placement {
        self.builder.push_box()
    }

    pub fn end_box(&mut self) {
        self.builder.pop_box();
    }

    pub fn begin_compound(&mut self, kind: CompoundKind) -> B::Placement {
        self.builder.push_compound(kind)
    }

    pub fn end_compound(&mut self) {
        self.builder.pop_compound();
    }

    pub fn resolve_forwards(&mut self) {
        while let Some((hole, forward)) = self.forwards.pop_front() {
            let resolved = match self.placements.get(&forward) {
                Some(resolved) => resolved,
                None => {
                    let loc = match &forward {
                        ForwardRef::Compound(vs, id) => T::visit_compound(self, *vs, id),
                        ForwardRef::Blob(object) => self.builder.emit_blob(object),
                    };

                    // TODO: this *should* always be empty, so eventually we should
                    // use `Entry::insert` (currently unstable)
                    self.placements.entry(forward).or_insert(loc)
                }
            };

            self.builder.fill(hole, resolved);
        }
    }

    pub fn get(&mut self, placement: &B::Placement) -> B::Output {
        assert!(
            self.forwards.is_empty(),
            "emitter contains unresolved forwards!"
        );

        self.builder.get(placement)
    }

    pub fn get_var_scope(&self, var_scope: VarScopeId) -> &B::VarScope {
        &self.scopes[var_scope.0]
    }

    pub fn take_var_scope(&mut self, var_scope: VarScopeId) -> B::VarScope {
        mem::take(&mut self.scopes[var_scope.0])
    }
}
