use ::im::Vector;

use crate::{
    graph::{Blob, IrCompoundKind, IrNode, IrRef, IrTermGraph},
    trans::{CompoundKind, TermWriter},
    Name, Var,
};

#[derive(Debug, Clone, Copy)]
pub struct NodeId(usize);

#[derive(Debug, Clone)]
pub enum Node {
    Full(IrNode),
    Ref(PlacementId),
}

#[derive(Debug, Clone, Copy)]
pub struct PlacementId(usize);

#[derive(Debug, Clone)]
pub enum Placement {
    Full(IrRef),
    Compound {
        args: Vec<NodeId>,
        kind: CompoundKind,
    },
    Blob(Blob),
}

impl Placement {
    fn unwrap_compound_kind(&self) -> CompoundKind {
        match self {
            Placement::Compound { kind, .. } => *kind,
            _ => unreachable!(),
        }
    }

    fn unwrap_args_ref(&self) -> &Vec<NodeId> {
        match self {
            Placement::Compound { args, .. } => args,
            _ => unreachable!(),
        }
    }

    fn unwrap_args_mut(&mut self) -> &mut Vec<NodeId> {
        match self {
            Placement::Compound { args, .. } => args,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct TermGraphWriter<'graph> {
    terms: &'graph mut IrTermGraph,
    nodes: Vec<Option<Node>>,
    placements: Vec<Placement>,
    stack: Vec<PlacementId>,
}

impl<'graph> TermGraphWriter<'graph> {
    fn push_to_top(&mut self, node: impl Into<Option<Node>>) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node.into());
        self.placements[self.stack.last().unwrap().0]
            .unwrap_args_mut()
            .push(id);
        id
    }

    fn write_to_graph(&mut self, placement_id: PlacementId) -> IrRef {
        if let Placement::Full(ir_ref) = &self.placements[placement_id.0] {
            *ir_ref
        } else {
            let mut args = Vector::new();
            let (arity, kind) = match self.placements[placement_id.0].unwrap_compound_kind() {
                CompoundKind::Tagged => (2, IrCompoundKind::Tagged),
                CompoundKind::Cons => (2, IrCompoundKind::Cons),
                CompoundKind::Cons2 => (3, IrCompoundKind::Cons2),
                CompoundKind::Struct(n) => (n, IrCompoundKind::Struct),
                CompoundKind::Extern(n) => (n, IrCompoundKind::Extern),
                CompoundKind::Opaque(n) => (n, IrCompoundKind::Opaque),
            };

            for i in 0..arity {
                let node_id = self.placements[placement_id.0].unwrap_args_ref()[i];
                let node = match self.nodes[node_id.0].clone().expect("unresolved node") {
                    Node::Full(full) => full,
                    Node::Ref(placement_id) => IrNode::Ref(self.write_to_graph(placement_id)),
                };

                args.push_back(node);
            }

            let ir_ref = self.terms.new_compound(kind, args);
            self.placements[placement_id.0] = Placement::Full(ir_ref);
            ir_ref
        }
    }
}

impl<'graph> TermWriter for TermGraphWriter<'graph> {
    type VarScope = ();

    type Hole = NodeId;
    type Placement = PlacementId;

    fn write_var(&mut self, _scope: &mut (), name: &Var) {
        self.push_to_top(Node::Full(IrNode::Var(name.clone())));
    }

    fn write_const(&mut self, name: &Name) {
        self.push_to_top(Node::Full(IrNode::Const(name.clone())));
    }

    fn write_i32(&mut self, i: i32) {
        self.push_to_top(Node::Full(IrNode::Int32(i)));
    }

    fn write_u32(&mut self, u: u32) {
        self.push_to_top(Node::Full(IrNode::UInt32(u)));
    }

    fn write_f32(&mut self, f: f32) {
        self.push_to_top(Node::Full(IrNode::Float32(f)));
    }

    fn write_forward(&mut self) -> NodeId {
        self.push_to_top(None)
    }

    fn write_ref(&mut self, placement: &PlacementId) {
        self.push_to_top(Node::Ref(*placement));
    }

    fn push_compound(&mut self, kind: CompoundKind) -> PlacementId {
        let placement_id = PlacementId(self.placements.len());
        self.placements.push(Placement::Compound {
            args: Vec::new(),
            kind,
        });
        self.stack.push(placement_id);
        placement_id
    }

    fn pop_compound(&mut self) {
        self.stack.pop();
    }

    fn emit_blob(&mut self, blob: &Blob) -> PlacementId {
        let id = PlacementId(self.placements.len());
        self.placements.push(Placement::Blob(blob.clone()));
        id
    }

    fn write_raw(&mut self, raw: u64) {
        self.push_to_top(Node::Full(IrNode::Raw(raw)));
    }

    fn fill(&mut self, hole: NodeId, placement_id: &PlacementId) {
        let node = match &self.placements[placement_id.0] {
            Placement::Full(ir_ref) => Node::Full(IrNode::Ref(*ir_ref)),
            Placement::Blob(blob) => Node::Full(IrNode::Blob(blob.clone())),
            Placement::Compound { .. } => Node::Ref(*placement_id),
        };

        self.nodes[hole.0] = Some(node);
    }

    type Output = IrRef;
    fn get(&mut self, placement_id: &PlacementId) -> Self::Output {
        self.write_to_graph(*placement_id)
    }
}
