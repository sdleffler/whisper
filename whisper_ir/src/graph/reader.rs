use ::im::vector;

use crate::{
    graph::{IrCompoundKind, IrNode, IrTermGraph},
    trans::{
        reader::{TermReader, TermVisitor},
        CompoundKind,
    },
};

pub struct TermGraphReader<'graph> {
    terms: &'graph IrTermGraph,
    nodes: vector::Iter<'graph, IrNode>,
}

impl<'re, 'graph: 're> TermReader<'re> for TermGraphReader<'graph> {
    type View = Self;

    fn as_view_mut(&mut self) -> &mut Self::View {
        self
    }

    fn read_raw(&mut self) -> Option<u64> {
        match self.nodes.next() {
            Some(IrNode::Raw(raw)) => Some(*raw),
            _ => None,
        }
    }

    fn read<V>(&mut self, v: V) -> Option<V::Value>
    where
        V: TermVisitor<'re, Self>,
    {
        let node = match self.nodes.next() {
            Some(it) => it,
            None => return None,
        };

        match node {
            IrNode::Var(var) => Some(v.visit_var(var.clone())),
            IrNode::Const(name) => Some(v.visit_const(self.terms.symbols(), name.clone())),
            IrNode::Ref(ir_ref) => {
                let comp = &self.terms[*ir_ref];
                let iter = comp.args.iter();
                let kind = match comp.kind {
                    IrCompoundKind::Tagged => CompoundKind::Tagged,
                    IrCompoundKind::Cons => CompoundKind::Cons,
                    IrCompoundKind::Cons2 => CompoundKind::Cons2,
                    IrCompoundKind::Struct => CompoundKind::Struct(comp.args.len()),
                    IrCompoundKind::Extern => CompoundKind::Extern(comp.args.len()),
                    IrCompoundKind::Opaque => CompoundKind::Opaque(comp.args.len()),
                };

                let tr = Self {
                    terms: self.terms,
                    nodes: iter,
                };

                Some(v.visit_compound(self.terms.symbols(), kind, tr))
            }
            IrNode::Int32(i) => Some(v.visit_i32(*i)),
            IrNode::UInt32(u) => Some(v.visit_u32(*u)),
            IrNode::Float32(f) => Some(v.visit_f32(*f)),
            IrNode::Blob(blob) => Some(v.visit_blob(blob.clone())),

            _ => panic!(),
        }
    }
}
