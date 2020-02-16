use crate::{
    parse::QuasiVar,
    trans::{writer::VarScopeId, TermEmitter, TermGraph, TermWriter},
    Name, SymbolTable, Var,
};

use ::{
    bincode,
    by_address::ByAddress,
    derive_more::From,
    im::Vector,
    serde::{Deserialize, Serialize},
    std::{fmt, ops::Index, rc::Rc},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Blob {
    bytes: ByAddress<Rc<[u8]>>,
}

impl Blob {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        Self {
            bytes: ByAddress(Rc::from(bytes)),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn serialize<S: Serialize>(value: &S) -> Self {
        let bytes = ByAddress(Rc::from(bincode::serialize(value).unwrap()));
        Self { bytes }
    }

    pub fn deserialize<'obj, D: Deserialize<'obj>>(&'obj self) -> D {
        bincode::deserialize(&self.bytes).unwrap()
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum IrNode {
    Var(Var),
    Const(Name),
    Ref(IrRef),
    Int32(i32),
    UInt32(u32),
    Float32(f32),
    Blob(Blob),

    Raw(u64),

    Quasi(QuasiVar),
}

impl<'a> From<&'a IrNode> for IrNode {
    fn from(node: &'a IrNode) -> Self {
        node.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IrRef(usize);

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IrCompoundKind {
    Tagged,
    Cons,
    Cons2,
    Struct,
    Extern,
    Opaque,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct IrCompound {
    pub args: Vector<IrNode>,
    pub kind: IrCompoundKind,
}

impl IrCompound {
    pub fn len(&self) -> usize {
        self.args.len()
    }
}

#[derive(Debug, Clone, From)]
pub enum IrGoal {
    Goal(IrRef),
    Quasi(QuasiVar),
}

impl IrGoal {
    pub fn unwrap(&self) -> IrRef {
        match self {
            IrGoal::Goal(ir_ref) => *ir_ref,
            IrGoal::Quasi(quasi) => panic!("IrGoal: unexpected quasi! {:?}", quasi),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrTermGraph {
    structs: Vector<IrCompound>,
    symbols: SymbolTable,
}

impl Index<IrRef> for IrTermGraph {
    type Output = IrCompound;

    fn index(&self, idx: IrRef) -> &Self::Output {
        &self.structs[idx.0]
    }
}

impl IrTermGraph {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            structs: Vector::new(),
            symbols: symbol_table,
        }
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn new_compound(&mut self, kind: IrCompoundKind, args: impl Into<Vector<IrNode>>) -> IrRef {
        let args = args.into();
        let i = IrRef(self.structs.len());
        self.structs.push_back(IrCompound { args, kind });
        i
    }

    pub fn new_structure(&mut self, args: impl Into<Vector<IrNode>>) -> IrRef {
        self.new_compound(IrCompoundKind::Struct, args)
    }

    pub fn new_extern(&mut self, args: impl Into<Vector<IrNode>>) -> IrRef {
        self.new_compound(IrCompoundKind::Extern, args)
    }

    pub fn new_opaque(&mut self, args: impl Into<Vector<IrNode>>) -> IrRef {
        self.new_compound(IrCompoundKind::Opaque, args)
    }

    pub fn new_cons(&mut self, head: IrNode, tail: IrNode) -> IrRef {
        self.new_compound(IrCompoundKind::Cons, im::vector![head, tail])
    }

    pub fn new_cons2(&mut self, key: IrNode, value: IrNode, tail: IrNode) -> IrRef {
        self.new_compound(IrCompoundKind::Cons2, im::vector![key, value, tail])
    }

    pub fn new_tagged(&mut self, tag: IrNode, value: IrNode) -> IrRef {
        self.new_compound(IrCompoundKind::Tagged, im::vector![tag, value])
    }
}

impl<'a> TermGraph for &'a IrTermGraph {
    type Id = IrRef;

    fn visit_compound<B>(
        emitter: &mut TermEmitter<B, Self>,
        var_scope: VarScopeId,
        &id: &IrRef,
    ) -> B::Placement
    where
        B: TermWriter,
    {
        use crate::trans::CompoundKind;

        let compound = emitter.get_graph()[id].clone();
        let kind = match (compound.kind, compound.args.len()) {
            (IrCompoundKind::Tagged, 2) => CompoundKind::Tagged,
            (IrCompoundKind::Tagged, n) => {
                panic!("tagged value has wrong arity `{}` (expected 2)", n)
            }
            (IrCompoundKind::Cons, 2) => CompoundKind::Cons,
            (IrCompoundKind::Cons, n) => panic!("cons cell has wrong arity `{}` (expected 2)", n),
            (IrCompoundKind::Cons2, 3) => CompoundKind::Cons2,
            (IrCompoundKind::Cons2, n) => panic!("cons2 cell has wrong arity `{}` (expected 3)", n),
            (IrCompoundKind::Struct, n) => CompoundKind::Struct(n),
            (IrCompoundKind::Extern, n) => CompoundKind::Extern(n),
            (IrCompoundKind::Opaque, n) => CompoundKind::Opaque(n),
        };

        let placement = emitter.begin_compound(kind);

        for arg in compound.args {
            match arg {
                IrNode::Var(var) => emitter.emit_var(var_scope, var),
                IrNode::Const(name) => emitter.emit_const(name),
                IrNode::Ref(ir_ref) => emitter.emit_compound(var_scope, ir_ref),
                IrNode::Int32(i) => emitter.emit_i32(i),
                IrNode::UInt32(u) => emitter.emit_u32(u),
                IrNode::Float32(f) => emitter.emit_f32(f),
                IrNode::Blob(blob) => emitter.emit_blob(blob),
                IrNode::Raw(raw) => emitter.emit_raw(raw),

                IrNode::Quasi(_) => panic!("quasiquotation variables cannot be emitted"),
            }
        }

        emitter.end_compound();
        placement
    }
}

pub struct DebugNode<'graph> {
    pub graph: &'graph IrTermGraph,
    pub node: IrNode,
}

impl IrTermGraph {
    pub fn debug_node(&self, ir_node: IrNode) -> DebugNode {
        DebugNode {
            graph: self,
            node: ir_node,
        }
    }
}

impl<'graph> fmt::Debug for DebugNode<'graph> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node {
            IrNode::Ref(ir_ref) => f
                .debug_tuple("IrNode")
                .field(&self.graph.debug_ref(*ir_ref))
                .finish(),
            other => other.fmt(f),
        }
    }
}

pub struct DebugRef<'graph> {
    pub graph: &'graph IrTermGraph,
    pub ir_ref: IrRef,
}

impl IrTermGraph {
    pub fn debug_ref(&self, ir_ref: IrRef) -> DebugRef {
        DebugRef {
            graph: self,
            ir_ref: ir_ref,
        }
    }
}

impl<'graph> fmt::Debug for DebugRef<'graph> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct DebugArgs<'graph> {
            graph: &'graph IrTermGraph,
            args: &'graph Vector<IrNode>,
        }

        impl<'graph> fmt::Debug for DebugArgs<'graph> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list()
                    .entries(
                        self.args
                            .iter()
                            .map(|node| self.graph.debug_node(node.clone())),
                    )
                    .finish()
            }
        }

        f.debug_struct("IrCompound")
            .field("kind", &self.graph[self.ir_ref].kind)
            .field(
                "args",
                &DebugArgs {
                    graph: self.graph,
                    args: &self.graph[self.ir_ref].args,
                },
            )
            .finish()
    }
}

#[cfg(test)]
use serde_json::{json, Value as JsonValue};

#[cfg(test)]
impl IrNode {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        match self {
            IrNode::Var(var) => json!({ "Var": var.to_string() }),
            IrNode::Const(name) => json!({ "Const": name.to_string() }),
            IrNode::Ref(ir_ref) => json! ({ "Ref": ir_ref.to_json(terms) }),
            IrNode::Int32(i) => json!({ "Int32": i }),
            IrNode::UInt32(u) => json!({ "UInt32": u }),
            IrNode::Float32(f) => json!({ "Float32": f }),
            IrNode::Blob(blob) => json! ({ "Blob": blob.as_bytes() }),
            IrNode::Raw(raw) => json!({ "Raw": raw }),
            IrNode::Quasi(qv) => json! ({ "Quasi": qv.0.to_string() }),
        }
    }
}

#[cfg(test)]
impl IrRef {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        let compound = &terms[*self];

        let args = JsonValue::Array(
            compound
                .args
                .iter()
                .map(|node| node.to_json(terms))
                .collect(),
        );

        json! {{
            "kind": format!("{:?}", compound.kind),
            "args": args,
        }}
    }
}

#[cfg(test)]
impl IrGoal {
    pub fn to_json(&self, terms: &IrTermGraph) -> JsonValue {
        match self {
            IrGoal::Goal(ir_ref) => ir_ref.to_json(terms),
            IrGoal::Quasi(qv) => json!({"Quasi": qv.0.to_string()}),
        }
    }
}
