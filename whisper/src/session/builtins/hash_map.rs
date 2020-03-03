use im::HashMap;

use crate::{
    heap::Heap,
    session::{Frame, GoalIndex, ModuleCache, Resolver, Trail, Unfolded, UnificationStack},
    word::{Address, Tag},
    SymbolIndex,
};

use super::{BuiltinContext, BuiltinState};

pub fn empty<'sesh, S, R>(ctx: &'sesh mut BuiltinContext<'sesh, S, R>, addr: Address) -> Unfolded
where
    R: Resolver,
{
    todo!()
}
