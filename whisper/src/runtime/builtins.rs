use std::any::Any;

use crate::{
    heap::Heap,
    runtime::{
        CacheIndex, ExternModule, Frame, GoalIndex, GoalType, ModuleCache, ObjectStack, Resolver,
        Trail, Unfolded, UnificationStack,
    },
    word::{Address, Tag},
    SymbolIndex,
};

#[macro_export]
macro_rules! unpack {
    (match ($e:expr) { $($i:ident($p:pat) $(=> $v:expr)?,)+ }) => {
        match $e.unpack() {
            $(UnpackedWord::$i($p) => ($($v,)? (),).0,)+
            _ => return Unfolded::Fail,
        }
    };
}

pub struct BuiltinContext<'sesh, E: ExternModule, R: Resolver<E>> {
    pub state: &'sesh mut BuiltinState,
    pub tail_frames: &'sesh mut [Frame<E::State>],
    pub heap: &'sesh mut Heap,
    pub modules: &'sesh mut ModuleCache<E>,
    pub resolver: &'sesh R,
    pub unifier: &'sesh mut UnificationStack,
    pub trail: &'sesh mut Trail,
    pub objects: &'sesh mut ObjectStack,
    pub goal: GoalIndex,
}

#[derive(Debug)]
pub enum BuiltinState {
    Boxed(Box<dyn Any + Send + Sync>),
    Uninit,
}

#[inline]
pub fn builtin_cut<'sesh, E, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, E, R>,
    _addr: Address,
) -> Unfolded
where
    E: ExternModule,
    R: Resolver<E>,
{
    let goal = &ctx.trail[ctx.goal];

    if let Some(frame) = ctx.tail_frames.get_mut(goal.frame_index) {
        frame.cut();
    }

    Unfolded::Succeed {
        next: goal.next,
        empty: true,
    }
}

/// The `is` builtin unifies two terms. That's it. Nice and simple.
#[inline]
pub fn builtin_is<'sesh, E, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, E, R>,
    addr: Address,
) -> Unfolded
where
    E: ExternModule,
    R: Resolver<E>,
{
    ctx.unifier.init(ctx.heap[addr + 2], ctx.heap[addr + 3]);

    // println!(
    //     "[is] unifying: `{}` = `{}`",
    //     ctx.heap.display_at(addr + 2),
    //     ctx.heap.display_at(addr + 3)
    // );

    if !ctx.unifier.unify(ctx.heap, ctx.trail, ctx.heap.len()) {
        Unfolded::Fail
    } else {
        Unfolded::Succeed {
            next: ctx.trail[ctx.goal].next,
            empty: true,
        }
    }
}

#[inline]
pub fn builtin_try_in<'sesh, E, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, E, R>,
    addr: Address,
) -> Unfolded
where
    E: ExternModule,
    R: Resolver<E>,
{
    let prev_next_goal = ctx.trail[ctx.goal].next;
    let next_goal = ctx.heap[addr + 2];
    let next_module = ctx
        .modules
        .get_or_insert(ctx.heap[addr + 3], ctx.heap, ctx.resolver);

    let goal_type = match next_module {
        CacheIndex::Dynamic(module_idx) => GoalType::Dynamic(module_idx),
        CacheIndex::Extern(ext_idx) => GoalType::Extern(ext_idx),
    };

    // println!(
    //     "`{}` in module with name `{}`",
    //     ctx.heap.display_word(next_goal),
    //     ctx.heap.symbols.normalize_full(next_module.get_name()),
    // );

    Unfolded::Succeed {
        next: Some(
            ctx.trail
                .cons(prev_next_goal, next_goal, goal_type, ctx.tail_frames.len()),
        ),
        empty: true,
    }
}

#[inline]
pub fn builtin_println<'sesh, E, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, E, R>,
    addr: Address,
) -> Unfolded
where
    E: ExternModule,
    R: Resolver<E>,
{
    println!("{}", ctx.heap.display_at(addr + 2));

    Unfolded::Succeed {
        next: ctx.trail[ctx.goal].next,
        empty: true,
    }
}

pub fn handle_goal<'sesh, E, R>(ctx: &'sesh mut BuiltinContext<'sesh, E, R>) -> Unfolded
where
    E: ExternModule,
    R: Resolver<E>,
{
    let goal_addr = ctx.trail[ctx.goal]
        .address
        .debug_assert_tag(Tag::OpaqueRef)
        .get_address();

    let arity = ctx.heap[goal_addr + 0]
        .debug_assert_tag(Tag::OpaqueArity)
        .get_value() as usize;

    let first = ctx.heap[goal_addr + 1]
        .debug_assert_tag(Tag::Const)
        .get_value() as usize;

    match (arity, ctx.heap.symbols().index(first)) {
        (1, SymbolIndex::CUT) => builtin_cut(ctx, goal_addr),
        (2, SymbolIndex::PRINTLN) => builtin_println(ctx, goal_addr),
        (3, SymbolIndex::IS) => builtin_is(ctx, goal_addr),
        (3, SymbolIndex::LET) => todo!("evaluation for `let` goals not implemented yet"),
        (3, SymbolIndex::TRY) => builtin_try_in(ctx, goal_addr),
        _ => todo!(),
    }
}
