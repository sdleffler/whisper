use crate::{
    heap::Heap,
    session::{
        ExternHandler, Frame, GoalRef, ModuleCache, Resolver, Trail, Unfolded, UnificationStack,
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

pub struct BuiltinContext<'sesh, H: ExternHandler, R: Resolver> {
    pub heap: &'sesh mut Heap,
    pub extern_handler: &'sesh mut H,
    pub extern_state: &'sesh mut H::State,
    pub modules: &'sesh mut ModuleCache,
    pub resolver: &'sesh R,
    pub unifier: &'sesh mut UnificationStack,
    pub frame: &'sesh mut Frame<H::State>,
    pub trail: &'sesh mut Trail,
    pub goal: GoalRef,
}

/// The `is` builtin unifies two terms. That's it. Nice and simple.
#[inline]
pub fn builtin_is<'sesh, H, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, H, R>,
    addr: Address,
) -> Unfolded
where
    H: ExternHandler,
    R: Resolver,
{
    ctx.unifier.init(ctx.heap[addr + 2], ctx.heap[addr + 3]);

    // println!(
    //     "[is] unifying: `{}` = `{}`",
    //     ctx.heap.display_at(addr + 2),
    //     ctx.heap.display_at(addr + 3)
    // );

    if !ctx.unifier.unify(
        ctx.extern_handler,
        ctx.extern_state,
        ctx.heap,
        ctx.trail,
        ctx.heap.len(),
    ) {
        Unfolded::Fail
    } else {
        Unfolded::Succeed(ctx.trail[ctx.goal].next)
    }
}

#[inline]
pub fn builtin_cut<'sesh, H, R>(ctx: &'sesh mut BuiltinContext<'sesh, H, R>) -> Unfolded
where
    H: ExternHandler,
    R: Resolver,
{
    ctx.frame.cut();
    Unfolded::Succeed(ctx.trail[ctx.goal].next)
}

#[inline]
pub fn builtin_try_in<'sesh, H, R>(
    ctx: &'sesh mut BuiltinContext<'sesh, H, R>,
    addr: Address,
) -> Unfolded
where
    H: ExternHandler,
    R: Resolver,
{
    let prev_next_goal = ctx.trail[ctx.goal].next;
    let next_goal = ctx.heap[addr + 2];
    let next_module = ctx
        .modules
        .get_or_insert(ctx.heap[addr + 3], ctx.heap, ctx.resolver);

    // println!(
    //     "`{}` in module with name `{}`",
    //     ctx.heap.display_word(next_goal),
    //     ctx.heap.symbols.normalize_full(next_module.get_name()),
    // );

    Unfolded::Succeed(Some(ctx.trail.cons(prev_next_goal, next_goal, next_module)))
}

pub fn handle_goal<'sesh, H, R>(ctx: &'sesh mut BuiltinContext<'sesh, H, R>) -> Unfolded
where
    H: ExternHandler,
    R: Resolver,
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
        (1, SymbolIndex::CUT) => builtin_cut(ctx),
        (3, SymbolIndex::IS) => builtin_is(ctx, goal_addr),
        (3, SymbolIndex::LET) => todo!("evaluation for `let` goals not implemented yet"),
        (3, SymbolIndex::TRY) => builtin_try_in(ctx, goal_addr),
        _ => todo!(),
    }
}
