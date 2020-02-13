use crate::{
    heap::Heap,
    knowledge_base::SharedKnowledgeBase,
    session::{ExternHandler, Frame, GoalRef, Trail, Unfolded, UnificationStack},
    word::{Address, Tag},
    Symbol,
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

pub struct BuiltinContext<'sesh, H: ExternHandler> {
    pub heap: &'sesh mut Heap,
    pub extern_handler: &'sesh H,
    pub extern_context: &'sesh H::Context,
    pub extern_state: &'sesh mut H::State,
    pub knowledge: &'sesh mut SharedKnowledgeBase,
    pub unifier: &'sesh mut UnificationStack,
    pub frame: &'sesh mut Frame<H::State>,
    pub trail: &'sesh mut Trail,
    pub goal: GoalRef,
}

/// The `is` builtin unifies two terms. That's it. Nice and simple.
#[inline]
pub fn builtin_is<'sesh, H: ExternHandler>(
    ctx: &'sesh mut BuiltinContext<'sesh, H>,
    addr: Address,
) -> Unfolded {
    ctx.unifier.init(ctx.heap[addr + 2], ctx.heap[addr + 3]);

    // println!(
    //     "[is] unifying: `{}` = `{}`",
    //     ctx.heap.display_at(addr + 2),
    //     ctx.heap.display_at(addr + 3)
    // );

    if !ctx.unifier.unify(
        ctx.extern_handler,
        ctx.extern_context,
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
pub fn builtin_cut<'sesh, H: ExternHandler>(ctx: &'sesh mut BuiltinContext<'sesh, H>) -> Unfolded {
    ctx.frame.cut();
    Unfolded::Succeed(ctx.trail[ctx.goal].next)
}

#[inline]
pub fn builtin_try_in<'sesh, H: ExternHandler>(
    ctx: &'sesh mut BuiltinContext<'sesh, H>,
    addr: Address,
) -> Unfolded {
    let prev_next_goal = ctx.trail[ctx.goal].next;
    let next_goal = ctx.heap[addr + 2];

    let name_index = ctx.heap[addr + 3].debug_assert_tag(Tag::Const).get_value() as usize;
    let next_module = ctx.knowledge.get(name_index).cloned().unwrap();

    // println!(
    //     "`{}` in module with name `{}`",
    //     ctx.heap.display_word(next_goal),
    //     ctx.heap.symbols.normalize_full(next_module.get_name()),
    // );

    Unfolded::Succeed(Some(ctx.trail.cons(prev_next_goal, next_goal, next_module)))
}

pub fn handle_goal<'sesh, H: ExternHandler>(ctx: &'sesh mut BuiltinContext<'sesh, H>) -> Unfolded {
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

    match (arity, first) {
        (1, Symbol::CUT_INDEX) => builtin_cut(ctx),
        (3, Symbol::IS_INDEX) => builtin_is(ctx, goal_addr),
        (3, Symbol::LET_INDEX) => todo!("evaluation for `let` goals not implemented yet"),
        (3, Symbol::TRY_INDEX) => builtin_try_in(ctx, goal_addr),
        _ => todo!(),
    }
}
