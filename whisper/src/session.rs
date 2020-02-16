use crate::{
    heap::Heap,
    knowledge_base::{KnowledgeBase, Matches, Module},
    query::{QueryMap, SharedQuery},
    word::{Address, Tag, Word},
    Symbol, SymbolIndex, SymbolTable,
};

use ::{
    failure::Fail,
    smallvec::SmallVec,
    std::{collections::HashMap, fmt, ops::Index},
};

mod builtins;

use builtins::BuiltinContext;

#[non_exhaustive]
#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "Foo.")]
    Foo,
}

/// A point in the trail which can be unwound to.
#[derive(Debug, Clone, Copy)]
pub struct Point {
    goal: usize,
    trail: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct GoalRef(usize);

#[derive(Debug, Clone)]
pub struct Goal {
    pub next: Option<GoalRef>,
    pub address: Word,
    pub module: ModuleIndex,
}

#[derive(Debug)]
pub struct Trail {
    trail: SmallVec<[Word; 64]>,
    goals: SmallVec<[Goal; 64]>,
}

impl Index<GoalRef> for Trail {
    type Output = Goal;

    fn index(&self, idx: GoalRef) -> &Self::Output {
        &self.goals[idx.0]
    }
}

impl Trail {
    pub fn new() -> Self {
        Self {
            trail: Default::default(),
            goals: Default::default(),
        }
    }

    pub fn clear(&mut self) {
        self.trail.clear();
        self.goals.clear();
    }

    #[inline(always)]
    pub fn get(&self) -> Point {
        Point {
            trail: self.trail.len(),
            goal: self.goals.len(),
        }
    }

    #[inline(always)]
    pub fn push(&mut self, word: Word) {
        self.trail.push(word);
    }

    #[inline]
    pub fn unwind(&mut self, point: Point, heap: &mut Heap) {
        if point.trail < self.trail.len() {
            for var in self.trail.drain(point.trail..) {
                heap[var.get_address()] = var;
            }
        }

        self.goals.truncate(point.goal);
    }

    #[inline]
    pub fn cons(&mut self, next: Option<GoalRef>, address: Word, module: ModuleIndex) -> GoalRef {
        let id = self.goals.len();
        self.goals.push(Goal {
            next,
            address,
            module,
        });
        GoalRef(id)
    }

    #[inline]
    pub fn append<I>(&mut self, tail: Option<GoalRef>, goals: I) -> Option<GoalRef>
    where
        I: IntoIterator<Item = (Word, ModuleIndex)>,
        I::IntoIter: DoubleEndedIterator,
    {
        goals.into_iter().rev().fold(tail, |acc, (addr, module)| {
            Some(self.cons(acc, addr, module))
        })
    }
}

/// The type of a stack frame in a `Session`.
///
/// Note that the root address of a frame is always the address of the *parent*
/// goal of whatever goal we're trying to prove; when unifying, only the head of
/// the newest potential subgoal is on the heap, so there's no constructed frame
/// for it yet. Because of this, a `Frame`'s trail point, when unwound, will undo
/// all assignments necessary to create the *parent* goal of a subgoal being
/// constructed; if unification fails, we don't want to unwind the parent goal
/// unless there are no other potential subgoals left to try.
#[non_exhaustive]
#[derive(Clone)]
pub struct Frame<S> {
    /// The address of the goal that this frame is trying to prove.
    root_address: Address,

    /// The trail point containing the necessary information to undo
    /// variable assignments created when building this frame.
    trail_point: Point,

    /// Possibly empty list of subgoals for the goal this frame is
    /// trying to prove. [GoalRef] is a handle while the actual data
    /// is stored in [Trail].
    goals: Option<GoalRef>,

    /// The remaining possible candidates for matches against this
    /// frame's first subgoal.
    matches: Matches,

    /// External state is stored on the stack so that it can be unwound
    /// when a relation fails.
    extern_state: S,
}

impl<S> fmt::Debug for Frame<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Frame")
            .field("root_address", &self.root_address)
            .field("trail_point", &self.trail_point)
            .field("goals", &self.goals)
            .field("matches", &self.matches.len())
            .finish()
    }
}

impl<S> Frame<S> {
    /// In Prolog, a "cut" locks searching of a specific predicate down a particular path.
    /// This can be useful for performance reasons, to avoid doing unnecessary work, or
    /// for control flow (usually discouraged.)
    ///
    /// This does a similar thing, by ensuring that after a cut, the frame will simply be
    /// unwound during backtracking rather than having the rest of its matches checked. Whatever
    /// goal that the frame was attempting to prove will not try any of the other options. So yeah it's
    /// basically a Prolog cut.
    #[inline]
    pub fn cut(&mut self) {
        self.matches.cut();
    }
}

/// State for unifying terms. Avoids recursion. Not sure why I'm doing that but hey that's how it
/// was done in the whitepaper this implementation is based on and I'm too lazy to change it. Also,
/// easy separation of unification state from the rest of the `Session`'s important state. Convenient
/// for passing stuff around.
#[derive(Debug)]
pub struct UnificationStack {
    stack: SmallVec<[(Word, Word); 64]>,
}

/// References to the state necessary for unification to take place, excluding the `UnificationStack` itself.
/// This is just condensing all the parameters threaded through the unification code into one place.
pub struct UnificationContext<'sesh, H: ExternHandler> {
    handler: &'sesh H,
    handler_context: &'sesh H::Context,
    handler_state: &'sesh mut H::State,
    trail: &'sesh mut Trail,
    heap: &'sesh mut Heap,
    base: usize,
}

impl UnificationStack {
    pub fn new() -> Self {
        Self {
            stack: SmallVec::new(),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.stack.clear();
    }

    #[inline]
    pub fn push(&mut self, l: Word, r: Word) {
        self.stack.push((l, r));
    }

    #[inline]
    pub fn init(&mut self, l: Word, r: Word) {
        self.clear();
        self.push(l, r);
    }

    #[inline]
    pub fn unify<H>(
        &mut self,
        handler: &H,
        handler_context: &H::Context,
        handler_state: &mut H::State,
        heap: &mut Heap,
        trail: &mut Trail,
        base: usize,
    ) -> bool
    where
        H: ExternHandler,
    {
        while let Some((l, r)) = self.stack.pop() {
            let (l, r) = (heap.chase(l), heap.chase(r));
            let ctx = UnificationContext {
                handler,
                handler_context,
                handler_state,
                heap,
                trail,
                base,
            };

            if l != r {
                if l.get_tag() == Tag::Var {
                    if r.get_tag() == Tag::Var {
                        self.unify_vars(ctx, l, r);
                    } else {
                        self.assign_var(ctx, l, r);
                    }
                } else if r.get_tag() == Tag::Var {
                    self.assign_var(ctx, r, l);
                } else if l.get_tag() == Tag::StructRef && r.get_tag() == Tag::StructRef {
                    if !self.unify_slices(ctx, l, r) {
                        return false;
                    }
                } else if (l.get_tag() == Tag::Cons && r.get_tag() == Tag::Cons)
                    || (l.get_tag() == Tag::Tagged && r.get_tag() == Tag::Tagged)
                {
                    self.push_range(&ctx.heap, l.get_address(), r.get_address(), 2);
                } else if l.get_tag() == Tag::Cons2 && r.get_tag() == Tag::Cons2 {
                    self.push_range(&ctx.heap, l.get_address(), r.get_address(), 3);
                } else if l.get_tag() == Tag::ExternRef && r.get_tag() == Tag::ExternRef {
                    if !ctx.handler.handle_unify(
                        ctx.handler_context,
                        ctx.handler_state,
                        self,
                        ctx.heap,
                        ctx.trail,
                        l.get_address(),
                        r.get_address(),
                    ) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

        true
    }

    #[inline]
    pub fn unify_vars<H>(&mut self, ctx: UnificationContext<H>, l: Word, r: Word)
    where
        H: ExternHandler,
    {
        // If both are variables, then we can guarantee that
        // l.0 > r.0 if and only if l.get_ref() > r.get_ref()
        // (because the tags are the same)
        if l.0 > r.0 {
            self.assign_var(ctx, l, r);
        } else {
            self.assign_var(ctx, r, l);
        }
    }

    #[inline]
    pub fn assign_var<H>(&mut self, ctx: UnificationContext<H>, var: Word, val: Word)
    where
        H: ExternHandler,
    {
        let var_addr = var.get_address();
        ctx.heap[var.get_address()] = val;
        if var_addr < ctx.base {
            ctx.trail.push(var);
        }
    }

    #[inline]
    pub fn unify_slices<H>(&mut self, ctx: UnificationContext<H>, lhs: Word, rhs: Word) -> bool
    where
        H: ExternHandler,
    {
        let (lhs_addr, rhs_addr) = (lhs.get_address(), rhs.get_address());
        let heap = &*ctx.heap;
        let l = heap[lhs_addr];
        let r = heap[rhs_addr];

        if l != r {
            return false;
        }

        self.push_range(ctx.heap, lhs_addr + 1, rhs_addr + 1, l.get_value() as usize);

        true
    }

    #[inline]
    pub fn push_range(&mut self, heap: &Heap, l_start: Address, r_start: Address, len: usize) {
        for i in 0..len {
            let l = heap[l_start + i];
            let r = heap[r_start + i];
            if l != r {
                self.stack.push((l, r));
            }
        }
    }
}

/// Represents the result of a single search for a solution. If a solution is found, then
/// `Yield::Solution` will be returned, and its parameter will be a clone of the external
/// term handler's state, from when the solution is found.
///
/// If `Yield::NoMoreSolutions` is found, then calling resuming the proof search will not
/// return more solutions.
#[derive(Debug, Clone, Copy)]
pub enum Yield<S> {
    Solution(S),
    NoMoreSolutions,
}

impl<S> Yield<S> {
    /// False if there are no more solutions, true otherwise.
    pub fn is_solution(&self) -> bool {
        match self {
            Yield::Solution(_) => true,
            Yield::NoMoreSolutions => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleIndex(usize);

#[derive(Debug)]
pub struct ModuleCache {
    knowledge: KnowledgeBase,
    module_table: HashMap<SymbolIndex, ModuleIndex>,
    modules: Vec<Module>,
}

impl Index<ModuleIndex> for ModuleCache {
    type Output = Module;

    fn index(&self, idx: ModuleIndex) -> &Module {
        &self.modules[idx.0]
    }
}

impl ModuleCache {
    pub fn new(knowledge: KnowledgeBase) -> Self {
        let mut this = Self {
            knowledge,
            module_table: HashMap::new(),
            modules: Vec::new(),
        };

        this.module_table.insert(Symbol::MOD_INDEX, ModuleIndex(0));
        this.modules.push(this.knowledge.get_root().clone());

        this
    }

    pub fn get_or_insert(&mut self, scope: SymbolIndex) -> ModuleIndex {
        match self.module_table.get(&scope).copied() {
            Some(midx) => midx,
            None => {
                let midx = ModuleIndex(self.modules.len());
                let module = self.knowledge.get(scope).unwrap().clone();

                self.module_table.insert(scope, midx);
                self.modules.push(module);

                midx
            }
        }
    }

    pub fn get_root(&self) -> ModuleIndex {
        ModuleIndex(0)
    }
}

pub type SimpleSession = Session<NullHandler>;

#[derive(Debug)]
pub struct Session<E: ExternHandler> {
    heap: Heap,
    handler: E,

    symbols: SymbolTable,
    modules: ModuleCache,

    unifier: UnificationStack,
    frames: SmallVec<[Frame<E::State>; 8]>,
    trail: Trail,

    query_vars: QueryMap,
}

// Statically assert that `Session` is `Send + Sync`.
#[allow(dead_code)]
fn assert_session_send_and_sync<E: ExternHandler>() {
    fn is_send_and_sync<T: Send + Sync>() {}
    is_send_and_sync::<Session<E>>();
}

impl<H: ExternHandler + Default> Session<H>
where
    H::State: Default,
{
    /// Shortcut for constructing a new `Session` when your `Handler` and `State` types both
    /// implement `Default` and you don't want to specify other values for them.
    pub fn new(symbols: SymbolTable, knowledge: KnowledgeBase) -> Self {
        Self::with_handler_and_state(symbols, knowledge, Default::default())
    }
}

impl<H: ExternHandler> Session<H>
where
    H::State: Default,
{
    /// Shortcut for constructing a new `Session` when your extern handler's `State` type has a useful default.
    pub fn with_handler(symbols: SymbolTable, knowledge: KnowledgeBase, handler: H) -> Self {
        Self::with_handler_and_state(symbols, knowledge, handler)
    }
}

impl<H: ExternHandler> Session<H> {
    /// Construct a new session with an external term handler and provide a starting state for the handler.
    pub fn with_handler_and_state(
        symbols: SymbolTable,
        knowledge: KnowledgeBase,
        handler: H,
    ) -> Self {
        assert_eq!(&symbols, knowledge.symbol_table());

        Self {
            heap: Heap::new(symbols.clone()),
            handler,

            symbols,
            modules: ModuleCache::new(knowledge),

            unifier: UnificationStack::new(),
            frames: SmallVec::new(),
            trail: Trail::new(),

            query_vars: QueryMap::new(),
        }
    }

    pub fn load(&mut self, query: SharedQuery)
    where
        H::State: Default,
    {
        self.load_with_extern_state(query, Default::default());
    }

    pub fn load_with_extern_state(&mut self, query: SharedQuery, extern_state: H::State) {
        assert_eq!(&self.symbols, query.symbol_table());

        let query = query.into_owned();

        self.trail.clear();
        self.frames.clear();
        self.unifier.clear();

        if !query.goals.is_empty() {
            let module_idx = self.modules.get_root();
            let first_goal = query.goals.first().copied().unwrap();

            self.frames.push(Frame {
                root_address: 0,
                trail_point: self.trail.get(),
                goals: self.trail.append(
                    None,
                    query
                        .goals
                        .iter()
                        .copied()
                        .map(|goal_addr| (goal_addr, module_idx)),
                ),
                matches: match first_goal.get_tag() {
                    Tag::StructRef => self.modules[module_idx].search(&query.heap, first_goal),
                    _ => Matches::empty(),
                },
                extern_state,
            });
        }

        self.heap = query.heap.into_owned();
        self.query_vars = query.vars;
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn query_vars(&self) -> &QueryMap {
        &self.query_vars
    }

    // TODO: Separate this function into smaller ones for clarity. I would do this now but I've had
    // so much goddamn espresso my left eye won't stop twitching.
    pub fn resume_with_context(&mut self, context: &H::Context) -> Yield<H::State> {
        // We proceed by a depth-first search over the proof space. Well, in fancy-speak.
        'searching: while let Some(frame) = self.frames.last_mut() {
            //println!("'searching");

            // We pick the first goal of the current stack frame and start matching against it.
            if let Some(goal) = frame.goals {
                let Goal {
                    next: next_goal,
                    address: goal_ref,
                    module: goal_module_idx,
                } = self.trail[goal];

                let undo_match = self.trail.get(); // This trail point lets us undo any change of state made by unification.
                let undo_addr = self.heap.len();

                //println!("goal: {}", self.heap.display_word(goal_ref));

                'backtrack: loop {
                    let goal_module = &self.modules[goal_module_idx];

                    let (mut remaining_goals, mut extern_state) = match goal_ref.get_tag() {
                        Tag::StructRef => {
                            'matching: loop {
                                let relation_id = match frame.matches.next() {
                                    Some(it) => it,
                                    None => break 'backtrack,
                                };

                                let relation = &goal_module[relation_id];
                                let head_ref = self.heap.copy_relation_head(goal_module, &relation);
                                self.unifier.init(head_ref, goal_ref);

                                // println!(
                                //     "Matching relation {:?} => {}... ",
                                //     relation_id,
                                //     self.heap.display_word(head_ref),
                                // );

                                let mut extern_state = frame.extern_state.clone();

                                if !self.unifier.unify(
                                    &self.handler,
                                    context,
                                    &mut extern_state,
                                    &mut self.heap,
                                    &mut self.trail,
                                    undo_addr,
                                ) {
                                    // println!("Unification failed.");
                                    // In this case, unifying the head failed, so we have to undo the
                                    // unification and put the heap back to where it was. Then, we try
                                    // the next candidate.
                                    self.trail.unwind(undo_match, &mut self.heap);
                                    self.heap.words.truncate(undo_addr);

                                    continue 'matching;
                                }

                                self.heap.copy_relation_body(goal_module, &relation);

                                // Unification has succeeded. We need to  map the absolute addresses in the relation's source heap
                                // address space to absolute addresses in the session heap, and then append those to the goal queue.
                                let relocated = relation.goals.iter().map(|g| {
                                    let relocated_addr =
                                        g.get_address() - relation.start.get_address() + undo_addr;
                                    let relocated_goal_word = g.with_address(relocated_addr);

                                    (relocated_goal_word, goal_module_idx)
                                });

                                let remaining_goals = self.trail.append(next_goal, relocated);

                                // println!("Success.");

                                break 'matching (remaining_goals, extern_state);
                            }
                        }
                        _ => (frame.goals, frame.extern_state.clone()),
                    };

                    let matches = 'finding_next_non_extern_goal: loop {
                        // println!("'finding_next_non_extern_goal");

                        if let Some(goal) = remaining_goals {
                            let Goal {
                                address: goal_ref,
                                module: goal_module_idx,
                                ..
                            } = self.trail[goal];
                            // println!("New goal `{}`; ", self.heap.display_word(goal_ref));

                            // Check to see if the newest goal is an extern or builtin goal.
                            let unfolded = match goal_ref.get_tag() {
                                Tag::StructRef => {
                                    let matches =
                                        self.modules[goal_module_idx].search(&self.heap, goal_ref);
                                    break 'finding_next_non_extern_goal matches;
                                }
                                Tag::ExternRef => self.handler.handle_goal(GoalContext {
                                    context,
                                    state: &mut extern_state,
                                    frame,
                                    heap: &mut self.heap,
                                    trail: &mut self.trail,
                                    unifier: &mut self.unifier,
                                    goal,
                                }),
                                Tag::OpaqueRef => builtins::handle_goal(&mut BuiltinContext {
                                    extern_handler: &self.handler,
                                    extern_context: context,
                                    extern_state: &mut extern_state,
                                    modules: &mut self.modules,
                                    frame,
                                    heap: &mut self.heap,
                                    trail: &mut self.trail,
                                    unifier: &mut self.unifier,
                                    goal,
                                }),
                                _ => unreachable!(
                                    "Goals should only start with an `Arity` or `Extern` word!"
                                ),
                            };

                            // If the builtin/extern goal failed, we have to backtrack to the next match of the frame that called it. Same
                            // procedure as we use when unification fails.
                            match unfolded {
                                Unfolded::Fail => {
                                    self.trail.unwind(undo_match, &mut self.heap);
                                    self.heap.words.truncate(undo_addr);

                                    continue 'backtrack; // Case 3: an extern goal has failed, and we have to backtrack.
                                }
                                Unfolded::Succeed(new_remaining_goals) => {
                                    remaining_goals = new_remaining_goals; // Note this is not a loop exit; we're just
                                                                           // popping the extern goal that just succeeded.
                                                                           // The continue here is just for clarity.
                                    continue 'finding_next_non_extern_goal;
                                }
                            }
                        } else {
                            if !frame.matches.is_empty() {
                                self.frames.push(Frame {
                                    root_address: undo_addr,
                                    trail_point: undo_match,
                                    goals: remaining_goals,
                                    matches: Matches::empty(),
                                    extern_state: extern_state.clone(),
                                });
                            }

                            return Yield::Solution(extern_state); // Case 2: we've found a solution.
                        }
                    };

                    // A minor optimization: if the previous frame is on its last match candidate, we can safely reuse it
                    // in a sort of tail-call situation.
                    // TODO: `ExactSizeIterator::exact_size_is_empty` is unstable; this is the same for now.
                    if frame.matches.is_empty() {
                        // println!(
                        //     "matches empty, continuing with tail substitution."
                        // );
                        frame.goals = remaining_goals;
                        frame.matches = matches;
                        frame.extern_state = extern_state;
                    } else {
                        // println!("match list nonempty, pushing new frame.");
                        self.frames.push(Frame {
                            root_address: undo_addr,
                            trail_point: undo_match,
                            goals: remaining_goals,
                            matches,
                            extern_state,
                        });
                    }

                    continue 'searching;
                }
            }

            // Either we're resuming from a prior call, or we've run out of matches to try.
            // We need to backtrack to get rid of old assignments before we can continue.
            // println!("Unwinding...");
            self.trail.unwind(frame.trail_point, &mut self.heap);
            self.heap.words.truncate(frame.root_address);
            self.frames.pop();
        }

        Yield::NoMoreSolutions
    }
}

impl<H: ExternHandler<Context = (), State = ()>> Session<H> {
    /// This function is a convenient shorthand for when you have a super simple (or noop) extern
    /// term handler. It's like `resume_with_context` but fills in the `()` context for you and
    /// automatically collapses `Yield` to a boolean since the state is `()` as well and carries
    /// no information.
    pub fn resume(&mut self) -> bool {
        self.resume_with_context(&()).is_solution()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Unfolded {
    Succeed(Option<GoalRef>),
    Fail,
}

pub struct GoalContext<'sesh, C: ?Sized, S> {
    pub context: &'sesh C,
    pub state: &'sesh mut S,
    pub frame: &'sesh mut Frame<S>,
    pub heap: &'sesh mut Heap,
    pub unifier: &'sesh mut UnificationStack,
    pub trail: &'sesh mut Trail,
    pub goal: GoalRef,
}

/// Describes the main hooks which can be registered to interact with `Extern` terms.
pub trait ExternHandler: Sized + Send + Sync {
    /// The `Context` type allows handlers to have per-resumption state they can inspect.
    type Context: ?Sized;

    /// The `State` type allows handlers to keep state which can be unwound for backtracking.
    /// Whenever your handler is doing mutable-state-things, it should be done through this.
    ///
    /// The actual `State` value is kept in the `Frame` type. It should be cheap to clone,
    /// since it will be cloned by `Session::resume_with_context`.
    type State: Send + Sync + Clone;

    /// Called when an `Extern` goal needs to be proved. This function allows an `Extern`
    /// goal to either fail or succeed and return a new goal list. A no-op implementation
    /// might look like:
    ///
    /// ```
    /// # extern crate whisper;
    /// # use whisper::{Heap, Address, session::{ExternHandler, Frame, Trail, GoalRef, Unfolded, GoalContext}};
    /// # struct NoopHandler;
    /// # impl ExternHandler for NoopHandler {
    /// # type Context = ();
    /// # type State = ();
    /// fn handle_goal(
    ///     &self,
    ///     ctx: GoalContext<Self::Context, Self::State>
    /// ) -> Unfolded {
    ///     Unfolded::Succeed(ctx.trail[ctx.goal].next)
    /// }
    /// # }
    /// ```
    fn handle_goal(&self, _goal_context: GoalContext<Self::Context, Self::State>) -> Unfolded {
        Unfolded::Fail
    }

    /// Called when an `Extern` term needs to be unified against another `Extern` term. Allows
    /// unification to fail or succeed and also add new unification goals. The default handler
    /// treats the extern term like a struct and attempts to unify its subterms; this is how
    /// it's implemented:
    ///
    /// ```
    /// # extern crate whisper;
    /// # use whisper::{Heap, Address, session::{ExternHandler, Trail, UnificationStack}};
    /// # struct NoopHandler;
    /// # impl ExternHandler for NoopHandler {
    /// # type Context = ();
    /// # type State = ();
    /// fn handle_unify(
    ///     &self,
    ///     _: &Self::Context,
    ///     _: &mut Self::State,
    ///     unifier: &mut UnificationStack,
    ///     heap: &mut Heap,
    ///     _trail: &mut Trail,
    ///     l: Address,
    ///     r: Address,
    /// ) -> bool {
    ///     // Return false if the two externs have different arities; otherwise,
    ///     // push all subterms onto the unification stack.
    ///     if heap[l].get_value() != heap[r].get_value() {
    ///         false
    ///     } else {
    ///         unifier.push_range(heap, l + 1, r + 1, heap[l].get_value() as usize);
    ///         true
    ///     }
    /// }
    /// # }
    /// ```
    fn handle_unify(
        &self,
        _ctx: &Self::Context,
        _state: &mut Self::State,
        unifier: &mut UnificationStack,
        heap: &mut Heap,
        _trail: &mut Trail,
        l: Address,
        r: Address,
    ) -> bool {
        // Return false if the two externs have different arities; otherwise,
        // push all subterms onto the unification stack.
        if heap[l].get_value() != heap[r].get_value() {
            false
        } else {
            unifier.push_range(heap, l + 1, r + 1, heap[l].get_value() as usize);
            true
        }
    }
}

impl<'a, E: ExternHandler> ExternHandler for &'a E {
    type Context = E::Context;
    type State = E::State;

    fn handle_goal(&self, goal_context: GoalContext<E::Context, E::State>) -> Unfolded {
        (*self).handle_goal(goal_context)
    }

    fn handle_unify(
        &self,
        ctx: &E::Context,
        state: &mut E::State,
        unifier: &mut UnificationStack,
        heap: &mut Heap,
        trail: &mut Trail,
        l: Address,
        r: Address,
    ) -> bool {
        (*self).handle_unify(ctx, state, unifier, heap, trail, l, r)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NullHandler;

impl ExternHandler for NullHandler {
    type Context = ();
    type State = ();
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DebugHandler;

impl ExternHandler for DebugHandler {
    type Context = ();
    type State = ();

    fn handle_goal(&self, goal_context: GoalContext<(), ()>) -> Unfolded {
        let GoalContext {
            heap, trail, goal, ..
        } = goal_context;

        println!("{}", heap.display_word(trail[goal].address));

        Unfolded::Succeed(trail[goal].next)
    }

    fn handle_unify(
        &self,
        _ctx: &(),
        _state: &mut (),
        unifier: &mut UnificationStack,
        heap: &mut Heap,
        _trail: &mut Trail,
        l: Address,
        r: Address,
    ) -> bool {
        println!("{} = {}", heap.display_at(l), heap.display_at(r));

        if heap[l].get_value() != heap[r].get_value() {
            false
        } else {
            unifier.push_range(heap, l + 1, r + 1, heap[l].get_value() as usize);
            true
        }
    }
}
