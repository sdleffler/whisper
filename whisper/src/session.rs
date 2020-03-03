use crate::{
    heap::Heap,
    knowledge_base::{KnowledgeBase, Matches, Module},
    query::{Query, QueryMap},
    word::{Address, Tag, Word},
    SymbolIndex, SymbolTable,
};

use ::{
    failure::Error,
    smallvec::SmallVec,
    std::{any::Any, collections::HashMap, fmt, marker::PhantomData, mem, ops::Index},
    whisper_ir::{trans::TermEmitter, IrKnowledgeBase, IrTermGraph},
    whisper_schema::{SchemaArena, SchemaGraph},
};

mod builtins;
pub mod goal;

use builtins::{BuiltinContext, BuiltinState};
pub use goal::Goal;

/// A point in the trail which can be unwound to.
#[derive(Debug, Clone, Copy)]
pub struct Point {
    goal: usize,
    trail: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct GoalIndex(usize);

#[derive(Debug, Clone)]
pub struct GoalNode {
    pub next: Option<GoalIndex>,
    pub address: Word,
    pub module: ModuleIndex,
    pub frame_index: usize,
}

#[derive(Debug, Clone, Default)]
pub struct Trail {
    trail: SmallVec<[Word; 64]>,
    goals: SmallVec<[GoalNode; 64]>,
}

impl Index<GoalIndex> for Trail {
    type Output = GoalNode;

    fn index(&self, idx: GoalIndex) -> &Self::Output {
        &self.goals[idx.0]
    }
}

impl Trail {
    pub fn new() -> Self {
        Self::default()
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
    pub fn cons(
        &mut self,
        next: Option<GoalIndex>,
        address: Word,
        module: ModuleIndex,
        frame_index: usize,
    ) -> GoalIndex {
        let id = self.goals.len();
        self.goals.push(GoalNode {
            next,
            address,
            module,
            frame_index,
        });
        GoalIndex(id)
    }

    #[inline]
    pub fn append<I>(
        &mut self,
        tail: Option<GoalIndex>,
        frame_index: usize,
        goals: I,
    ) -> Option<GoalIndex>
    where
        I: IntoIterator<Item = (Word, ModuleIndex)>,
        I::IntoIter: DoubleEndedIterator,
    {
        goals.into_iter().rev().fold(tail, |acc, (addr, module)| {
            Some(self.cons(acc, addr, module, frame_index))
        })
    }
}

pub enum Remaining<S> {
    Matches(Matches),
    External(S),
    Builtin(BuiltinState),
    Uninit,
    Empty,
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
pub struct Frame<S> {
    /// The address of the goal that this frame is trying to prove.
    root_address: Address,

    /// The trail point containing the necessary information to undo
    /// variable assignments created when building this frame.
    trail_point: Point,

    /// The top of the object stack at the time this frame was created,
    /// so that when unwinding we can remove objects which are no longer
    /// needed.
    objects_top: usize,

    /// Possibly empty list of subgoals for the goal this frame is
    /// trying to prove. [GoalIndex] is a handle while the actual data
    /// is stored in [Trail].
    goals: Option<GoalIndex>,

    /// Remaining alternatives to try for this goal.
    alts: Remaining<S>,
}

impl<S> fmt::Debug for Frame<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Frame")
            .field("root_address", &self.root_address)
            .field("trail_point", &self.trail_point)
            .field("goals", &self.goals)
            .field("alts", &"Remaining")
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
        self.alts = Remaining::Empty;
    }
}

/// State for unifying terms. Avoids recursion. Not sure why I'm doing that but hey that's how it
/// was done in the whitepaper this implementation is based on and I'm too lazy to change it. Also,
/// easy separation of unification state from the rest of the `Session`'s important state. Convenient
/// for passing stuff around.
#[derive(Debug, Default)]
pub struct UnificationStack {
    stack: SmallVec<[(Word, Word); 64]>,
}

/// References to the state necessary for unification to take place, excluding the `UnificationStack` itself.
/// This is just condensing all the parameters threaded through the unification code into one place.
pub struct UnificationContext<'sesh> {
    trail: &'sesh mut Trail,
    heap: &'sesh mut Heap,
    base: usize,
}

impl UnificationStack {
    pub fn new() -> Self {
        Self::default()
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
    pub fn unify(&mut self, heap: &mut Heap, trail: &mut Trail, base: usize) -> bool {
        while let Some((l, r)) = self.stack.pop() {
            let (l, r) = (heap.chase(l), heap.chase(r));
            let ctx = UnificationContext { heap, trail, base };

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
                } else {
                    return false;
                }
            }
        }

        true
    }

    #[inline]
    pub fn unify_vars(&mut self, ctx: UnificationContext, l: Word, r: Word) {
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
    pub fn assign_var(&mut self, ctx: UnificationContext, var: Word, val: Word) {
        let var_addr = var.get_address();
        ctx.heap[var.get_address()] = val;
        if var_addr < ctx.base {
            ctx.trail.push(var);
        }
    }

    #[inline]
    pub fn unify_slices(&mut self, ctx: UnificationContext, lhs: Word, rhs: Word) -> bool {
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
pub enum Yield {
    Solution,
    NoMoreSolutions,
}

impl Yield {
    /// False if there are no more solutions, true otherwise.
    pub fn is_solution(&self) -> bool {
        match self {
            Yield::Solution => true,
            Yield::NoMoreSolutions => false,
        }
    }
}

pub trait Resolver: Send + Sync {
    fn symbols(&self) -> &SymbolTable;
    fn resolve(&self, idx: Word, heap: &Heap) -> Option<Module>;
    fn root(&self) -> Module;
}

impl Resolver for KnowledgeBase {
    fn symbols(&self) -> &SymbolTable {
        self.symbols()
    }

    fn resolve(&self, idx: Word, heap: &Heap) -> Option<Module> {
        self.get(SymbolIndex(
            idx.debug_assert_tag(Tag::Const).get_value() as usize,
            heap.symbols().id(),
        ))
        .cloned()
    }

    fn root(&self) -> Module {
        self.root().clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleIndex(usize);

#[derive(Debug, Clone, Default)]
pub struct ModuleCache {
    module_table: HashMap<Word, ModuleIndex>,
    modules: Vec<Module>,
}

impl Index<ModuleIndex> for ModuleCache {
    type Output = Module;

    fn index(&self, idx: ModuleIndex) -> &Module {
        &self.modules[idx.0]
    }
}

impl ModuleCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn init(&mut self, resolver: &impl Resolver) {
        self.module_table.clear();
        self.modules.clear();
        let root = resolver.root();
        self.insert(Word::r#const(root.name().0), root);
    }

    fn insert(&mut self, scope: Word, module: Module) -> ModuleIndex {
        let midx = ModuleIndex(self.modules.len());
        self.modules.push(module);
        self.module_table.insert(scope, midx);
        midx
    }

    pub fn get_or_insert(
        &mut self,
        scope: Word,
        heap: &Heap,
        resolver: &impl Resolver,
    ) -> ModuleIndex {
        match self.module_table.get(&scope).copied() {
            Some(midx) => midx,
            None => {
                let module = resolver.resolve(scope, heap).unwrap();
                self.insert(scope, module)
            }
        }
    }

    pub fn root(&self) -> ModuleIndex {
        ModuleIndex(0)
    }
}

#[derive(Debug)]
pub struct Machine<T: ExternFrame> {
    unifier: UnificationStack,
    frames: SmallVec<[Frame<T>; 8]>,
    trail: Trail,
    objects: Vec<Box<dyn Any + Send + Sync>>,
    module_cache: ModuleCache,
}

impl<T: ExternFrame> Default for Machine<T> {
    fn default() -> Self {
        Self {
            unifier: UnificationStack::new(),
            frames: SmallVec::new(),
            trail: Trail::new(),
            module_cache: ModuleCache::new(),
            objects: Vec::new(),
        }
    }
}

impl<T: ExternFrame> Machine<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn init(&mut self, resolver: &impl Resolver) {
        self.unifier.clear();
        self.frames.clear();
        self.trail.clear();
        self.objects.clear();
        self.module_cache.init(resolver);
    }
}

#[derive(Debug)]
pub struct SimpleSession<H>
where
    H: ExternHandler<Resolver = KnowledgeBase>,
{
    pub machine: Machine<H::State>,
    pub heap: Heap,
    pub knowledge_base: KnowledgeBase,
    pub handler: H,
}

impl<H> SimpleSession<H>
where
    H: ExternHandler<Resolver = KnowledgeBase>,
{
    pub fn new<F>(symbols: SymbolTable, kb: F) -> Self
    where
        F: FnOnce(&mut IrTermGraph) -> IrKnowledgeBase,
        H: Default,
    {
        let mut terms = IrTermGraph::new(symbols);
        let modules = kb(&mut terms);
        let knowledge_base = crate::trans::knowledge_base(&terms, &modules);
        let mut machine = Machine::new();
        machine.init(&knowledge_base);
        let heap = Heap::new(knowledge_base.symbols().clone());
        let handler = H::default();

        Self {
            machine,
            heap,
            knowledge_base,
            handler,
        }
    }

    pub fn run(&mut self) -> Runtime<H> {
        Runtime::new(
            &mut self.machine,
            &mut self.heap,
            &mut self.handler,
            &self.knowledge_base,
        )
    }
}

#[derive(Debug)]
pub struct Runtime<'all, H: ExternHandler> {
    unifier: &'all mut UnificationStack,
    frames: &'all mut SmallVec<[Frame<H::State>; 8]>,
    trail: &'all mut Trail,
    objects: &'all mut Vec<Box<dyn Any + Send + Sync>>,
    module_cache: &'all mut ModuleCache,
    heap: &'all mut Heap,
    handler: &'all mut H,
    resolver: &'all H::Resolver,
}

impl<'all, H: ExternHandler> Runtime<'all, H> {
    pub fn new(
        machine: &'all mut Machine<H::State>,
        heap: &'all mut Heap,
        handler: &'all mut H,
        resolver: &'all H::Resolver,
    ) -> Self {
        Self {
            unifier: &mut machine.unifier,
            frames: &mut machine.frames,
            trail: &mut machine.trail,
            objects: &mut machine.objects,
            module_cache: &mut machine.module_cache,
            heap,
            handler,
            resolver,
        }
    }

    pub fn from_query(
        machine: &'all mut Machine<H::State>,
        handler: &'all mut H,
        resolver: &'all H::Resolver,
        query: &'all mut Query,
    ) -> Self {
        Self {
            unifier: &mut machine.unifier,
            frames: &mut machine.frames,
            trail: &mut machine.trail,
            objects: &mut machine.objects,
            module_cache: &mut machine.module_cache,
            heap: &mut query.heap,
            handler,
            resolver,
        }
    }

    pub fn solve_once<G>(&mut self, goal: G) -> Result<Option<G::Output>, Error>
    where
        G: Goal,
    {
        let mut goal_addrs = SmallVec::<[Word; 8]>::new();
        let arena = SchemaArena::new();
        let schema_graph = SchemaGraph::new(&arena);
        let mut emitter = TermEmitter::new(schema_graph, self.heap.write_top());
        goal.emit(&mut emitter, &mut goal_addrs)?;

        self.trail.clear();
        self.frames.clear();
        self.unifier.clear();
        self.objects.clear();

        if !goal_addrs.is_empty() {
            let module_idx = self.module_cache.root();
            self.frames.push(Frame {
                root_address: 0,
                trail_point: self.trail.get(),
                objects_top: 0,
                goals: self.trail.append(
                    None,
                    0,
                    goal_addrs
                        .iter()
                        .copied()
                        .map(|goal_addr| (goal_addr, module_idx)),
                ),
                alts: Remaining::Uninit,
            });
        }

        if self.resume().is_solution() {
            let extracted = G::extract(
                &mut goal_addrs
                    .iter()
                    .map(|&w| self.heap.read_at(w.get_address())),
            )?;

            Ok(Some(extracted))
        } else {
            Ok(None)
        }
    }

    pub fn solve_all<G>(&mut self, goal: G) -> Result<Vec<G::Output>, Error>
    where
        G: Goal,
    {
        let mut goal_addrs = SmallVec::<[Word; 8]>::new();
        let arena = SchemaArena::new();
        let schema_graph = SchemaGraph::new(&arena);
        let mut emitter = TermEmitter::new(schema_graph, self.heap.write_top());
        goal.emit(&mut emitter, &mut goal_addrs)?;

        self.trail.clear();
        self.frames.clear();
        self.unifier.clear();

        if !goal_addrs.is_empty() {
            let module_idx = self.module_cache.root();
            self.frames.push(Frame {
                root_address: 0,
                trail_point: self.trail.get(),
                objects_top: 0,
                goals: self.trail.append(
                    None,
                    0,
                    goal_addrs
                        .iter()
                        .copied()
                        .map(|goal_addr| (goal_addr, module_idx)),
                ),
                alts: Remaining::Uninit,
            });
        }

        let mut solved = Vec::new();
        while self.resume().is_solution() {
            let extracted = G::extract(
                &mut goal_addrs
                    .iter()
                    .map(|&w| self.heap.read_at(w.get_address())),
            )?;
            solved.push(extracted);
        }

        Ok(solved)
    }

    pub fn resume(&mut self) -> Yield {
        // We proceed by a depth-first search over the proof space. Well, in fancy-speak.
        'searching: while let Some((frame, tail_frames)) = self.frames.split_last_mut() {
            println!("'searching");

            if let Some(goal) = frame.goals {
                let GoalNode {
                    next: next_goal,
                    address: goal_ref,
                    module: goal_module_idx,
                    ..
                } = self.trail[goal];

                let undo_trail = self.trail.get();
                let undo_addr = self.heap.len();
                let undo_objects = self.objects.len();

                let goal_module = &self.module_cache[goal_module_idx];

                let remaining_goals = match frame.alts {
                    Remaining::Matches(ref mut matches) => {
                        'matching: loop {
                            let relation_id = match matches.next() {
                                Some(it) => it,
                                None => {
                                    self.trail.unwind(frame.trail_point, &mut self.heap);
                                    self.heap.words.truncate(frame.root_address);
                                    self.objects.truncate(frame.objects_top);
                                    self.frames.pop();
                                    continue 'searching;
                                }
                            };

                            let relation = &goal_module[relation_id];
                            let head_ref = self.heap.copy_relation_head(goal_module, &relation);
                            self.unifier.init(head_ref, goal_ref);

                            println!(
                                "Matching relation {:?} => {} against {}... ",
                                relation_id,
                                self.heap.display_word(head_ref),
                                self.heap.display_word(goal_ref),
                            );

                            if !self
                                .unifier
                                .unify(&mut *self.heap, &mut *self.trail, undo_addr)
                            {
                                // println!("Unification failed.");
                                // In this case, unifying the head failed, so we have to undo the
                                // unification and put the heap back to where it was. Then, we try
                                // the next candidate.
                                self.trail.unwind(undo_trail, &mut self.heap);
                                self.heap.words.truncate(undo_addr);
                                // No need to unwind the object stack here as this could not possibly
                                // have created any.

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

                            // Workaround: #![feature(exact_size_is_empty)]. we stable bois
                            if matches.len() == 0 {
                                frame.alts = Remaining::Empty;
                            }

                            println!("Success.");

                            break 'matching self.trail.append(
                                next_goal,
                                tail_frames.len(),
                                relocated,
                            );
                        }
                    }
                    Remaining::External(ref mut state) => {
                        let unfolded = self.handler.handle_goal(
                            ExternContext {
                                modules: &mut *self.module_cache,
                                resolver: &*self.resolver,
                                heap: &mut *self.heap,
                                trail: &mut *self.trail,
                                unifier: &mut *self.unifier,
                                goal,
                            },
                            state,
                        );

                        match unfolded {
                            Unfolded::Succeed { next, empty } => {
                                if empty {
                                    frame.alts = Remaining::Empty;
                                }

                                next
                            }
                            Unfolded::Fail => {
                                self.trail.unwind(frame.trail_point, &mut self.heap);
                                self.heap.words.truncate(frame.root_address);
                                self.objects.truncate(frame.objects_top);
                                self.frames.pop();
                                continue 'searching;
                            }
                        }
                    }
                    Remaining::Builtin(ref mut state) => {
                        let unfolded = builtins::handle_goal(&mut BuiltinContext {
                            state,
                            tail_frames,
                            modules: &mut *self.module_cache,
                            resolver: &*self.resolver,
                            heap: &mut *self.heap,
                            trail: &mut *self.trail,
                            unifier: &mut *self.unifier,
                            goal,
                        });

                        match unfolded {
                            Unfolded::Succeed { next, empty } => {
                                if empty {
                                    frame.alts = Remaining::Empty;
                                }

                                next
                            }
                            Unfolded::Fail => {
                                self.trail.unwind(frame.trail_point, &mut self.heap);
                                self.heap.words.truncate(frame.root_address);
                                self.objects.truncate(frame.objects_top);
                                self.frames.pop();
                                continue 'searching;
                            }
                        }
                    }
                    Remaining::Uninit => {
                        frame.alts = Remaining::Empty;
                        Some(goal)
                    }
                    Remaining::Empty => {
                        self.trail.unwind(frame.trail_point, &mut self.heap);
                        self.heap.words.truncate(frame.root_address);
                        self.objects.truncate(frame.objects_top);
                        self.frames.pop();
                        continue 'searching;
                    }
                };

                if let Some(goal) = remaining_goals {
                    let GoalNode {
                        address: goal_ref,
                        module: goal_module_idx,
                        ..
                    } = self.trail[goal];

                    let alts = match goal_ref.get_tag() {
                        Tag::StructRef => {
                            let matches =
                                self.module_cache[goal_module_idx].search(&self.heap, goal_ref);
                            Remaining::Matches(matches)
                        }
                        Tag::ExternRef => {
                            // TODO because need to figure out what data the handler needs for this
                            let init_state = self.handler.init_state(ExternContext {
                                modules: &mut *self.module_cache,
                                resolver: &*self.resolver,
                                heap: &mut *self.heap,
                                trail: &mut *self.trail,
                                unifier: &mut *self.unifier,
                                goal,
                            });
                            Remaining::External(init_state)
                        }
                        Tag::OpaqueRef => Remaining::Builtin(BuiltinState::Uninit),
                        _ => unreachable!(
                            "Goals should only start with an `Arity`, `Extern`, or `Opaque` word!"
                        ),
                    };

                    if let Remaining::Empty = frame.alts {
                        frame.alts = alts;
                        frame.goals = remaining_goals;
                    } else {
                        self.frames.push(Frame {
                            root_address: undo_addr,
                            trail_point: undo_trail,
                            objects_top: undo_objects,
                            goals: remaining_goals,
                            alts,
                        });
                    }

                    continue 'searching;
                } else {
                    return Yield::Solution;
                }
            }

            // // We pick the first goal of the current stack frame and start matching against it.
            // if let Some(goal) = frame.goals {
            //     let GoalNode {
            //         next: next_goal,
            //         address: goal_ref,
            //         module: goal_module_idx,
            //     } = self.trail[goal];

            //     let undo_match = self.trail.get(); // This trail point lets us undo any change of state made by unification.
            //     let undo_addr = self.heap.len();

            //     //println!("goal: {}", self.heap.display_word(goal_ref));

            //     'backtrack: loop {
            //         let goal_module = &self.module_cache[goal_module_idx];

            //         let (mut remaining_goals, mut extern_state) = match goal_ref.get_tag() {
            //             Tag::StructRef => {
            //                 'matching: loop {
            //                     let relation_id = match frame.matches.next() {
            //                         Some(it) => it,
            //                         None => break 'backtrack,
            //                     };

            //                     let relation = &goal_module[relation_id];
            //                     let head_ref = self.heap.copy_relation_head(goal_module, &relation);
            //                     self.unifier.init(head_ref, goal_ref);

            //                     // println!(
            //                     //     "Matching relation {:?} => {}... ",
            //                     //     relation_id,
            //                     //     self.heap.display_word(head_ref),
            //                     // );

            //                     if !self.unifier.unify(
            //                         &mut *self.handler,
            //                         &mut *self.heap,
            //                         &mut *self.trail,
            //                         undo_addr,
            //                     ) {
            //                         // println!("Unification failed.");
            //                         // In this case, unifying the head failed, so we have to undo the
            //                         // unification and put the heap back to where it was. Then, we try
            //                         // the next candidate.
            //                         self.trail.unwind(undo_match, &mut self.heap);
            //                         self.heap.words.truncate(undo_addr);

            //                         continue 'matching;
            //                     }

            //                     self.heap.copy_relation_body(goal_module, &relation);

            //                     // Unification has succeeded. We need to  map the absolute addresses in the relation's source heap
            //                     // address space to absolute addresses in the session heap, and then append those to the goal queue.
            //                     let relocated = relation.goals.iter().map(|g| {
            //                         let relocated_addr =
            //                             g.get_address() - relation.start.get_address() + undo_addr;
            //                         let relocated_goal_word = g.with_address(relocated_addr);

            //                         (relocated_goal_word, goal_module_idx)
            //                     });

            //                     let remaining_goals = self.trail.append(next_goal, relocated);

            //                     // println!("Success.");

            //                     break 'matching (remaining_goals, extern_state);
            //                 }
            //             }
            //             _ => (frame.goals, frame.extern_state.clone()),
            //         };

            //         'finding_next_non_extern_goal: loop {
            //             // println!("'finding_next_non_extern_goal");

            //             if let Some(goal) = remaining_goals {
            //                 let GoalNode {
            //                     address: goal_ref,
            //                     module: goal_module_idx,
            //                     ..
            //                 } = self.trail[goal];
            //                 // println!("New goal `{}`; ", self.heap.display_word(goal_ref));

            //                 // Check to see if the newest goal is an extern or builtin goal.
            //                 let unfolded = match goal_ref.get_tag() {
            //                     Tag::StructRef => {
            //                         let matches = self.module_cache[goal_module_idx]
            //                             .search(&self.heap, goal_ref);

            //                         // A minor optimization: if the previous frame is on its last match candidate, we can safely reuse it
            //                         // in a sort of tail-call situation.
            //                         // TODO: `ExactSizeIterator::exact_size_is_empty` is unstable; this is the same for now.
            //                         if frame.matches.is_empty() {
            //                             // println!(
            //                             //     "matches empty, continuing with tail substitution."
            //                             // );
            //                             frame.goals = remaining_goals;
            //                             frame.matches = matches;
            //                             frame.extern_state = extern_state;
            //                         } else {
            //                             // println!("match list nonempty, pushing new frame.");
            //                             self.frames.push(Frame {
            //                                 root_address: undo_addr,
            //                                 trail_point: undo_match,
            //                                 goals: remaining_goals,
            //                                 matches,
            //                                 extern_state,
            //                             });
            //                         }

            //                         continue 'searching;
            //                     }
            //                     Tag::ExternRef => self.handler.handle_goal(ExternContext {
            //                         state: &mut extern_state,
            //                         frame,
            //                         modules: &mut *self.module_cache,
            //                         resolver: &*self.resolver,
            //                         heap: &mut *self.heap,
            //                         trail: &mut *self.trail,
            //                         unifier: &mut *self.unifier,
            //                         goal,
            //                     }),
            //                     Tag::OpaqueRef => builtins::handle_goal(&mut BuiltinContext {
            //                         extern_handler: &mut *self.handler,
            //                         extern_state: &mut extern_state,
            //                         modules: &mut *self.module_cache,
            //                         resolver: &*self.resolver,
            //                         frame,
            //                         heap: &mut *self.heap,
            //                         trail: &mut *self.trail,
            //                         unifier: &mut *self.unifier,
            //                         goal,
            //                     }),
            //                     _ => unreachable!(
            //                         "Goals should only start with an `Arity` or `Extern` word!"
            //                     ),
            //                 };

            //                 // If the builtin/extern goal failed, we have to backtrack to the next match of the frame that called it. Same
            //                 // procedure as we use when unification fails.
            //                 match unfolded {
            //                     Unfolded::Fail => {
            //                         self.trail.unwind(undo_match, &mut *self.heap);
            //                         self.heap.words.truncate(undo_addr);

            //                         continue 'backtrack; // Case 3: an extern goal has failed, and we have to backtrack.
            //                     }
            //                     Unfolded::Succeed {
            //                         next: new_remaining_goals,
            //                         empty,
            //                     } => {
            //                         // Optimization: if this unfolding was successful and the builtin/extern goal has no more "matches" to
            //                         // yield, then we can simply continue this loop.
            //                         if empty {
            //                             remaining_goals = new_remaining_goals; // Note this is not a loop exit; we're just
            //                                                                    // popping the extern goal that just succeeded.
            //                                                                    // The continue here is just for clarity.
            //                             continue 'finding_next_non_extern_goal;
            //                         } else {
            //                             self.frames.push(Frame {});
            //                         }
            //                     }
            //                 }
            //             } else {
            //                 if !frame.matches.is_empty() {
            //                     self.frames.push(Frame {
            //                         root_address: undo_addr,
            //                         trail_point: undo_match,
            //                         goals: remaining_goals,
            //                         matches: Matches::empty(),
            //                         extern_state: extern_state.clone(),
            //                     });
            //                 }

            //                 return Yield::Solution(extern_state); // Case 2: we've found a solution.
            //             }
            //         }
            //     }
            // }

            // // Either we're resuming from a prior call, or we've run out of matches to try.
            // // We need to backtrack to get rid of old assignments before we can continue.
            // // println!("Unwinding...");
            // self.trail.unwind(frame.trail_point, &mut self.heap);
            // self.heap.words.truncate(frame.root_address);
            // self.frames.pop();
        }

        Yield::NoMoreSolutions
    }
}

#[derive(Debug)]
pub struct Session<T: ExternFrame> {
    heap: Heap,

    unifier: UnificationStack,
    frames: SmallVec<[Frame<T>; 8]>,
    objects: Vec<Box<dyn Any + Send + Sync>>,
    trail: Trail,

    query_vars: QueryMap,
}

// Statically assert that `Session` is `Send + Sync`.
#[allow(dead_code)]
fn assert_session_send_and_sync<T: ExternFrame>() {
    fn is_send_and_sync<T: Send + Sync>() {}
    is_send_and_sync::<Session<T>>();
}

impl<T: ExternFrame> Session<T> {
    /// Construct a new session.
    pub fn new(symbols: SymbolTable) -> Self {
        Self {
            heap: Heap::new(symbols),

            unifier: UnificationStack::new(),
            frames: SmallVec::new(),
            objects: Vec::new(),
            trail: Trail::new(),

            query_vars: QueryMap::new(),
        }
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.heap.symbols
    }

    pub fn load(&mut self, mut query: Query, modules: &ModuleCache)
    where
        T: Default,
    {
        self.load_and_reuse_query(&mut query, modules);
    }

    /// This function leaves the `query` passed by mut holding *garbage.* It will swap
    /// the two heaps, allowing the previous query's `Heap` and `QueryMap` to be reused.
    /// *After calling this function, the `query` will not be the same `Query`. This
    /// function should only be used if you know what you are doing and are going for
    /// performance.*
    pub fn load_and_reuse_query(&mut self, query: &mut Query, modules: &ModuleCache) {
        assert_eq!(self.symbols(), query.symbols());

        self.trail.clear();
        self.frames.clear();
        self.unifier.clear();
        self.objects.clear();

        if !query.goals.is_empty() {
            let module_idx = modules.root();
            self.frames.push(Frame {
                root_address: 0,
                trail_point: self.trail.get(),
                objects_top: 0,
                goals: self.trail.append(
                    None,
                    0,
                    query
                        .goals
                        .iter()
                        .copied()
                        .map(|goal_addr| (goal_addr, module_idx)),
                ),
                alts: Remaining::Uninit,
            });
        }

        mem::swap(&mut self.heap, &mut query.heap);
        mem::swap(&mut self.query_vars, &mut query.vars);
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn query_vars(&self) -> &QueryMap {
        &self.query_vars
    }

    // TODO: Separate this function into smaller ones for clarity. I would do this now but I've had
    // so much goddamn espresso my left eye won't stop twitching.
    pub fn resume_with_context<H>(
        &mut self,
        modules: &mut ModuleCache,
        handler: &mut H,
        resolver: &H::Resolver,
    ) -> Yield
    where
        H: ExternHandler<State = T>,
    {
        let mut runtime = Runtime {
            unifier: &mut self.unifier,
            frames: &mut self.frames,
            trail: &mut self.trail,
            objects: &mut self.objects,
            module_cache: modules,
            heap: &mut self.heap,
            handler,
            resolver,
        };

        runtime.resume()
    }
}

impl Session<()> {
    pub fn resume(&mut self, modules: &mut ModuleCache, knowledge_base: &KnowledgeBase) -> bool {
        self.resume_with_context(modules, &mut NullHandler::default(), knowledge_base)
            .is_solution()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Unfolded {
    Succeed {
        next: Option<GoalIndex>,
        empty: bool,
    },
    Fail,
}

pub struct ExternContext<'sesh, R: Resolver> {
    pub modules: &'sesh mut ModuleCache,
    pub resolver: &'sesh R,
    pub heap: &'sesh mut Heap,
    pub unifier: &'sesh mut UnificationStack,
    pub trail: &'sesh mut Trail,
    pub goal: GoalIndex,
}

impl<'sesh, R: Resolver> ExternContext<'sesh, R> {
    pub fn as_unification_context_with_handler<'ctx>(
        &'ctx mut self,
    ) -> (&'ctx mut UnificationStack, UnificationContext<'ctx>) {
        let uni_ctx = UnificationContext {
            trail: &mut *self.trail,
            base: self.heap.len(),
            heap: &mut *self.heap,
        };
        let unifier = &mut *self.unifier;

        (unifier, uni_ctx)
    }
}

pub trait ExternFrame: Send + Sync + 'static {}
impl<T: Send + Sync + 'static> ExternFrame for T {}

/// Describes the main hooks which can be registered to interact with `Extern` terms.
pub trait ExternHandler: Sized {
    /// The `State` type allows handlers to keep state which can be unwound for backtracking.
    /// Whenever your handler is doing mutable-state-things, it should be done through this.
    ///
    /// The actual `State` value is kept in the `Frame` type. It should be cheap to clone,
    /// since it will be cloned by `Session::resume_with_context`.
    type State: ExternFrame;

    type Resolver: Resolver;

    fn init_state(&mut self, _goal_context: ExternContext<Self::Resolver>) -> Self::State;

    /// Called when an `Extern` goal needs to be proved. This function allows an `Extern`
    /// goal to either fail or succeed and return a new goal list. A no-op implementation
    /// might look like:
    ///
    /// ```
    /// # extern crate whisper;
    /// # use whisper::{Heap, Address, session::{ExternHandler, Frame, Trail, GoalIndex, Unfolded, ExternContext}};
    /// # struct NoopHandler;
    /// # impl ExternHandler for NoopHandler {
    /// # type Context = ();
    /// # type State = ();
    /// fn handle_goal(
    ///     &self,
    ///     ctx: ExternContext<Self::Context, Self::State>
    /// ) -> Unfolded {
    ///     Unfolded::Succeed(ctx.trail[ctx.goal].next)
    /// }
    /// # }
    /// ```
    fn handle_goal(
        &mut self,
        _goal_context: ExternContext<Self::Resolver>,
        _state: &mut Self::State,
    ) -> Unfolded {
        Unfolded::Fail
    }
}

impl<'a, E: ExternHandler> ExternHandler for &'a mut E {
    type State = E::State;
    type Resolver = E::Resolver;

    fn init_state(&mut self, goal_context: ExternContext<Self::Resolver>) -> Self::State {
        (*self).init_state(goal_context)
    }

    fn handle_goal(
        &mut self,
        goal_context: ExternContext<Self::Resolver>,
        state: &mut Self::State,
    ) -> Unfolded {
        (*self).handle_goal(goal_context, state)
    }
}

pub struct NullHandler<R: Resolver>(PhantomData<R>);

impl<R: Resolver> fmt::Debug for NullHandler<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("NullHandler").finish()
    }
}

impl<R: Resolver> Default for NullHandler<R> {
    fn default() -> Self {
        NullHandler(PhantomData)
    }
}

impl<R: Resolver> ExternHandler for NullHandler<R> {
    type State = ();
    type Resolver = R;

    fn init_state(&mut self, _goal_context: ExternContext<Self::Resolver>) -> Self::State {
        ()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DebugHandler<R: Resolver>(PhantomData<R>);

impl<R: Resolver> Default for DebugHandler<R> {
    fn default() -> Self {
        DebugHandler(PhantomData)
    }
}

impl<R: Resolver> ExternHandler for DebugHandler<R> {
    type State = ();
    type Resolver = R;

    fn init_state(&mut self, _goal_context: ExternContext<R>) -> Self::State {
        ()
    }

    fn handle_goal(&mut self, goal_context: ExternContext<R>, _state: &mut ()) -> Unfolded {
        let ExternContext {
            heap, trail, goal, ..
        } = goal_context;

        println!("{}", heap.display_word(trail[goal].address));

        Unfolded::Succeed {
            next: trail[goal].next,
            empty: true,
        }
    }
}
