use crate::{
    heap::Heap,
    maybe_shared::MaybeShared,
    word::{Address, Word},
    Ident,
};

use ::{
    im::OrdMap,
    smallvec::SmallVec,
    std::{fmt, ops::Deref},
};

pub type SharedQuery = MaybeShared<'static, Query>;

pub type QueryMap = OrdMap<Ident, Address>;

/// A fully constructed query, ready to be executed on a [`KnowledgeBase`].
///
/// [`KnowledgeBase`]: crate::knowledge_base::KnowledgeBase
#[derive(Debug, Clone)]
pub struct Query {
    pub heap: Heap,
    pub goals: SmallVec<[Word; 8]>,
    pub vars: QueryMap,
}

impl Deref for Query {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        &self.heap
    }
}

impl fmt::Display for Query {
    // TODO: Display vars
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Query heap:\n{}", self.heap.display())?;
        for (i, &goal) in self.goals.iter().enumerate() {
            writeln!(f, "[GOAL{}] {}", i, self.heap.display_word(goal))?;
        }
        Ok(())
    }
}

impl Query {
    pub fn clear(&mut self) {
        self.heap.clear();
        self.goals.clear();
        self.vars.clear();
    }
}
