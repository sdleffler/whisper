use ::std::{borrow::Borrow, mem, ops::Deref, rc::Rc};

#[derive(Debug)]
pub enum MaybeShared<'a, T> {
    Owned(T),
    Shared(Rc<T>),

    Borrowed(&'a T),
    BorrowedMut(&'a mut T),

    #[doc(hidden)]
    Empty,
}

use self::MaybeShared::*;

impl<'a, T> From<T> for MaybeShared<'a, T> {
    fn from(t: T) -> Self {
        Owned(t)
    }
}

impl<'a, T> From<&'a T> for MaybeShared<'a, T> {
    fn from(t: &'a T) -> Self {
        Borrowed(t)
    }
}

impl<'a, T> From<&'a mut T> for MaybeShared<'a, T> {
    fn from(t: &'a mut T) -> Self {
        BorrowedMut(t)
    }
}

impl<'a, T: Clone> Clone for MaybeShared<'a, T> {
    fn clone(&self) -> Self {
        match self {
            Owned(t) => Owned(t.clone()),
            Shared(p) => Shared(p.clone()),
            Borrowed(r) => Owned((**r).clone()),
            BorrowedMut(r) => Owned((**r).clone()),
            Empty => unreachable!(),
        }
    }
}

impl<'a, T> Deref for MaybeShared<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Owned(owned) => owned,
            Shared(shared) => &**shared,
            Borrowed(r) => &**r,
            BorrowedMut(r) => &**r,
            Empty => unreachable!(),
        }
    }
}

impl<'a, T> Borrow<T> for MaybeShared<'a, T> {
    fn borrow(&self) -> &T {
        &*self
    }
}

impl<'a, T: Clone> MaybeShared<'a, T> {
    pub fn into_owned(self) -> T {
        match self {
            Owned(owned) => owned,
            Shared(shared) => Rc::try_unwrap(shared).unwrap_or_else(|rc| (*rc).clone()),
            Borrowed(borrowed) => (*borrowed).clone(),
            BorrowedMut(borrowed) => (*borrowed).clone(),
            Empty => unreachable!(),
        }
    }

    pub fn to_shared<'b: 'a>(&mut self) -> MaybeShared<'b, T> {
        let shared = match mem::replace(self, Empty) {
            Owned(owned) => Shared(Rc::new(owned.clone())),
            Shared(shared) => Shared(shared),
            Borrowed(borrowed) => Shared(Rc::new((*borrowed).clone())),
            BorrowedMut(borrowed) => Shared(Rc::new((*borrowed).clone())),
            Empty => unreachable!(),
        };
        *self = shared.clone();
        shared
    }

    pub fn into_unscoped(self) -> MaybeShared<'static, T>
    where
        T: 'static,
    {
        match self {
            Owned(owned) => Owned(owned),
            Shared(shared) => Shared(shared),
            Borrowed(borrowed) => Owned(borrowed.to_owned()),
            BorrowedMut(borrowed_mut) => Owned(borrowed_mut.to_owned()),
            Empty => unreachable!(),
        }
    }

    pub fn to_mut(&mut self) -> &mut T {
        match self {
            Owned(owned) => owned,
            Shared(_) | Borrowed(_) => {
                let this = mem::replace(self, Empty);
                let owned = match this {
                    Shared(shared) => Rc::try_unwrap(shared).unwrap_or_else(|rc| (*rc).clone()),
                    Borrowed(borrowed) => borrowed.clone(),
                    _ => unreachable!(),
                };
                mem::replace(self, Owned(owned));

                match self {
                    Owned(owned) => owned,
                    _ => unreachable!(),
                }
            }
            BorrowedMut(r) => *r,
            Empty => unreachable!(),
        }
    }
}
