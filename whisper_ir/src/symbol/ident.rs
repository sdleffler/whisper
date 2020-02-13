use ::{
    serde::{Deserialize, Serialize},
    std::{
        fmt,
        sync::atomic::{AtomicU64, Ordering},
    },
};

use crate::symbol::Atom;

static NEXT_UNIQUE_ID: AtomicU64 = AtomicU64::new(1);

/// An identifier with a string and integer component.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Ident {
    #[doc(hidden)]
    /// This is public SPECIFICALLY for the `ident!` macro, and should not be
    /// touched by user code!
    pub atom: Atom,

    #[doc(hidden)]
    /// This is public SPECIFICALLY for the `ident!` macro, and should not be
    /// touched by user code!
    pub id: u64,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Ident")
            .field(&self.atom)
            .field(&self.id)
            .finish()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.id == 0 {
            self.atom.fmt(f)
        } else {
            write!(f, "{}#{}", self.atom, self.id)
        }
    }
}

impl<'a> From<&'a Ident> for Ident {
    fn from(ident: &'a Ident) -> Self {
        ident.clone()
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Self::from(Atom::from(s))
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, rhs: &str) -> bool {
        self.id == 0 && &self.atom == rhs
    }
}

impl PartialEq<Ident> for str {
    fn eq(&self, rhs: &Ident) -> bool {
        rhs.eq(self)
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Self::from(Atom::from(s))
    }
}

impl From<usize> for Ident {
    fn from(id: usize) -> Self {
        Self {
            atom: atom!(""),
            id: id as u64,
        }
    }
}

impl From<u64> for Ident {
    fn from(id: u64) -> Self {
        Self {
            atom: atom!(""),
            id,
        }
    }
}

impl From<Atom> for Ident {
    fn from(atom: Atom) -> Self {
        Self { atom, id: 0 }
    }
}

impl From<Ident> for Atom {
    fn from(ident: Ident) -> Self {
        ident.atom
    }
}

impl<T: From<Atom>> From<Ident> for (T, u64) {
    fn from(ident: Ident) -> Self {
        (ident.atom.into(), ident.id)
    }
}

impl Ident {
    pub const fn from_parts(atom: Atom, id: u64) -> Self {
        Self { atom, id }
    }

    pub const fn from_atom(atom: Atom) -> Self {
        Self { atom, id: 0 }
    }

    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn atom(&self) -> &Atom {
        &self.atom
    }

    pub fn str_ref(&self) -> &str {
        &self.atom.as_ref()
    }

    pub fn gensym() -> Self {
        Self {
            atom: atom!(""),
            id: NEXT_UNIQUE_ID.fetch_add(1, Ordering::SeqCst),
        }
    }

    pub fn gensym_with_name(name: impl Into<Atom>) -> Self {
        Self {
            atom: name.into(),
            id: NEXT_UNIQUE_ID.fetch_add(1, Ordering::SeqCst),
        }
    }
}

#[macro_export]
macro_rules! ident_internal {
    ($atom:tt) => {
        $crate::symbol::ident::Ident {
            atom: atom!($atom),
            id: 0,
        }
    };
}

#[macro_export]
macro_rules! ident {
    ($atom:tt) => {
        $crate::symbol::ident::Ident {
            atom: $crate::atom!($atom),
            id: 0,
        }
    };
}
