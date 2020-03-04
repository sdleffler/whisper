use std::{
    any::Any,
    fmt,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectPoint(usize);

impl ObjectPoint {
    pub const NULL: ObjectPoint = ObjectPoint(usize::max_value());
}

pub type BoxedObject = Box<dyn Any + Send + Sync>;

pub struct ObjectStack {
    objects: Vec<BoxedObject>,
}

impl fmt::Debug for ObjectStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ObjectStack")
            .field("top", &self.top())
            .finish()
    }
}

impl Index<ObjectIndex> for ObjectStack {
    type Output = dyn Any + Send + Sync;

    fn index(&self, idx: ObjectIndex) -> &Self::Output {
        &self.objects[idx.0]
    }
}

impl IndexMut<ObjectIndex> for ObjectStack {
    fn index_mut(&mut self, idx: ObjectIndex) -> &mut Self::Output {
        &mut self.objects[idx.0]
    }
}

impl ObjectStack {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.objects.clear();
    }

    pub fn top(&self) -> ObjectPoint {
        ObjectPoint(self.objects.len())
    }

    pub fn unwind(&mut self, point: ObjectPoint) {
        self.objects.truncate(point.0);
    }

    pub fn push<T: Any + Send + Sync>(&mut self, obj: T) -> ObjectIndex {
        let idx = ObjectIndex(self.objects.len());
        self.objects.push(Box::new(obj));
        idx
    }
}
