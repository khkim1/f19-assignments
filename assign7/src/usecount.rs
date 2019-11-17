/* Add UseCounter struct, methods, and trait implementations here */
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicUsize, Ordering};



pub struct UseCounter<T> {
    counter: AtomicUsize,
    value: T
}


impl<T> UseCounter<T> {
    pub fn new(val: T) -> UseCounter<T> {
        UseCounter { counter: AtomicUsize::new(0), value: val }
    }

    pub fn count(&self) -> usize {
        self.counter.load(Ordering::Relaxed)
    }
}


impl<T> Deref for UseCounter<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.counter.store(self.counter.load(Ordering::Relaxed)+1, Ordering::Relaxed);
        &self.value
    }
}


impl<T> DerefMut for UseCounter<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.counter.store(self.counter.load(Ordering::Relaxed)+1, Ordering::Relaxed);
        &mut self.value
    }
}

