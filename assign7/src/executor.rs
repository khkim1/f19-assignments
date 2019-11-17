use std::mem;
use std::sync::{mpsc, Mutex, Arc};
use std::thread;
use future::{Future, Poll};

/*
 * Core executor interface.
 */

pub trait Executor {
  fn spawn<F>(&mut self, f: F)
  where
    F: Future<Item = ()> + 'static;
  fn wait(&mut self);
}


/*
 * Example implementation of a naive executor that executes futures
 * in sequence.
 */

pub struct BlockingExecutor;

impl BlockingExecutor {
  pub fn new() -> BlockingExecutor {
    BlockingExecutor
  }
}

impl Executor for BlockingExecutor {
  fn spawn<F>(&mut self, mut f: F)
  where
    F: Future<Item = ()>,
  {
    loop {
      if let Poll::Ready(()) = f.poll() {
        break;
      }
    }
  }

  fn wait(&mut self) {}
}

/*
 * Part 2a - Single threaded executor
 */

pub struct SingleThreadExecutor {
  futures: Vec<Box<dyn Future<Item = ()>>>,
}

impl SingleThreadExecutor {
  pub fn new() -> SingleThreadExecutor {
    SingleThreadExecutor { futures: vec![] }
  }
}

impl Executor for SingleThreadExecutor {
  fn spawn<F>(&mut self, mut f: F)
  where
    F: Future<Item = ()> + 'static,
  {
    match f.poll() {
      Poll::Ready(_) => {  }
      Poll::NotReady => { self.futures.push(Box::new(f)); }
    }
  }

  fn wait(&mut self) {

    let mut comp_vec = Vec::new();

    while self.futures.len() != comp_vec.len() {

      for (i, fut) in self.futures.iter_mut().enumerate() {

        if !comp_vec.contains(&i) {
          match fut.poll() {
            Poll::NotReady => {}
            Poll::Ready(_) => { comp_vec.push(i) }
          }
        } else {}
      }
    }
  }
}

pub struct MultiThreadExecutor {
  sender: mpsc::Sender<Option<Box<dyn Future<Item = ()>>>>,
  threads: Vec<thread::JoinHandle<()>>,
}

impl MultiThreadExecutor {
  pub fn new(num_threads: i32) -> MultiThreadExecutor {

    // Create a shared channel
    let (sender, receiver) = mpsc::channel();
    let receiver = Arc::new(Mutex::new(receiver));
    let mut thread_vec = Vec::new();

    // Spawn a pool of threads
    for _i in 0..num_threads {

      let receiver_ref = receiver.clone();

      thread_vec.push( thread::spawn(move || {
        // Create single thread executor
        let mut sing_exec = SingleThreadExecutor::new();

        loop {
          // Pull work from channel
          let receiver_own = receiver_ref.lock().unwrap();
          let message = receiver_own.recv().unwrap();

          // Spawn work with message
          match message {
            Some(msg) => { sing_exec.spawn(msg); }
            None => { sing_exec.wait(); return; }
          }
        }
      }));
    }
    // Return the MultiThreadExecutor
    MultiThreadExecutor { sender: sender, threads: thread_vec }
  }
}

impl Executor for MultiThreadExecutor {
  fn spawn<F>(&mut self, f: F)
  where
    F: Future<Item = ()> + 'static,
  {
    self.sender.send(Some(Box::new(f))).unwrap();
  }

  fn wait(&mut self) {
    // Loop through the threads while acquiring ownership
    for _i in 0..self.threads.len() {
      self.sender.send(None).unwrap();
    }

    while !self.threads.is_empty() {
      let thr = self.threads.pop();
      match thr {
        None => {}
        Some(thr) => {thr.join().unwrap()}
      }
    }
  }
}
