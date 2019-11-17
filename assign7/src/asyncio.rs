use future::*;
use std::path::PathBuf;
use std::thread;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::fs;
use std::io;


pub struct FileReader {
  path: PathBuf,
  thread: Option<thread::JoinHandle<io::Result<String>>>,
  done_flag: Arc<AtomicBool>,
}

impl FileReader {
  pub fn new(path: PathBuf) -> FileReader {

    let mut done_flag = Arc::new(AtomicBool::new(false));

    let done_cp = done_flag.clone();
    let path_cp = path.clone();

    let thread = Some(thread::spawn( move || {
      let read_str = fs::read_to_string(path_cp);

      // let df_own = df_ref.unwrap();
      done_cp.store(true, Ordering::Relaxed);
      read_str
    }));

    FileReader { path: path, thread: thread, done_flag: done_flag }
  }
}

impl Future for FileReader {
  type Item = io::Result<String>;

  fn poll(&mut self) -> Poll<Self::Item> {

    match self.done_flag.load(Ordering::Relaxed) {
      false => { Poll::NotReady }
      true => { Poll::Ready( self.thread.take().unwrap().join().unwrap() ) }
    }
  }
}
