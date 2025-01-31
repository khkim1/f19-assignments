use take_mut;

/*
 * Core futures interface.
 */

#[derive(Debug)]
pub enum Poll<T> {
  Ready(T),
  NotReady,
}

pub trait Future: Send {
  type Item: Send;
  fn poll(&mut self) -> Poll<Self::Item>;
}



/*
 * Example implementation of a future for an item that returns immediately.
 */

// Container for the state of the future.
pub struct Immediate<T> {
  t: Option<T>,
}

// Constructor to build the future. Note that the return type just says
// "this produces a future", not specifying concretely the type Immediate.
pub fn immediate<T>(t: T) -> impl Future<Item = T>
where
  T: Send,
{
  Immediate { t: Some(t) }
}

// To treat Immediate as a future, we have to implement poll. Here it's
// relatively simple, since we return immediately with a Poll::Ready.
impl<T> Future for Immediate<T>
where
  T: Send,
{
  type Item = T;

  fn poll(&mut self) -> Poll<Self::Item> {
    Poll::Ready(self.t.take().unwrap())
  }
}


/*
 * Example implementation of a future combinator that applies a function to
 * the output of a future.
 */

struct Map<Fut, Fun> {
  fut: Fut,
  fun: Option<Fun>,
}

pub fn map<T, Fut, Fun>(fut: Fut, fun: Fun) -> impl Future<Item = T>
where
  T: Send,
  Fut: Future,
  Fun: FnOnce(Fut::Item) -> T + Send,
{
  Map {
    fut,
    fun: Some(fun),
  }
}

impl<T, Fut, Fun> Future for Map<Fut, Fun>
where
  T: Send,
  Fut: Future,
  Fun: FnOnce(Fut::Item) -> T + Send,
{
  type Item = T;

  fn poll(&mut self) -> Poll<Self::Item> {
    match self.fut.poll() {
      Poll::NotReady => Poll::NotReady,
      Poll::Ready(s) => {
        let f = self.fun.take();
        Poll::Ready(f.unwrap()(s))
      }
    }
  }
}


/*
 * Part 1a - Join
 */

// A join of two futures is a state machine depending on which future is
// completed, represented as an enum.
pub enum Join<F, G>
where
  F: Future,
  G: Future,
{
  BothRunning(F, G),
  FirstDone(F::Item, G),
  SecondDone(F, G::Item),
  Done,
}

// When a join is created, we start by assuming neither child future
// has completed.
pub fn join<F, G>(f: F, g: G) -> impl Future<Item = (F::Item, G::Item)>
where
  F: Future,
  G: Future,
{
  Join::BothRunning(f, g)
}

impl<F, G> Future for Join<F, G>
where
  F: Future,
  G: Future,
{
  type Item = (F::Item, G::Item);

  fn poll(&mut self) -> Poll<Self::Item> {
    // Store result in variable
    let mut result:Poll<Self::Item> = Poll::NotReady;

    // Poll until self is Done
    while match self { Join::Done => false, _ => true} {

      take_mut::take(self, |join_own: Join<F, G>| -> Join<F, G> {

        match join_own {
          Join::BothRunning(mut f, mut g) => {
            match f.poll() {
              Poll::Ready(s) => { Join::FirstDone(s, g) }
              Poll::NotReady => {
                match g.poll() {
                  Poll::Ready(s) => { Join::SecondDone(f, s) } // Q. How do I still use f here?
                  Poll::NotReady => { Join::BothRunning(f, g) }
                }
              }
            }
          }
          Join::FirstDone(f_ret, mut g) => {
            match g.poll() {
              Poll::Ready(s) => { result = Poll::Ready((f_ret, s)); Join::Done }
              Poll::NotReady => { Join::FirstDone(f_ret, g) }
            }
          }
          Join::SecondDone(mut f, g_ret) => {
            match f.poll() {
              Poll::Ready(s) => { result = Poll::Ready((s, g_ret)); Join::Done }
              Poll::NotReady => { Join::SecondDone(f, g_ret) }
            }
          }
          Join::Done => { unreachable!() }
        }
      });
    }
    // Return the result
    result
  }
}

/*
 * Part 1b - AndThen
 */

// The AndThen state machine depends on which future is currently running.
pub enum AndThen<Fut1, Fut2, Fun> {
  First(Fut1, Fun),
  Second(Fut2),
  Done,
}

pub fn and_then<Fut1, Fut2, Fun>(fut: Fut1, fun: Fun)
                                 -> impl Future<Item = Fut2::Item>
where
  Fut1: Future,
  Fut2: Future,
  Fun: FnOnce(Fut1::Item) -> Fut2 + Send,
{
  AndThen::First(fut, fun)
}

impl<Fut1, Fut2, Fun> Future for AndThen<Fut1, Fut2, Fun>
where
  Fut1: Future,
  Fut2: Future,
  Fun: FnOnce(Fut1::Item) -> Fut2 + Send,
{
  type Item = Fut2::Item;

  fn poll(&mut self) -> Poll<Self::Item> {

    let mut result:Poll<Self::Item> = Poll::NotReady;

    // Poll until self is Done
    while match self { AndThen::Done => false, _ => true} {
      take_mut::take(self, |at_own: AndThen<Fut1, Fut2, Fun>| -> AndThen<Fut1, Fut2, Fun> {
        match at_own {
          AndThen::First(mut fut1, fun) => {
            match fut1.poll() {
              Poll::NotReady => { AndThen::First(fut1, fun) }
              Poll::Ready(s) => { AndThen::Second(fun(s)) }
            }
          }
          AndThen::Second(mut fut2) => {
            match fut2.poll() {
              Poll::NotReady => { AndThen::Second(fut2) }
              Poll::Ready(s) => { result = Poll::Ready(s); AndThen::Done }
            }
          }
          AndThen::Done => { unreachable!() }
        }
      });
    }

    // Return results
    result
  }
}
