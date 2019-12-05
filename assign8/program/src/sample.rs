// "S" is shorthand for "sigma", meaning "the rest of the session"
pub struct Send<T, S>(PhantomData<(T, S)>);
pub struct Recv<T, S>(PhantomData<(T, S)>);
pub struct Offer<Left, Right>(PhantomData<(Left, Right)>);
pub struct Choose<Left, Right>(PhantomData<(Left, Right)>);
pub struct Close; // equivalent to epsilon


type Id = String;
type AtmDeposit = Recv<u64, Send<u64, Close>>;
type AtmWithdraw = Recv<u64, Choose<Close, Close>>;
type AtmServer =
  Recv<Id,
  Choose<
    Offer<AtmDeposit, AtmWithdraw>,
    Close>>;
type AtmClient = <AtmServer as HasDual>::Dual;

type Server = Offer<Recv<u64, Close>, Send<u64, Close>>;
type Client = <Server as HasDual>::Dual;


fn approved(_id: &str) -> bool { true }


impl<T, S> Chan<Send<T, S>>
where
  T: marker::Send + 'static,
{
  pub fn send(self, x: T) -> Chan<S> {
    unsafe {
      self.write(x);
      transmute(self)
    }
  }
}

impl<T, S> Chan<Recv<T, S>>
where
  T: marker::Send + 'static,
{
  pub fn recv(self) -> (Chan<S>, T) {
    unsafe {
      let a = self.read();
      (transmute(self), a)
    }
  }
}

impl<Left, Right> Chan<Choose<Left, Right>> {
  pub fn left(self) -> Chan<Left> {
    unsafe {
      self.write(true);
      transmute(self)
    }
  }

  pub fn right(self) -> Chan<Right> {
    unsafe {
      self.write(false);
      transmute(self)
    }
  }
}

pub enum Branch<L, R> {
  Left(L),
  Right(R),
}

impl<Left, Right> Chan<Offer<Left, Right>> {
  pub fn offer(self) -> Branch<Chan<Left>, Chan<Right>> {
    unsafe {
      if self.read() {
        Branch::Left(transmute(self))
      } else {
        Branch::Right(transmute(self))
      }
    }
  }
}

fn server(c: Chan<Server>) {
  // offer should ask the client which branch to take
  match c.offer() {
    Branch::Left(c) => {
      let (c, n) = c.recv();
      println!("{}", n);
      c.close();
    }
    Branch::Right(c) => {
      let c = c.send(0);
      c.close();
    }
  }
}

fn client(c: Chan<Client>) {
  let c = c.left(); // tell server to run left branch
  let c = c.send(1); // send allowed because we're in left branch
  c.close();
}


pub fn atm_server(c: Chan<AtmServer>) {
  let (c, id) = c.recv();
  if !approved(&id) {
    c.right().close();
    return;
  }
  let mut balance = 100; // get balance for id

  let c = c.left();
  match c.offer() {
    Branch::Left(c) => { // Deposit
      let (c, amt) = c.recv();
      balance += amt;
      c.send(balance).close();
    }
    Branch::Right(c) => { // Withdraw
      let (c, amt) = c.recv();
      if balance >= amt {
        balance -= amt;
        c.left().close();
      } else {
        c.right().close();
      }
    }
  }
}

fn atm_client(c: Chan<AtmClient>) {
  let id = String::from("wcrichto");
  let c = c.send(id);
  match c.offer() {
    Branch::Left(c) => {
      let c = c.right(); // withdraw
      let c = c.send(105);
      match c.offer() {
        Branch::Left(c) => {
          println!("Withdrawl succeeded.");
          c.close();
        }
        Branch::Right(c) => {
          println!("Insufficient funds.");
          c.close()
        }
      }
    }
    Branch::Right(c) => {
      println!("Invalid authorization");
      c.close();
    }
  }
}

use std::thread;

fn main() {
  let (server_chan, client_chan) = Chan::new();
  let server_thread = thread::spawn(move || atm_server(server_chan));
  let client_thread = thread::spawn(move || atm_client(client_chan));

  server_thread.join().unwrap();
  client_thread.join().unwrap();

  // Prints "Insufficient funds."
}
