extern crate assign8;
use assign8::cart::*;

fn main() {
  let empty = Cart::login("A".into(), "B".into()).unwrap();
  let nonempty = empty.additem(1.0);
  let check = nonempty.checkout();
  let (check, err) = check.order().unwrap();
  // Assumes order always fails
  check.order();
}
