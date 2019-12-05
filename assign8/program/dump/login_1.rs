extern crate assign8;
use assign8::cart::*;

fn main() {
  let empty = Cart::login("A".into(), "B".into());
  empty.additem(1.0);
}

