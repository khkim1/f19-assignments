extern crate assign8;
use assign8::cart::*;

fn main() {
  let start = Cart::login("A".into(), "B".into()).unwrap();
  // Alway assume login fails
  start.login("A".into(), "B".into()).unwrap();
}
