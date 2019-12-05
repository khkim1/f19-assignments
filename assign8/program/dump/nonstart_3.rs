extern crate assign8;
use assign8::cart::*;

fn main() {
  let nonempty = Cart<Checkout> {_user: String::from("khkim"),
                                 _items: Vec::new(),
                                 _marker: PhantomData<Checkout>};
}
