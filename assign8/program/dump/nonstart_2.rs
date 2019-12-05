extern crate assign8;
use assign8::cart::*;

fn main() {
  let nonempty = Cart<NonEmpty> {_user: String::from("khkim"),
                                 _items: Vec::new(),
                                 _marker: PhantomData<NonEmpty>};
}
