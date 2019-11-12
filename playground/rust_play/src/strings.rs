


enum List<T> { Nil, Cons(T, Box(List<T>)) }

pub fn run() {

    let y : Box<List<i32>> = Box::new([1, 2, 3, 4]);
    println!("{:?}", y);

}
