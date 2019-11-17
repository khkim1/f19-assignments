use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

struct Bank {
    cash: i32
}

fn deposit(the_bank: &Arc<Mutex<Bank>>, n: i32) {
    let mut bank_ref = the_bank.lock().unwrap();
    bank_ref.cash += n;
}

fn withdraw(the_bank: &Arc<Mutex<Bank>>, n: i32) {
    let mut bank_ref = the_bank.lock().unwrap();
    bank_ref.cash -= n;
}

fn customer(the_bank: Arc<Mutex<Bank>>) {
    for _ in 0..100 {
        deposit(&the_bank, 2);
    }

    for _ in 0..100 {
        withdraw(&the_bank, 2);
    }
}

fn main() {
  let the_bank: Arc<Mutex<Bank>> =
      Arc::new(Mutex::new(Bank { cash: 0 }));

  let n = 32;
  let handles = (0..n).iter().map(|_| {
    let bank_ref = the_bank.clone();
    thread::spawn(|| {
      customer(bank_ref)
    })
  });

  for handle in handles {
    handle.join().unwrap();
  }

  println!("Total: {}", the_bank.lock().unwrap().cash);
}



