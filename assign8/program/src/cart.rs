use backend;
use backend::{UserId, order, login};
use std::marker::PhantomData;


pub struct Start;
pub struct Empty;
pub struct NonEmpty;
pub struct Checkout;

pub struct Cart<S> {
    _user: UserId,
    _items: Vec::<f64>,
    _marker: PhantomData<S>
}

pub enum LoginResult { Ok(Cart<Empty>), Fail(Cart<Start>) }
pub enum OrderResult { Ok(Cart<Empty>), Fail(Cart<Checkout>) }

impl Cart<Start> {
    pub fn login (_username: String, _password: String) -> Result<Cart<Empty>, String> {
        // The backend login function never fails
        match backend::login(_username, _password) {
            Ok(uid) => { Ok(Cart {_user: uid, _items: Vec::new(), _marker: PhantomData }) }
            Err(s) => { Err(s) }
        }
    }
}


impl Cart<Empty> {
    // take ownership of cart and return new cart
    pub fn additem(mut self, item: f64) -> Cart<NonEmpty> {
        self._items.push(item);
        Cart {_user: self._user, _items: self._items, _marker: PhantomData}
    }
}


impl Cart<NonEmpty> {

    pub fn additem(mut self, item: f64) -> Cart<NonEmpty> {
        self._items.push(item);
        Cart {_user: self._user, _items: self._items, _marker: PhantomData}
    }

    pub fn clearitems(self) -> Cart<Empty> {
        Cart {_user: self._user, _items: Vec::new(), _marker: PhantomData }
    }

    pub fn checkout(self) -> Cart<Checkout> {
        Cart {_user: self._user, _items: self._items, _marker: PhantomData}
    }
}


impl Cart<Checkout> {
    pub fn order(self) -> Result<Cart<Empty>, (Cart<Checkout>, String)> {

        let total: f64 = self._items.iter().sum();

        match backend::order(&self._user, total) {
            Ok(()) => { Ok (Cart {_user: self._user, _items: Vec::new(), _marker: PhantomData }) }
            Err(s) => Err((Cart {_user: self._user, _items: self._items, _marker: PhantomData}, s))

        }
    }

    pub fn cancel(self) -> Cart<NonEmpty> {
        Cart { _user: self._user, _items: self._items, _marker: PhantomData }
    }
}

