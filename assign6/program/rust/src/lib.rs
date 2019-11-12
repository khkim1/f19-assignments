use std::{mem, fmt};
use std::fmt::{Display, Debug};

#[derive(PartialEq, Eq, Clone)]
pub enum BinaryTree<T> {
  Leaf,
  Node(T, Box<BinaryTree<T>>, Box<BinaryTree<T>>)
}

impl<T: Debug + Display + PartialOrd> BinaryTree<T> {
  pub fn len(&self) -> usize {
    match self {
      BinaryTree::Leaf => {0},
      BinaryTree::Node(t, l, r) => {1 + l.len() + r.len()}
    }
  }

  pub fn to_vec(&self) -> Vec<&T> {
    match self {
      BinaryTree::Leaf => vec![],
      BinaryTree::Node(t, l, r) => {
        let mut v: Vec<&T> = vec![];
        v.append(&mut l.to_vec());
        v.append(&mut vec![t]);
        v.append(&mut r.to_vec());
        v
      }
    }
  }

  pub fn sorted(&self) -> bool {
    match self {
      BinaryTree::Leaf => {true},
      BinaryTree::Node(t, l, r) => {
        (match r as &BinaryTree<T> {BinaryTree::Leaf => true,
                                    BinaryTree::Node(tr, _, _) => (*t <= *tr)})
        &&
        (match l as &BinaryTree<T> {BinaryTree::Leaf => true,
                                    BinaryTree::Node(tl, _, _) => (*t >= *tl)})
        &&
        r.sorted()
        &&
        l.sorted()
      }
    }
  }

  pub fn insert(&mut self, t: T) {
    match self {
      BinaryTree::Leaf => {*self = BinaryTree::Node(t, Box::new(BinaryTree::Leaf), Box::new(BinaryTree::Leaf));}
      BinaryTree::Node(t_cur, l, r) => {
        if *t_cur >= t {l.insert(t);}
        else {r.insert(t);}
      }
    }
  }

  pub fn search(&self, query: &T) -> Option<&T> {
    match self {
      BinaryTree::Leaf => {None}
      BinaryTree::Node(t, l, r) => {
        if *query == *t {Some(t)}
        else if *query > *t {r.search(query)}
        else {
          match l as &BinaryTree<T> {BinaryTree::Leaf => {Some(t)}
                                     BinaryTree::Node(tl, l_sub, _) => {
                                       if *query <= *tl {l_sub.search(query)}
                                       else {Some(t)}}}
        }
      }
    }
  }

  // pub fn remove_largest(&mut self) -> Option<&T> {
  //   match self {
  //     BinaryTree::Leaf => {None}
  //     BinaryTree::Node(t, l, r) => {
  //       match r as &BinaryTree<T> {
  //         BinaryTree::Leaf => {match l as &BinaryTree<T> {
  //                                BinaryTree::Leaf => {*self = BinaryTree::Leaf;}
  //                                BinaryTree::Node(t_sub, l_sub, r_sub) => {
  //                                  mem::replace(l, *l_sub);
  //                                  mem::replace(r, *r_sub);
  //                                  mem::replace(t, *t_sub);
  //                                }}
  //                                Some(t)
  //                              }
  //         BinaryTree::Node(t_sub, l_sub, r_sub) => {r_sub.remove_largest()}
  //       }
  //     }
  //   }
  // }

  fn find_greatest(&mut self) -> BinaryTree<T> {
    match self {
      BinaryTree::Leaf => { *self },
      BinaryTree::Node(_, _, r) => {
        // if the right child is a leaf, return node
        if r.len() == 0 { *self }
        // otherwise, recurse on the right child
        else { BinaryTree::find_greatest(r) }
      }
    }
  }

  pub fn find_smallest(&mut self) -> BinaryTree<T> {
    match self {
      BinaryTree::Leaf => { *self },
      BinaryTree::Node(_, l, _) => {
        // if the right child is a leaf, return node
        if l.len() == 0 { *self }
        // otherwise, recurse on the right child
        else { BinaryTree::find_smallest(l) }
      }
    }
  }

  pub fn rebalance(&mut self) {
    match self {
      BinaryTree::Leaf => {}
      BinaryTree::Node(t, l, r) => {
        // Left subtree is larger
        if l.len() - r.len() >= 2 {
          // remove largest node in tree
          let new_root = l.find_greatest();
          let nr_ref = &mut new_root;

          let nr_val = match nr_ref {
            BinaryTree::Node(t, l, r) => {&mut t}
          };
          // *self = BinaryTree::Node(new_root, l, self)
        }
        // Right subtree is larger
        else if r.len() - l.len() >= 2 {
          let new_root = r.find_smallest();
        }
        // Do nothing if sub-tree sizes are similar
        else {}
      }
    }
  }


  // Adapted from https://github.com/bpressure/ascii_tree
  fn fmt_levels(&self, f: &mut fmt::Formatter<'_>, level: Vec<usize>) -> fmt::Result {
    use BinaryTree::*;
    const EMPTY: &str = "   ";
    const EDGE: &str = " └─";
    const PIPE: &str = " │ ";
    const BRANCH: &str = " ├─";

    let maxpos = level.len();
    let mut second_line = String::new();
    for (pos, l) in level.iter().enumerate() {
      let last_row = pos == maxpos - 1;
      if *l == 1 {
        if !last_row { write!(f, "{}", EMPTY)? } else { write!(f, "{}", EDGE)? }
        second_line.push_str(EMPTY);
      } else {
        if !last_row { write!(f, "{}", PIPE)? } else { write!(f, "{}", BRANCH)? }
        second_line.push_str(PIPE);
      }
    }

    match self {
      Node(s, l, r) => {
        let mut d = 2;
        write!(f, " {}\n", s)?;
        for t in &[l, r] {
          let mut lnext = level.clone();
          lnext.push(d);
          d -= 1;
          t.fmt_levels(f, lnext)?;
        }
      }
      Leaf => {write!(f, "\n")?}
    }
    Ok(())
  }
}

impl<T: Debug + Display + PartialOrd> fmt::Debug for BinaryTree<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_levels(f, vec![])
  }
}

#[cfg(test)]
mod test {
  use lazy_static::lazy_static;
  use super::BinaryTree::*;
  use crate::BinaryTree;

  lazy_static! {
    static ref TEST_TREE: BinaryTree<&'static str> = {
      Node(
        "B",
        Box::new(Node("A", Box::new(Leaf), Box::new(Leaf))),
        Box::new(Node("C", Box::new(Leaf), Box::new(Leaf))))
    };
  }

  #[test]
  fn len_test() {
    assert_eq!(TEST_TREE.len(), 3);
  }

  #[test]
  fn to_vec_test() {
    assert_eq!(TEST_TREE.to_vec(), vec![&"A", &"B", &"C"]);
  }

  #[test]
  fn sorted_test() {
    let mut t = TEST_TREE.clone();
    assert!(t.sorted());

    t = Node("D", Box::new(Leaf), Box::new(t));
    assert!(!t.sorted());
  }

  #[test]
  fn insertion_test() {
    let mut t = TEST_TREE.clone();
    t.insert("E");
    assert!(t.sorted());
  }

  #[test]
  fn search_test() {
    let mut t= TEST_TREE.clone();
    t.insert("E");
    assert!(t.search(&"D") == Some(&"E"));
    assert!(t.search(&"C") == Some(&"C"));
    assert!(t.search(&"F") == None);
  }

  #[test]
  fn rebalance1_test() {
    let mut t = Node(
      "D",
      Box::new(Node(
        "B",
        Box::new(Node(
          "A", Box::new(Leaf), Box::new(Leaf))),
        Box::new(Node(
          "C", Box::new(Leaf), Box::new(Leaf))))),
      Box::new(Node(
        "E", Box::new(Leaf), Box::new(Leaf))));

    let t2 = Node(
      "C",
      Box::new(Node(
        "B",
        Box::new(Node(
          "A", Box::new(Leaf), Box::new(Leaf))),
        Box::new(Leaf))),
      Box::new(Node(
        "D",
        Box::new(Leaf),
        Box::new(Node(
          "E", Box::new(Leaf), Box::new(Leaf)))
      )));

    t.rebalance();
    assert_eq!(t, t2);
  }

  #[test]
  fn rebalance2_test() {
    let mut t = Node(
      "A",
      Box::new(Leaf),
      Box::new(Node(
        "B",
        Box::new(Leaf),
        Box::new(Node(
          "C",
          Box::new(Leaf),
          Box::new(Node(
            "D",
            Box::new(Leaf),
            Box::new(Leaf))))))));

    let t2 = Node(
      "B",
      Box::new(Node("A", Box::new(Leaf), Box::new(Leaf))),
        Box::new(Node(
          "C",
          Box::new(Leaf),
          Box::new(Node(
            "D",
            Box::new(Leaf),
            Box::new(Leaf))))));

    t.rebalance();
    assert_eq!(t, t2);
  }

  #[test]
  fn rebalance3_test() {
    let mut t = Node(
      "E",
      Box::new(Node(
        "B",
        Box::new(Leaf),
        Box::new(Node(
          "D",
          Box::new(Node(
            "C", Box::new(Leaf), Box::new(Leaf))),
          Box::new(Leaf))))),
      Box::new(Node(
        "F", Box::new(Leaf), Box::new(Leaf))));

    let t2 = Node(
      "D",
      Box::new(Node(
        "B",
        Box::new(Leaf),
        Box::new(Node("C", Box::new(Leaf), Box::new(Leaf))))),
      Box::new(Node(
        "E",
        Box::new(Leaf),
        Box::new(Node("F", Box::new(Leaf), Box::new(Leaf))))));

    t.rebalance();
    assert_eq!(t, t2);
  }
}
