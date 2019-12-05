


variables p q : Prop 

example (h : p ∧ q) : q ∧ p :=
have hq: q, from h.right,
suffices hp: p, from ⟨hq, hp⟩,
show p, from h.left