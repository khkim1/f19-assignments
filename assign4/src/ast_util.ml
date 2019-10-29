open Flags
open Core

exception Unimplemented

let fresh s = s ^ "'"

module Type = struct
  open Ast.Type

  let add_fresh (rename : t String.Map.t) (y : string) : (t String.Map.t) =
    String.Map.set rename ~key:y ~data:(Var(fresh y))

  let rec substitute_map (rename : t String.Map.t) (tau : t) : t =
    match tau with
    | Num -> Num
    | Bool -> Bool
    | Unit -> Unit

    | Var a -> (match String.Map.find rename a with
      | None -> tau
      | Some map_e -> map_e)
    | Fn {arg; ret} -> Fn {arg = substitute_map rename arg; ret = substitute_map rename ret}

    | Product {left; right} -> Product {left = substitute_map rename left;
                                        right = substitute_map rename right}
    | Sum {left; right} -> Sum {left = substitute_map rename left;
                                right = substitute_map rename right}

    | Forall {a; tau} -> Forall {a = (fresh a); tau = substitute_map (add_fresh rename a) tau}

    | Rec {a; tau} -> Rec {a = (fresh a); tau = substitute_map (add_fresh rename a) tau}

    | Exists {a; tau} -> Exists {a = (fresh a); tau = substitute_map (add_fresh rename a) tau}

    (* Add more cases here! *)
    | _ -> raise Unimplemented

  let substitute (x : string) (tau' : t) (tau : t) : t =
    substitute_map (String.Map.singleton x tau') tau

  let update_map (depth : int String.Map.t) (x : string) : (int String.Map.t) =
    String.Map.set (String.Map.map depth ~f:(fun (data:int) : int -> data + 1)) ~key:x ~data:0

  let rec to_debruijn (tau : t) : t =
    let rec aux (depth : int String.Map.t) (tau : t) : t =
      match tau with
      | Num -> Num
      | Bool -> Bool
      | Unit -> Unit
      | Var x -> (
        match String.Map.find depth x with
        | None -> tau
        | Some d -> Var(Int.to_string d)
        )
      | Fn {arg; ret}-> Fn {arg = aux depth arg; ret = aux depth ret}
      | Product {left; right} -> Product {left = aux depth left; right = aux depth right}
      | Sum {left; right} -> Sum {left = aux depth left; right = aux depth right}

      | Forall {a; tau} -> Forall {a = "_"; tau = aux (update_map depth a) tau}
      | Rec {a; tau} -> Rec {a = "_"; tau = aux (update_map depth a) tau}
      | Exists {a; tau} -> Exists {a = "_"; tau = aux (update_map depth a) tau}
      | _ -> raise Unimplemented
    in
    aux String.Map.empty tau

  let rec aequiv (tau1 : t) (tau2 : t) : bool =
    let rec aux (tau1 : t) (tau2 : t) : bool =
      match (tau1, tau2) with
      | (Num, Num) -> true
      | (Bool, Bool) | (Unit, Unit) -> true
      | (Var x, Var y) -> x = y
      | (Fn x, Fn y) -> aux x.arg y.arg && aux x.ret y.ret
      | (Sum x, Sum y) -> aux x.left y.left && aux x.right y.right
      | (Product x, Product y) -> aux x.left y.left && aux x.right y.right
      | (Rec x, Rec y) -> aux x.tau y.tau
      | (Forall x, Forall y) -> aux x.tau y.tau
      | (Exists x, Exists y) -> aux x.tau y.tau
      | _ -> false
    in
    aux (to_debruijn tau1) (to_debruijn tau2)

  let inline_tests () =
    let p = Parser.parse_type_exn in

(*     assert (
      aequiv
        (substitute "a" (p "num") (p "forall b . a"))
        (p "forall a . num"));
    assert (
      aequiv
        (substitute "a" (p "b") (p "forall b . a"))
        (p "forall c . b"));
    assert (
      not (aequiv
        (substitute "a" (p "b") (p "forall b . a"))
        (p "forall b . b")));
    assert (
      aequiv
        (substitute "a" (p "b") (p "forall b . forall b . a"))
        (p "forall q . forall c . b"));
    assert (
      not (aequiv
        (substitute "a" (p "b") (p "forall b . forall b . a"))
        (p "forall a . forall b . a"))); *)

    assert (aequiv (p "forall a . a") (p "forall b . b"));
    assert (not (aequiv (p "forall a . a") (p "forall b . num")));
    assert (aequiv
              (p "forall a . forall b . a -> b")
              (p "forall x . forall y . x -> y"))

  (* Uncomment the line below when you want to run the inline tests. *)
  let () = inline_tests ()
end

module Expr = struct
  open Ast.Expr

  let add_fresh (rename : t String.Map.t) (y : string) : (t String.Map.t) =
    String.Map.set rename ~key:y ~data:(Var(fresh y))

  let rec substitute_map (rename : t String.Map.t) (e : t) : t =
    match e with
    (* Numbers / Binop *)
    | Num _ -> e
    | Binop {binop; left; right} -> Binop {
      binop;
      left = substitute_map rename left;
      right = substitute_map rename right}


    (* ----------------------------------------------------------------- *)
    (* Bool / If / Relop / And / Or *)
    | True -> e
    | False -> e
    | If {cond; then_; else_} -> If {cond = substitute_map rename cond;
                                     then_ = substitute_map rename then_;
                                     else_ = substitute_map rename else_}
    | Relop {relop; left; right} -> Relop {relop = relop;
                                           left = substitute_map rename left;
                                           right = substitute_map rename right}
    | And {left; right} -> And {left = substitute_map rename left;
                                right = substitute_map rename right}
    | Or {left; right} -> Or {left = substitute_map rename left;
                              right = substitute_map rename right}


    (* ----------------------------------------------------------------- *)
    (* Var / Lam / App *)
    | Var x -> (match String.Map.find rename x with
                | None -> e
                | Some map_e -> map_e)
    | Lam {x; tau; e} -> Lam {x = (fresh x); tau = tau; e = substitute_map (add_fresh rename x) e}
    | App {lam; arg} -> App {lam = substitute_map rename lam; arg = substitute_map rename arg}


    (* ----------------------------------------------------------------- *)
    (* Product types *)
    | Unit -> e
    | Pair {left; right} -> Pair {left = substitute_map rename left;
                                  right = substitute_map rename right}
    | Project {e; d} -> Project {e = substitute_map rename e; d = d}


    (* ----------------------------------------------------------------- *)
    (* Sum types *)
    | Inject {e; d; tau} -> Inject {e = substitute_map rename e; d = d; tau = tau}
    | Case {e; xleft; eleft; xright; eright} -> Case {e = substitute_map rename e;
                                                      xleft = (fresh xleft);
                                                      eleft = substitute_map (add_fresh rename xleft) eleft;
                                                      xright = (fresh xright);
                                                      eright = substitute_map (add_fresh rename xright) eright}


    (* ----------------------------------------------------------------- *)
    (* Fixed Point *)
    | Fix {x; tau; e} -> Fix {x = (fresh x); tau = tau; e = substitute_map (add_fresh rename x) e}


    (* ----------------------------------------------------------------- *)
    (* Polymorphism *)
    | TyLam {a; e} -> TyLam {a = (fresh a); e = substitute_map (add_fresh rename a) e}
    | TyApp {e; tau} -> TyApp {e = substitute_map rename e; tau = tau}
(*     | TyLam {a; e} -> TyLam {a = a; e = substitute_map rename e}
    | TyApp {e; tau} -> TyApp {e = substitute_map rename e; tau = tau} *)


    (* ----------------------------------------------------------------- *)
    (* Recursive types *)
    | Fold_ {e; tau} -> Fold_ {e = substitute_map rename e; tau = tau}
    | Unfold e -> Unfold(substitute_map rename e)


    (* ----------------------------------------------------------------- *)
    (* Existential *)
    | Export {e; tau_adt; tau_mod} -> Export {e = substitute_map rename e; tau_adt = tau_adt; tau_mod = tau_mod}
    | Import {x; a; e_mod; e_body} ->
      Import {x = (fresh x);
              a = a;
              e_mod = substitute_map rename e_mod;
              e_body = substitute_map (add_fresh rename x) e_body}

(*     | Import {x; a; e_mod; e_body} ->
      Import {x = (fresh x);
              a = a;
              e_mod = substitute_map rename e_mod;
              e_body = substitute_map (add_fresh (add_fresh rename x) a) e_body} *)

    (* Put more cases here! *)
    | _ -> raise Unimplemented

  let substitute (x : string) (e' : t) (e : t) : t =
    substitute_map (String.Map.singleton x e') e

  let update_map (depth : int String.Map.t) (x : string) : (int String.Map.t) =
    String.Map.set (String.Map.map depth ~f:(fun (data:int) : int -> data + 1)) ~key:x ~data:0

  let rec to_debruijn (e : t) : t =
    let rec aux (depth : int String.Map.t) (e : t) : t =
      match e with
      | Num _ -> e
      | Binop {binop; left; right} -> Binop {binop; left = aux depth left; right = aux depth right}

      | True -> e
      | False -> e
      | If {cond; then_; else_} -> If {cond = aux depth cond;
                                       then_ = aux depth then_;
                                       else_ = aux depth else_}
      | Relop {relop; left; right} -> Relop {relop = relop;
                                             left = aux depth left;
                                             right = aux depth right}
      | And {left; right} -> And {left = aux depth left;
                                  right = aux depth right}
      | Or {left; right} -> Or {left = aux depth left;
                                right = aux depth right}

      | Var x -> (match String.Map.find depth x with
        | None -> e
        | Some d -> Var(Int.to_string d))
      | Lam {x; tau; e} -> Lam {x = "_"; tau = Ast.Type.Var "_"; e = aux (update_map depth x) e}
      | App {lam; arg} -> App {lam = aux depth lam; arg = aux depth arg}

      | Unit -> e
      | Pair {left; right} -> Pair {left = aux depth left; right = aux depth right}
      | Project {e; d} -> Project {e = aux depth e; d = d}
      | Inject {e; d; tau} -> Inject {e = aux depth e; d = d; tau = Ast.Type.Var "_"}
      | Case {e; xleft; eleft; xright; eright} -> Case {e = aux depth e;
                                                        xleft = "_";
                                                        eleft = aux (update_map depth xleft) eleft;
                                                        xright = "_";
                                                        eright = aux (update_map depth xright) eright}

      | Fix {x; tau; e} -> Fix {x = "_"; tau = Ast.Type.Var "_"; e = aux (update_map depth x) e}


      (* Not sure how to handle it for theseother types... *)
      (* | TyLam {a; e} -> TyLam {a = "_"; e = aux (update_map depth a) e} *)
      (* | TyApp {e; tau} -> TyApp {e = aux depth e; tau = Ast.Type.Var "_"} *)

      | TyLam {a; e} -> TyLam {a = "_"; e = aux depth e}
      | TyApp {e; tau} -> TyApp {e = aux depth e; tau = Ast.Type.Var "_"}


      | Fold_ {e; tau} -> Fold_ {e = aux depth e; tau = Ast.Type.Var "_"}
      | Unfold e -> Unfold(aux depth e)


      | Export {e; tau_adt; tau_mod} -> Export {e = aux depth e;
                                                tau_adt = Ast.Type.Var "_";
                                                tau_mod = Ast.Type.Var "_"}

      | Import {x; a; e_mod; e_body} -> Import {x = "_";
                                                a = "_";
                                                e_mod = aux depth e_mod;
                                                e_body = aux (update_map depth x) e_body}
(*
      | Import {x; a; e_mod; e_body} -> Import {x = "_";
                                                a = "_";
                                                e_mod = aux depth e_mod;
                                                e_body = aux (update_map depth x) e_body}
 *)

      | _ -> raise Unimplemented
    in
    aux String.Map.empty e

  let aequiv (e1 : t) (e2 : t) : bool =
    let rec aux (e1 : t) (e2 : t) : bool =
      match (e1, e2) with
      | (Num n1, Num n2) -> n1 = n2
      | (Var x, Var y) -> x = y
      | (Binop l, Binop r) ->
        l.binop = r.binop && aux l.left r.left && aux l.right r.right
      | (True, True) | (False, False) -> true
      | (If l, If r) ->
        aux l.cond r.cond && aux l.then_ r.then_ && aux l.else_ r.else_
      | (Relop l, Relop r) ->
        l.relop = r.relop && aux l.left r.left && aux l.right r.right
      | (And l, And r) ->
        aux l.left r.left && aux l.right r.right
      | (Or l, Or r) ->
        aux l.left r.left && aux l.right r.right
      | (Lam l, Lam r) ->
        aux l.e r.e
      | (App l, App r) ->
        aux l.lam r.lam && aux l.arg r.arg
      | (Unit, Unit) -> true
      | (Pair l, Pair r) ->
        aux l.left r.left && aux l.right r.right
      | (Project l, Project r) ->
        aux l.e r.e && l.d = r.d
      | (Inject l, Inject r) ->
        aux l.e r.e && l.d = r.d
      | (Case l, Case r) ->
        aux l.e r.e && aux l.eleft r.eleft && aux l.eright r.eright
      | (Fix l, Fix r) -> aux l.e r.e
      | (TyLam l, TyLam r) ->
        aux l.e r.e
      | (TyApp l, TyApp r) -> aux l.e r.e
      | (Fold_ l, Fold_ r) -> aux l.e r.e
      | (Unfold l, Unfold r) -> aux l r
      | (Export l, Export r) -> aux l.e r.e
      | (Import l, Import r) -> aux l.e_mod r.e_mod && aux l.e_body r.e_body
      | _ -> false
    in
    aux (to_debruijn e1) (to_debruijn e2)

  let inline_tests () =
    let p = Parser.parse_expr_exn in

    let t1 = p "(fun (x : num) -> x) y" in
    assert (aequiv (substitute "x" (Num 0) t1) t1);
    assert (aequiv (substitute "y" (Num 0) t1)
              (p "(fun (x : num) -> x) 0"));

    let t2 = p "x + (fun (x : num) -> y)" in
    assert (aequiv
              (substitute "x" (Num 0) t2)
              (p "0 + (fun (x : num) -> y)"));
    assert (aequiv (substitute "y" (Num 0) t2)
              (p "x + (fun (x : num) -> 0)"));

    assert (aequiv (p "fun (x : num) -> x") (p "fun (y : num) -> y"));

    assert (not (aequiv (p "fun (x : num) -> fun (x : num) -> x + x")
                   (p "fun (x : num) -> fun (y : num) -> y + x")));

    assert (
      aequiv
        (p "tyfun a -> fun (x : a) -> x")
        (p "tyfun b -> fun (x : b) -> x"));

    ()

  (* Uncomment the line below when you want to run the inline tests. *)
  let () = inline_tests ()
end
