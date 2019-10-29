open Flags
open Core
open Ast

type outcome =
  | Step of Expr.t
  | Val

exception RuntimeError of string

let rec trystep (e : Expr.t) : outcome =
  match e with
  | (Expr.Lam _ | Expr.Num _ | Expr.True | Expr.False | Expr.Pair _ | Expr.Unit
    | Expr.Inject _ | Expr.TyLam _ | Expr.Export _ | Expr.Fold_ _) -> Val

  (* Numbers / Binop *)
  | Expr.Binop {binop; left; right} ->
    (left, fun left' -> Expr.Binop {left = left'; binop; right;}) |-> fun () ->
    (right, (fun right' -> Expr.Binop {right = right'; binop; left})) |-> fun () ->
    let (Expr.Num n1, Expr.Num n2) = (left, right) in
    let f = match binop with
      | Expr.Add -> (+)
      | Expr.Sub -> (-)
      | Expr.Mul -> ( * )
      | Expr.Div -> (/)
    in
    Step (Expr.Num (f n1 n2) )


  (* ----------------------------------------------------------------- *)
  (* Bool / If / Relop / And / Or *)
  | Expr.If {cond; then_; else_} ->
    (cond, fun cond' -> Expr.If {cond = cond'; then_; else_}) |-> fun () ->
    (match cond with
    | Expr.True -> Step(then_)
    | Expr.False -> Step(else_))

  | Expr.Relop {relop; left; right} ->
    (left, fun left' -> Expr.Relop {left = left'; relop; right}) |-> fun () ->
    (right, (fun right' -> Expr.Relop {right = right'; relop; left})) |-> fun () ->
    let (Expr.Num n1, Expr.Num n2) = (left, right) in
    (let f = match relop with
      | Expr.Lt -> (<)
      | Expr.Gt -> (>)
      | Expr.Eq -> ( = )
    in
    Step (match (f n1 n2) with | true -> Expr.True | false -> Expr.False))

  | Expr.And {left; right} ->
    (left, fun left' -> Expr.And {left = left'; right}) |-> fun () ->
    (right, (fun right' -> Expr.And {right = right'; left})) |-> fun () ->
    (match (left, right) with
    | (Expr.False, _) -> Step(Expr.False)
    | (_, Expr.False) -> Step(Expr.False)
    | (Expr.True, Expr.True) -> Step(Expr.True))

  | Expr.Or {left; right} ->
    (left, fun left' -> Expr.Or {left = left'; right}) |-> fun () ->
    (right, (fun right' -> Expr.Or {right = right'; left})) |-> fun () ->
    (match (left, right) with
    | (Expr.True, _) -> Step(Expr.True)
    | (_, Expr.True) -> Step(Expr.True)
    | (Expr.False, Expr.False) -> Step(Expr.False))


  (* ----------------------------------------------------------------- *)
  (* Var / Lam / App *)
  | Expr.App {lam; arg} ->
    (lam, fun lam' -> Expr.App {lam = lam'; arg}) |-> fun () ->
    (match lam with
     | Lam {x; tau; e} -> Step(Ast_util.Expr.substitute x arg e))


  (* ----------------------------------------------------------------- *)
  (* Product types *)
  | Expr.Project {e; d} ->
    (e, fun e' -> Expr.Project {e = e'; d}) |-> fun () ->
    (match e with
    | Expr.Pair {left; right} -> if d = Expr.Left then Step(left) else Step(right))


  (* ----------------------------------------------------------------- *)
  (* Sum types *)
  | Expr.Case {e; xleft; eleft; xright; eright} ->
    (e, fun e' -> Expr.Case {e = e'; xleft; eleft; xright; eright}) |-> fun () ->
    (match e with
    | Expr.Inject {e; d; tau} -> (
      if d = Expr.Left then Step(Ast_util.Expr.substitute xleft e eleft)
      else Step(Ast_util.Expr.substitute xright e eright)))


  (* ----------------------------------------------------------------- *)
  (* Fix *)
  | Expr.Fix {x; tau; e=in_e} -> Step(Ast_util.Expr.substitute x e in_e)


  (* ----------------------------------------------------------------- *)
  (* Polymorphism *)
  | Expr.TyApp {e; tau} ->
    (e, fun e' -> Expr.TyApp {e = e'; tau = tau}) |-> fun () ->
    (match e with | Expr.TyLam {a; e=e_body} -> Step(e_body))


  (* ----------------------------------------------------------------- *)
  (* Recursive types *)
  | Expr.Unfold e ->
    (e, fun e' -> Expr.Unfold(e')) |-> fun () ->
    (match e with | Expr.Fold_ {e; tau} -> Step(e))


  (* ----------------------------------------------------------------- *)
  (* Existential *)
  | Expr.Import {x; a; e_mod; e_body} ->
    (e_mod, fun e_mod' -> Expr.Import {x; a; e_mod = e_mod'; e_body}) |-> fun () ->
    (match e_mod with
     | Expr.Export {e; tau_adt; tau_mod} -> Step(Ast_util.Expr.substitute x e e_body))


  (* Add more cases here! *)
  | _ -> raise (RuntimeError (
    Printf.sprintf "Reached a stuck state at expression: %s" (Expr.to_string e)))

and (|->) ((e, hole) : Expr.t * (Expr.t -> Expr.t)) (next : unit -> outcome)
  : outcome =
  match trystep e with Step e' -> Step (hole e') | Val -> next ()

let rec eval e =
  match trystep e with
  | Step e' ->
    (if extra_verbose () then
       Printf.printf "Stepped:\n%s\n|->\n%s\n\n"
         (Expr.to_string e) (Expr.to_string e'));
    eval e'
  | Val -> Ok e

let inline_tests () =
  let p = Parser.parse_expr_exn in
  let e1 = p "2 + 3" in
  assert (trystep e1 = Step(Expr.Num 5));

  let e2 = p "(fun (x : num) -> x) 3" in
  assert (trystep e2 = Step(Expr.Num 3))

(* Uncomment the line below when you want to run the inline tests. *)
let () = inline_tests ()
