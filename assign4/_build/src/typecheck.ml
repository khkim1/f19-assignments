open Flags
open Core
open Result.Monad_infix
open Ast

exception Unimplemented

let add_var_ctx (ctx : Type.t String.Map.t) (x : string) (tau : Type.t) : (Type.t String.Map.t) =
  String.Map.set ctx ~key:x ~data:tau


let rec typecheck_expr (ctx : Type.t String.Map.t) (e : Expr.t)
  : (Type.t, string) Result.t =
  match e with
  (* Numbers / Binop *)
  | Expr.Num _ -> Ok Type.Num
  | Expr.Binop {left; right; _} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Num, Type.Num) -> Ok(Type.Num)
     | _ -> Error (
       Printf.sprintf
         "Binary operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))


  (* ----------------------------------------------------------------- *)
  (* Bool / If / Relop / And / Or *)
  | Expr.True -> Ok(Type.Bool)
  | Expr.False -> Ok(Type.Bool)
  | Expr.If {cond; then_; else_} ->
    typecheck_expr ctx cond >>= fun tau_cond ->
    typecheck_expr ctx then_ >>= fun tau_then ->
    typecheck_expr ctx else_ >>= fun tau_else -> (
    if (Ast_util.Type.aequiv tau_cond Type.Bool) && (Ast_util.Type.aequiv tau_then tau_else) then
      Ok(tau_then)
    else
      Error(Printf.sprintf "If statement incompatible types: cond (%s : %s), then (%s : %s), else (%s : %s)"
      (Expr.to_string cond) (Type.to_string tau_cond)
      (Expr.to_string then_) (Type.to_string tau_then)
      (Expr.to_string else_) (Type.to_string tau_else)))

  | Expr.Relop {left; right; _} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Num, Type.Num) -> Ok(Type.Bool)
     | _ -> Error(
       Printf.sprintf
         "Relational operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))

  | Expr.And {left; right} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Bool, Type.Bool) -> Ok(Type.Bool)
     | _ -> Error(
       Printf.sprintf
         "And operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))

  | Expr.Or {left; right} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Bool, Type.Bool) -> Ok(Type.Bool)
     | _ -> Error(
       Printf.sprintf
         "Or operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))


  (* ----------------------------------------------------------------- *)
  (* Var / Lam / App *)
  | Expr.Var x -> (match String.Map.find ctx x with
    | None -> Error(Printf.sprintf "var %s type not in ctx" (Expr.to_string e))
    | Some tau -> Ok(tau))

  | Expr.Lam {x; tau; e} ->
    typecheck_expr (add_var_ctx ctx x tau) e >>= fun tau_e -> Ok(Type.Fn {arg = tau; ret = tau_e})

  | Expr.App {lam; arg} ->
    typecheck_expr ctx lam >>= fun tau_lam ->
    typecheck_expr ctx arg >>= fun tau_arg ->
    (match tau_lam with
     | Type.Fn {arg=arg_in; ret} -> (if (Ast_util.Type.aequiv arg_in tau_arg) then Ok(ret)
                              else Error(Printf.sprintf "Fn-App type mismatch -> lam: (%s, %s), arg: (%s, %s)"
                                         (Expr.to_string lam) (Type.to_string tau_lam)
                                         (Expr.to_string arg) (Type.to_string tau_arg)))
     | _ -> Error(Printf.sprintf "Function body type wrong : %s" (Type.to_string tau_lam))
    )


  (* ----------------------------------------------------------------- *)
  (* Product types *)
  | Expr.Unit -> Ok(Type.Unit)
  | Expr.Pair {left; right} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    Ok(Type.Product {left = tau_left; right = tau_right})


  | Expr.Project {e; d} ->
    typecheck_expr ctx e >>= fun tau_prod ->
    (match tau_prod with
     | Type.Product {left; right} -> (
       if d = Expr.Left then Ok(left)
       else if d = Expr.Right then Ok(right)
       else Error( Printf.sprintf "Unknown projection direction"))
     | _ -> Error( Printf.sprintf "Projecting a non-pair type : %s" (Type.to_string tau_prod)  )
    )

  (* ----------------------------------------------------------------- *)
  (* Sum types *)
  | Expr.Inject {e; d; tau} ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau with
     | Type.Sum {left; right} ->
       (match d with
       | Expr.Left -> (
         if (Ast_util.Type.aequiv left tau_e) then Ok(tau)
         else Error(Printf.sprintf "Injection incompatible types"))
       | Expr.Right -> (
         if (Ast_util.Type.aequiv right tau_e) then Ok(tau)
         else Error(Printf.sprintf "Injection incompatible types"))
       | _ -> Error( Printf.sprintf "Unknown projection direction"))
     | _ -> Error( Printf.sprintf "Injection not a sum type" ))

  | Expr.Case {e; xleft; eleft; xright; eright} ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau_e with
    | Type.Sum {left; right} ->
      typecheck_expr (add_var_ctx ctx xleft left) eleft >>= fun tau_eleft ->
      typecheck_expr (add_var_ctx ctx xright right) eright >>= fun tau_eright ->
      if Ast_util.Type.aequiv tau_eleft tau_eright then Ok(tau_eleft)
      else Error( Printf.sprintf "Case terms don't have same type" )
    | _ -> Error( Printf.sprintf "Case expression not a sum type" ))


  (* ----------------------------------------------------------------- *)
  (* Fix *)
  | Expr.Fix {x; tau; e} ->
    typecheck_expr (add_var_ctx ctx x tau) e >>= fun tau_e ->
    (if Ast_util.Type.aequiv tau_e tau then Ok(tau_e)
     else Error( Printf.sprintf "Fix arg and body type mismatch" ))


  (* ----------------------------------------------------------------- *)
  (* Polymorphism *)
  | Expr.TyLam {a; e} ->
    typecheck_expr ctx e >>= fun tau_e -> Ok(Type.Forall {a = a; tau=tau_e})

  | Expr.TyApp {e; tau=tau_arg} ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau_e with
     | Type.Forall {a; tau=tau_body} -> Ok(Ast_util.Type.substitute a tau_arg tau_body)
     | _ -> Error( Printf.sprintf "TyApp exp does not have polymorphic type" ))


  (* ----------------------------------------------------------------- *)
  (* Recursive types *)
  | Expr.Fold_ {e; tau} ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau with
     | Type.Rec {a; tau=tau_in} -> (
       if Ast_util.Type.aequiv tau_e (Ast_util.Type.substitute a tau tau_in) then Ok(tau)
       else Error( Printf.sprintf "T-Fold type and body type do not match" ))
     | _ -> Error( Printf.sprintf "T-Fold without recursive type" )
    )

  | Expr.Unfold e ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau_e with
     | Type.Rec {a; tau=tau_in} -> Ok(Ast_util.Type.substitute a tau_e tau_in)
     | _ -> Error( Printf.sprintf "Trying to unfold non-recursive type" ))


  (* ----------------------------------------------------------------- *)
  (* Existential *)
  | Expr.Export {e; tau_adt; tau_mod} ->
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau_mod with
     | Exists {a; tau=tau_in} -> (
       if Ast_util.Type.aequiv tau_e (Ast_util.Type.substitute a tau_adt tau_in) then Ok(tau_mod)
       else Error( Printf.sprintf "Export inner type doesn't match" ))
     | _ -> Error ( Printf.sprintf "Non-existential type in tau_mod" )
    )

  | Expr.Import {x; a = beta; e_mod; e_body} ->
    typecheck_expr ctx e_mod >>= fun tau_e_mod ->
    (match tau_e_mod with
     | Exists {a; tau=tau_mod} -> (
       typecheck_expr (add_var_ctx ctx x (Ast_util.Type.substitute a (Type.Var beta) tau_mod)) e_body >>= fun tau_e_body ->
       Ok(tau_e_body)
      )
     | _ -> Error ( Printf.sprintf "e_mod does not have existential type in Import" )
    )


  (* Add more cases here! *)
  | _ -> raise Unimplemented

let typecheck t = typecheck_expr String.Map.empty t

let inline_tests () =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  let e1 = p_ex "fun (x : num) -> x" in
  assert (typecheck e1 = Ok(p_ty "num -> num"));

  let e2 = p_ex "fun (x : num) -> y" in
  assert (Result.is_error (typecheck e2));

  let t3 = p_ex "(fun (x : num) -> x) 3"in
  assert (typecheck t3 = Ok(p_ty "num"));

  let t4 = p_ex "((fun (x : num) -> x) 3) 3" in
  assert (Result.is_error (typecheck t4));

  let t5 = p_ex "0 + (fun (x : num) -> x)" in
  assert (Result.is_error (typecheck t5))

(* let inline_tests () =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  let e1 = p_ex "1" in
  assert (typecheck e1 = Ok(p_ty "num"));

  let e2 = p_ex "true" in
  assert (typecheck e2 = Ok(p_ty "bool"));

  let e3 = p_ex "if true then 1 else 0" in
  assert (typecheck e3 = Ok(p_ty "num"));

  let e4 = p_ex "true || false" in
  assert (typecheck e4 = Ok(p_ty "bool"));

  let e5 = p_ex "false && (true || false)" in
  assert (typecheck e5 = Ok(p_ty "bool")) *)


(* Uncomment the line below when you want to run the inline tests. *)
let () = inline_tests ()
