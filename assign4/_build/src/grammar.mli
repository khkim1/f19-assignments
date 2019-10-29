
(* The type of tokens. *)

type token = 
  | WITHOUT
  | VAR of (string)
  | UNFOLD
  | TY_UNIT
  | TY_NUM
  | TY_BOOL
  | TRUE
  | THEN
  | TFN
  | SUB
  | RPAREN
  | RIGHT
  | REC
  | RBRACKET
  | RBRACE
  | PLUS
  | OR
  | NUM of (int)
  | MUL
  | LT
  | LPAREN
  | LETREC
  | LET
  | LEFT
  | LBRACKET
  | LBRACE
  | INJECT
  | IN
  | IMPORT
  | IF
  | GT
  | FORALL
  | FOLD
  | FN
  | FALSE
  | EXPORT
  | EXISTS
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | COLON
  | CASE
  | BAR
  | AS
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val type_toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.Type.t)

val expr_toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.Expr.t)
