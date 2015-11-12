(** This function is used for converting SQL strings to ASTs
    We use menhir to parse the sql query
*)
(* The type of tokens. *)
type token =
  | FALSE
  | TRUE
  | RPAREN
  | LPAREN
  | GT (*>*)
  | LT (*<*)
  | EQ (*=*)
  | NE (*<>*)
  | GE (*>=*)
  | LE (*>=*)
  | SELECT
  | FROM
  | WHERE
  | UPDATE
  | DELETE
  | INSERT
  | VALUE
  | CREATEDB
  | ALTERDB
  | CREATETB
  | ALTERTB
  | DROPTB
  | CREATEID
  | DROPID
  | INT of (int)
  | FLOAT of (float)
  | PERCENT of (float)
  | STRING of (string)
  | ORDER of (float)
  | TBNAME of (string)
  | COLNAME of (string)
  | EOF

(* This exception is raised by the monolithic API functions. *)
exception Error

(* PThe monolithic API. *)
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
