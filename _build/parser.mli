
(* The type of tokens. *)

type token = 
  | WHERE
  | VALUES
  | UPDATE
  | UNION
  | TSTRING
  | TRUE
  | TOP
  | TINT
  | TBOOL
  | TABLE
  | STRING of (string)
  | SET
  | SEMICOLON
  | SEL
  | RPAREN
  | PERCENT
  | ORDER
  | OR
  | ON
  | NE
  | LT
  | LPAREN
  | LE
  | JOIN
  | INTO
  | INT of (int)
  | INSERT
  | ID of (string)
  | GT
  | GE
  | FROM
  | FALSE
  | EQ
  | EOF
  | DOT
  | DISTINCT
  | DESC
  | DELETE
  | CREATE
  | COMMA
  | BY
  | ASC
  | ANY
  | AND
  | ALL

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
