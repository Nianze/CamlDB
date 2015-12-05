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
  | TFLOAT
  | TBOOL
  | TABLE
  | STRING of (string)
  | SET
  | SEMICOLON
  | SEL
  | SCATT
  | RPAREN
  | PLOT
  | PERCENT
  | ORDER
  | OR
  | ON
  | NE
  | LT
  | LPAREN
  | LINE
  | LE
  | JOIN
  | INTO
  | INT of (int)
  | INSERT
  | ID of (string)
  | HISTOG
  | GT
  | GE
  | FROM
  | FLOAT of (float)
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
  | BAR
  | ASC
  | ANY
  | AND
  | ALL

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

