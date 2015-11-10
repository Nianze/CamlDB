(** store column names, table names *)
type var = string

(** the binary operators *)
type operator =
  | Gt | Lt | Eq | GtEq | LtEq | NotEq  (** >, <, =, >=, <=, <> *)

(** (OCaml) values of type expr represent SQL expressions.
    Here are some examples of how expressions are represented:
     - Unit represents ()
     - Int 7 represents 7
     - Bool true represents true
     - String "hello" represents "hello"
     - Binop (Plus, e1, e2) represents e1 + e2
     - If (e1,e2,3) represents if e1 then e2 else e3
     - Var "x" represents the variable x
     - Let ("x",e1,e2) represents let x = e1 in e2
     - LetRec ("x",e1,e2) represents let rec x = e1 in e2
     - App (e1,e2) represents (e1 e2)
     - Fun (x,e) represents (fun x -> e)
     - Pair (e1,e2) represents (e1,e2)
     - Variant ("Cons", e) represents Cons e
       (where Cons could be any constructor name
     - Match (e, [(p1,e1);(p2,e2);...]) represents
       match e with | p1 -> e1 | p2 -> e1 | ...
     *)
type expr =
  | Int      of int
  | Bool     of bool
  | String   of string
  | BinOp    of operator * expr * expr
  | Var      of var
