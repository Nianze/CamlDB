open Ast
open Table
(* evaluates the query and returns the table corresponding to the *)
(* query result. *)
val eval: expr -> table
val save_open_tables: unit -> unit
