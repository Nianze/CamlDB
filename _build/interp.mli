open Ast
open Table
(* evaluates the query and returns the table corresponding to the *)
(* query result. *)
val eval: ?draw:bool -> expr -> table
(* saves all currently open tables *)
val save_open_tables: unit -> unit
