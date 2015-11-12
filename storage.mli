open Table

(* save_table [t] [filename] saves the table [t] in a file with *)
(* filename [filename]. *)
val save_table: table -> string -> unit

(* load_table [filename] loads the table with filename [filename]. *)
val load_table: string -> table

