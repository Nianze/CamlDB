type colname
(* A [node] is a node of a mutable doubly-linked list.
 * It stores a row in the table.
 * It contains a value of type [val] and optionally has
 * pointers to previous and/or next nodes. *)
type node
(* A [t] stores the type and values of each value *)
type t

type condition = colname * operator * t

type condition lst
(* Operators for comparing values *)
type operator
(* An [table] is a table.
 * [name] is the table name
 * [colnames] c a list of column names
 * [coltypes] is a list of column types and the default values
 * The table content is stored in a doubly linked list
 * Each row is stored in a 'a node.
 *)
type table

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
val create_node: val -> node

(* [empty_table name colnames coltypes] is an empty table. *)
val empty_table: string -> colname list -> t list -> table

(* [insert r t] inserts a row [r] to the top of a 
 * table [t]. *)
val insert: node -> table -> table

(* [delete r t] deletes a row [r] to from table [t]. *)
val delete: node -> table -> table

(* [delete cond_list t] finds rows satisfies condions in 
 * [cond_list] in table [t]. *)
val find: condition lst -> table -> table