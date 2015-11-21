type colname
(* A [node] is a node of a mutable doubly-linked list.
 * It stores a row in the table.
 * It contains a value of type [val] and optionally has
 * pointers to previous and/or next nodes. *)
type node
(* A [t] stores the type and values of each value *)
type t = 
	| Int of int 
	| String of string 
	| Float of float 
	| Bool of bool

type condition = colname * operator * t

type condition lst
(* Operators for comparing values *)
type operator = EQ | NE | GT | LT | GE | LE
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

(* [get_tablename t] gets table name of table [t] *)
val get_tablename: table -> string

(* [get_colnames t] gets a list of column name of table [t] *)
val get_colnames: table -> colname list

(* [get_coltypes t] gets a list of column types of table [t] *)
val get_coltypes: table -> t list 

(* [get_first t] gets the first node in table [t] *)
val get_first: table -> node option

(* [get_last t] gets the last node in table [t] *)
val get_last: table -> node option


