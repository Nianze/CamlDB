(******************* Basic data type *******************)
(* A [t] stores the type and values of each value *)
type t =
	| Int of int
	| String of string
	| Float of float
	| Bool of bool

(* [match_type t1 t2] returns true if the type of t1 t2 is the same
 * false otherwise
 *)
val match_type: t -> t -> bool

(* [type_string v] convert a [v] to a string of its type
 *)
val type_string: t -> string

type order = DESC | ASC
type top_t = TopNum of int | TopPercent of int


(******************* Status of operations *******************)

(* [status] is the status of operations, it includes all the erros *)

type status =
	| Success
	| DBError of string


(******************* Node *******************)
(* A [node] is a node of a mutable doubly-linked list.
 * It stores a row in the table.
 * It contains a value of type [val] and optionally has
 * pointers to previous and/or next nodes. *)
type node = {
	mutable prev : node option;
	mutable next : node option;
	mutable value : (t ref) list
}

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
val create_node: (t ref) list -> node


(******************* Compare *******************)
(* Operators for comparing values
EQ: =	Equal
NE: <>	Not equal
GT: >	Greater than
LT: <	Less than
GE: >=	Greater than or equal
LE: <=	Less than or equal
*)
type operator = EQ | NE | GT | LT | GE | LE

(* [compare_value v1 v2] compare two primitive types *)
val compare_value: operator -> 'a -> 'a -> bool

(* [compare_value t1 t2] compare two data of type [t] *)
val compare: operator -> t -> t -> bool * status


(******************* Table type *******************)
type colname = string

(* An [table] is a table.
 * [name] is the table name
 * [colnames] c a list of column names and column types tuple
 * [numcol] is the number of columns in the table
 * [numrow] is the number of rows in the table
 * The table content is stored in a doubly linked list
 * Each row is stored in a 'a node.
 *)
type table = {
	name: string;
	colnames: (colname * t) list;
	numcol : int;
	mutable numrow : int;
	mutable first : node option;
	mutable last : node option;
}

(******************* Condition *******************)
(* ex. ("column1", Eq, 3) *)
type condition = colname * operator * t

type cond_tree =
  | Cond of condition
  | And of cond_tree * cond_tree
  | Or of cond_tree * cond_tree


(******************* Table Accessor *******************)

(* [get_tablename t] gets table name of table [t] *)
val get_tablename: table -> string

(* [get_colnames t] gets a list of column name and type tuples
of table [t] *)
val get_colnames: table -> (colname * t) list

(* [col_in_table t col_name] checks if col [col_name] is in table [t]  *)
val col_in_table: table -> colname -> bool

(* [get_col_i t col_name] gets the index of col [col_name] *)
val get_col_i: table -> colname -> int

(* [get_col_i t col_name] gets the col [col_name] as a t list *)
val get_col: table -> colname -> t list

(* [get_first t] gets the first node in table [t] *)
val get_first: table -> node option

(* [get_last t] gets the last node in table [t] *)
val get_last: table -> node option


(******************* Table Helper *******************)

(* [empty_table name colnames coltypes] is an empty table. *)
val empty_table: string -> (colname * t) list -> table

(* [insert r t] inserts a row [r] to the top of a
 * table [t]. *)
val insert: node -> table -> status

(* [cond_row cond_list r] checks if row [n] satisfies condions in
 * [cond_list] and returns true or false and the status
 * requires: [n] to be non-empty
 *)
val cond_row: cond_tree -> (colname * t) list ->  node -> bool * status

(* [delete cond_list t] finds rows satisfies condions in
 * [cond_list] in table [t]. *)
val find: cond_tree -> table -> (node list) * status

(* [delete r t] deletes a row [r] to from table [t]. *)
val delete: node -> table -> status

(* [delete_list n_list t] deletes a list of rows [n_list] from table [t]. *)
val delete_list: node list -> table -> status

(* [delete_find cond_list t] delete all the rows that satisfy [cond_list]
 * in table [t]
 *)
val delete_find: cond_tree -> table -> status

(* [iter f t] iterate the table [t] and apply [f] on every row
 *)
val iter: (node -> unit) -> table -> unit

(* [fold_left f a t] iterate through table [t], apply [f] on every row
 * return: [a] accumulator
 *)
val fold_left: ('a -> node -> 'a) -> 'a -> table -> 'a

(* [table_to_list t] iterate the table [t] return a list of nodes
 * the most recent node is the first
 *)
val table_to_list: table -> node list

(* [list_to_table names colnames rows] converts a list to a table.
 * regard the first row in list as the most recent one
 * [name] - table name
 * [colnames] - colnames
 * [rows] - a list of node
 *)
val list_to_table: string -> (colname * t) list -> node list -> status * table


val in_some: 'a option -> 'a

(* [node_equal n1 n2] compare two nodes and return true if they are equal
 * structrually
 *)
val node_equal: node -> node -> bool

(* [node_list_equal node lst] compare the value of a node with a list and
 * return true if they are equal structrually
 *)
val node_list_equal: node -> t list -> bool

(* [table_equal t1 t2] returns true if each node in two table equals
 * structurally
 *)
val table_equal: table -> table -> bool