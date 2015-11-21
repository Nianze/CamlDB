type colname
(* A [node] is a node of a mutable doubly-linked list.
 * It stores a row in the table.
 * It contains a value of type [val] and optionally has
 * pointers to previous and/or next nodes. *)
type node = {
	mutable prev : node option;
	mutable next : node option;
	mutable value : t list
}

(* A [t] stores the type and values of each value *)
type t = 
	| Int of ref int 
	| String of ref string 
	| Float of ref float 
	| Bool of ref bool

type condition = colname * operator * t

type condition lst

(* Operators for comparing values *)
type operator = EQ | NE | GT | LT | GE | LE

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
	colnames: colname * t list;
	numcol : int;
	mutable numrow : int;
	mutable first : node option;
	mutable last : node option;
}

(* [status] is the status of operations, it includes all the erros *)
type status

(* [table status] is a tuple of table and status *)
type table status = table * status

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
val create_node: val -> node

(* [empty_table name colnames coltypes] is an empty table. *)
val empty_table: string -> colname * t list -> table

(* [insert r t] inserts a row [r] to the top of a 
 * table [t]. *)
val insert: node -> table -> table status

(* [delete r t] deletes a row [r] to from table [t]. *)
val delete: node -> table -> table status

(* [delete cond_list t] finds rows satisfies condions in 
 * [cond_list] in table [t]. *)
val find: condition lst -> table -> table status

(* [get_tablename t] gets table name of table [t] *)
val get_tablename: table -> string

(* [get_colnames t] gets a list of column name and type tuples 
of table [t] *)
val get_colnames: table -> colname * t list

(* [get_first t] gets the first node in table [t] *)
val get_first: table -> node option

(* [get_last t] gets the last node in table [t] *)
val get_last: table -> node option


