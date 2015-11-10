type colname = string 

(* Operators for comparing values
EQ: =	Equal
NE: <>	Not equal
GT: >	Greater than
LT: <	Less than
GE: >=	Greater than or equal
LE: <=	Less than or equal
*)
type operator = EQ | NE | GT | LT | GE | LE

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
	| Int of int 
	| String of string 
	| Float of float 
	| Bool of bool

type condition = colname * operator * t

type condition lst
(* An [table] is a table.
 * [name] is the table name
 * [colnames] is a list of column names
 * [coltypes] is a list of column types and the default values
 * The table content is stored in a doubly linked list
 * Each row is stored in a 'a node.
 *)
type table = {
	name: string;
	colnames: colname list;
	coltypes: t list;

	mutable first : node option;
	mutable last : node option;
}

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
let create_node v = {prev=None; next=None; value=v}

(* [empty_table name colnames coltypes] is an empty table. *)
let empty_table (name :string) (colnames: colname list)
	(coltypes: t list) :table = 
	{
		name = name;
		colnames = colnames;
		coltypes = coltypes;
		first = None;
		last = None
	}

(* [insert r t] inserts a row [r] to the top of a 
 * table [t]. *)
let insert (r: node) (t:table) :table = 
	failwith "unimplemented"

(* [delete r t] deletes a row [r] to from table [t]. *)
let delete (r: node) (t:table) :table = 
	failwith "unimplemented"

(* [delete cond_list t] finds rows satisfies condions in 
 * [cond_list] in table [t]. *)
let find (cond_list: condition lst) (t: table): table =
	failwith "unimplemented"

