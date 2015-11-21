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
	mutable value : ref t list
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

type status = 
	| Success
	| ConditionError of string
	| NumColError of string

type table status = table * status

(* [get_tablename t] gets table name of table [t] *)
let get_tablename (t:table) : string = t.name

(* [get_colnames t] gets a list of column name of table [t] *)
let get_colnames (t:table) : colname list = t.colnames

(* [get_coltypes t] gets a list of column types of table [t] *)
let get_coltypes (t:table) : t list = t.coltypes

(* [get_first t] gets the first node in table [t] *)
let get_first (t:table) : node option = t.first

(* [get_types t] gets the last node in table [t] *)
let get_last (t:table) : node option = t.last

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
let create_node v = {prev=None; next=None; value=v}

(* [empty_table name colnames coltypes] is an empty table. *)
let empty_table (name :string) (colnames: colname * t list):table = 
	{
		name = name;
		colnames = colnames;
		first = None;
		last = None
	}

(* [insert r t] inserts a row [r] to the top of a 
 * table [t]. *)
let insert (r: node) (t:table) : table status = 
	match t.first with
	| None -> t.first <- r; t.last <- r; (t, Success)
	| Some n -> t.first <- r; r.next <- Some n; n.prev <- Some r; (t, Success)

(* [delete r t] deletes a row [r] to from table [t]. *)
let delete (r: node) (t:table) :table = 
	(*let rec h r c =
		if c *)
	failwith "unimplemented"


let compare (op: operator) v1 v2 

(* [cond_row cond_list r] checks if row [n] satisfies condions in 
 * [cond_list] and returns true or false and the status *)
let rec cond_row (cond_list: condition lst) (colnames: colname list)
	(n: node) : bool * status =
	match cond_list with
	| [] -> (true, Success)
	| (col, op, v)::cond_list -> 
		if not List.mem col colnames then 
		ConditionError "Column name " ^ col ^ " is not found."
		else if List.(length colnames <> length n.value) then 
		NumColError "cond_row: Colnames, node value mismatch."
		else 
		let pair_list = List.combine colnames n.value in
		let content = List.assoc col pair_list in




(* [delete cond_list t] finds rows satisfies condions in 
 * [cond_list] in table [t]. *)
let find (cond_list: condition lst) (t: table): table =
	failwith "unimplemented"


