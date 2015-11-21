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
	| DBError of string


type table status = table * status

(* [get_tablename t] gets table name of table [t] *)
let get_tablename (t:table) : string = t.name

(* [get_colnames t] gets a list of column name of table [t] *)
let get_colnames (t:table) : colname * t list = t.colnames

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
 * table [t]. 
 * require: nothing about the default prev, next of [r]
 *)
let insert (r: node) (t:table) : status = 
	match t.first with
	| None -> 
		r.prev <- None;
		r.next <- None;
		t.first <- r; 
		t.last <- r; 
		t.numrow <- (t.numrow + 1); 
		Success
	| Some n -> 
		r.prev <- None;
		r.next <- Some n; 
		t.first <- r; 
		n.prev <- Some r; 
		t.numrow <- (t.numrow + 1); 
		Success

	

let match_type (t1:t) (t2:t) : bool = 
	match (t1, t2) with
	| (Int _, Int _) | (Float _, Float _) | (String _, String _)
	| (Bool _, Bool _) -> true
	| _ -> false

let type_string (v:t) : string =
	match v with
	| Int _ -> "int"
	| String _ -> "string"
	| Bool _ -> "bool"
	| Float _ -> "float"

let compare_value (op: operator) (v1: 'a) (v2: 'a) : bool = 
	match op with 
	| EQ -> v1 = v2
	| NE -> v1 <> v2
	| GT -> v1 > v2
	| LT -> v1 < v2
	| GE -> v1 >= v2
	| LE -> v1 <= v2


let compare (op: operator) (v1:t) (v2:t) : bool * status =
	match (v1, v2) with
	| (Int i1, Int i2) -> (compare_value i1 i2, Success)
	| (Float f1, Float f2) -> (compare_value f1 f2, Success)
	| (String s1, String s2) -> (compare_value s1 s2, Success)
	| (Bool b1, Bool b2) -> (compare_value b1 b2, Success)
	| _ -> false, DBError "compare: wrong type in table contents"



(* [cond_row cond_list r] checks if row [n] satisfies condions in 
 * [cond_list] and returns true or false and the status 
 * requires: [n] to be non-empty
 *)
let rec cond_row (cond_list: condition lst) (colnames: colname * t list) (n: node) : bool * status =
	match cond_list with
	| [] -> (true, Success)
	| (col, op, v)::cond_list' -> 
		let num_found = List.(length (filter (fun (n,t) -> n = col) colnames)) in
		
		if num_found < 1 
		then (false, DBError "Column name " ^ col ^ " is not found.")
		
		else if num_found > 1 
		then (false, DBError "Duplicate columns " ^ col ^ " found.")
		
		else if List.(length colnames <> length n.value) 
		then (false, DBError
		"cond_row: Number of columns mismatch between colname and row content.")
		
		else if 
		List.(length (filter (fun (n,t) -> (n = col) && (match_type t v)))) <> 1
		then (false, DBError "Column " ^ col ^ " has the wrong type")
		
		else 
		let pair_list = List.combine colnames n.value in
		let (_, content) = List.find (fun ((cn, _), _) -> cn = col) pair_list in
		let res = compare op v content in
		match res with
		| (true, Success) -> cond_row cond_list' colnames n
		| _ -> res

(* [delete cond_list t] finds rows satisfies condions in 
 * [cond_list] in table [t]. *)
let find (cond_list: condition lst) (t: table): table status =
	let t' = empty_table t.name t.colnames in
	let rec helper (n : node option) : status = 
		match n with
		| None -> Success
		| Some r > 
			match cond_row cond_list t.colnames r with
			| (true, Success) -> 
				let r' = create_node r.value in
				ignore (t'.insert r'); helper r.next
			| (false, Success) -> helper r.next
		| (_, error) -> error
	in (t', helper t.first)


let in_some (a: 'a option) : 'a =
	match a with 
	| None -> failwith "in_some" 
	| Some b -> b

(* [delete r t] deletes a row [r] to from table [t]. *)
let delete (r: node) (t:table) :table status = 
	if t.first == r and t.last == r then 
		t.first <- None;
		t.last <- None;
		Success
	else if t.first == r && r.next <> None then 
		t.first <- r.next;
		(in_some r.next).prev <- None;
		Success
	else if t.last == r && r.prev <> None then 
		t.last <- r.prev;
		(in_some r.prev).next <- None;
		Success
	else if r.prev <> None && r.next <> None then
		(in_some r.prev).next = r.next;
		(in_some r.next).prev = r.prev;
		Success
	else 
		DBError "delete: row not found"







