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
let match_type (t1:t) (t2:t) : bool =
	match (t1, t2) with
	| (Int _, Int _) | (Float _, Float _) | (String _, String _)
	| (Bool _, Bool _) -> true
	| _ -> false

(* [type_string v] convert a [v] to a string of its type
 *)
let type_string (v:t) : string =
	match v with
	| Int _ -> "int"
	| String _ -> "string"
	| Bool _ -> "bool"
	| Float _ -> "float"

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
let create_node v  = {prev=None; next=None; value=v}



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
let compare_value (op: operator) (v1: 'a) (v2: 'a) : bool =
	match op with
	| EQ -> v1 = v2
	| NE -> v1 <> v2
	| GT -> v1 > v2
	| LT -> v1 < v2
	| GE -> v1 >= v2
	| LE -> v1 <= v2

(* [compare_value t1 t2] compare two data of type [t] *)
let compare (op: operator) (v1:t) (v2:t) : bool * status =
	match (v1, v2) with
	| (Int i1, Int i2) -> (compare_value op i1 i2, Success)
	| (Float f1, Float f2) -> (compare_value op f1 f2, Success)
	| (String s1, String s2) -> (compare_value op s1 s2, Success)
	| (Bool b1, Bool b2) -> (compare_value op b1 b2, Success)
	| _ -> false, DBError "compare: compare type mismatch"


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
let get_tablename (t:table) : string = t.name

(* [get_colnames t] gets a list of column name of table [t] *)
let get_colnames (t:table) : (colname * t) list = t.colnames

(* [col_in_table t col_name] checks if col [col_name] is in table [t]  *)
let col_in_table (t: table) (col_name: colname): bool =
	let colnames = t.colnames in
	List.mem_assoc col_name colnames

(* [get_col_i t col_name] gets the index of col [col_name]
 * return -1 if col [col_name] not in table [t]
 *)
let get_col_i (t: table) (col_name: colname): int =
	let colnames = t.colnames in
	let index = ref (-1) in
	List.iteri (fun i (c, _) -> if col_name = c then index := i else ()) colnames;
	!index

(* [get_first t] gets the first node in table [t] *)
let get_first (t:table) : node option = t.first

(* [get_types t] gets the last node in table [t] *)
let get_last (t:table) : node option = t.last


(******************* Table Helper *******************)
let in_some (a: 'a option) : 'a =
	match a with
	| None -> failwith "in_some"
	| Some b -> b

(* [empty_table name colnames coltypes] is an empty table. *)
let empty_table (name :string) (colnames: (colname * t) list):table =
	{
		name = name;
		colnames = colnames;
		numcol = List.length colnames;
		numrow = 0;
		first = None;
		last = None
	}

(* check if each columns in pair_list has the same type as originally
	 defined by table in colnames
	 return:
	 	empty string "" if all column types match,
		string of names of the columns that don't type match
 *)
let insert_type_check (d_list : (t ref) list)
(colnames : (colname * t) list): bool =
	List.fold_left2
	(fun a tref (_, d) ->
		a && (match_type (!tref) d)
	) true d_list colnames

(* [insert r t] inserts a row [r] to the top of a
 * table [t].
 * require: nothing about the default prev, next of [r]
 *)
let insert (r: node) (t:table) : status =
	if insert_type_check r.value t.colnames then
		match t.first with
		| None ->
			r.prev <- None;
			r.next <- None;
			t.first <- Some r;
			t.last <- Some r;
			t.numrow <- 1;
			Success
		| Some n ->
			r.prev <- None;
			r.next <- Some n;
			t.first <- Some r;
			n.prev <- Some r;
			t.numrow <- (t.numrow + 1);
			Success
	else DBError "insert: row to insert doesn't type match with table"

(* [delete r t] deletes a row [r] from table [t]. *)
let delete (r: node) (t:table) : status =
	match (r.prev, r.next) with
	| (None, None) ->
		(t.first <- None);
		(t.last <- None);
		(r.prev <- None);
		(r.next <- None);
		t.numrow <- 0;
		Success
	| (Some x, None) ->
		(x.next <- None);
		(t.last <- Some x);
		(r.prev <- None);
		(r.next <- None);
		t.numrow <- t.numrow - 1;
		Success
	| (None, Some y) ->
		(y.prev <- None);
		(t.first <- Some y);
		(r.prev <- None);
		(r.next <- None);
		t.numrow <- t.numrow - 1;
		Success
	| (Some x, Some y) ->
		(x.next <- Some y);
		(y.prev <- Some x);
		(r.prev <- None);
		(r.next <- None);
		t.numrow <- t.numrow - 1;
		Success



(* let delete (r: node) (t:table) : status =
	match (t.first, t.last) with
	| (None, None) -> DBError ("delete: table " ^ t.name ^ " is empty")
	| (Some x, Some y) when x == r && y == r ->
			(t.first <- None);
			(t.last <- None);
			t.numrow <- 0;
			Success
	| (Some x, Some _) when x == r ->
			t.first <- r.next;
			(in_some r.next).prev <- None;
			t.numrow <- t.numrow - 1;
			Success
	| (Some _, Some y) when y == r ->
			t.last <- r.prev;
			(in_some r.prev).next <- None;
			t.numrow <- t.numrow - 1;
			Success
	| (Some _, Some _) ->
			(in_some r.prev).next <- r.next;
			(in_some r.next).prev <- r.prev;
			t.numrow <- t.numrow - 1;
			Success
	| _ -> DBError "delete: row not found" *)

(* [cond_row cond_list r] checks if row [n] satisfies condions in
 * [cond_list] and returns true or false and the status
 * requires: [n] to be non-empty
 *)
let rec cond_row (cond_list: cond_tree) (colnames: (colname * t) list)
(n: node) : bool * status =
	match cond_list with
	| Cond (col, op, v) -> (
		let num_found = List.(length (filter (fun (n,t) -> n = col) colnames)) in

		if num_found < 1
		then (false, DBError ("Column name " ^ col ^ " is not found.") )
		else if num_found > 1
		then (false, DBError ("Duplicate columns " ^ col ^ " found.") )
		else if List.(length colnames <> length n.value)
		then (false, DBError
		"cond_row: Number of columns mismatch between colname and row content.")
		else if
		List.(length
			(filter (fun (n,t) -> (n = col) && (match_type t v)) colnames)
		) <> 1
		then (false, DBError ("Column " ^ col ^ " has the wrong type") )

		else
		let pair_list = List.combine colnames n.value in
		let (_, content) = List.find (fun ((cn, _), _) -> cn = col) pair_list in
		compare op !content v
		)
	| And (c1, c2) -> (
		match (cond_row c1 colnames n, cond_row c2 colnames n) with
			| ((_, DBError e), _) -> (false, DBError e)
			| (_, (_, DBError e)) -> (false, DBError e)
			| ((true, _), (true, _)) -> (true, Success)
			| _ -> (false, Success)
		)
	| Or (c1, c2) -> (
		match (cond_row c1 colnames n, cond_row c2 colnames n) with
			| ((_, DBError e), _) -> (false, DBError e)
			| (_, (_, DBError e)) -> (false, DBError e)
			| ((false, _), (false, _))  -> (false, Success)
			| _ -> (true, Success)
		)



(* let rec cond_row (cond_list: condition list) (colnames: (colname * t) list)
(n: node) : bool * status =
	match cond_list with
	| [] -> (true, Success)
	| (col, op, v)::cond_list' ->
		let num_found = List.(length (filter (fun (n,t) -> n = col) colnames)) in

		if num_found < 1
		then (false, DBError ("Column name " ^ col ^ " is not found.") )

		else if num_found > 1
		then (false, DBError ("Duplicate columns " ^ col ^ " found.") )

		else if List.(length colnames <> length n.value)
		then (false, DBError
		"cond_row: Number of columns mismatch between colname and row content.")

		else if
		List.(length
			(filter (fun (n,t) -> (n = col) && (match_type t v)) colnames)
		) <> 1
		then (false, DBError ("Column " ^ col ^ " has the wrong type") )

		else
		let pair_list = List.combine colnames n.value in
		let (_, content) = List.find (fun ((cn, _), _) -> cn = col) pair_list in
		let res = compare op !content v in
		match res with
		| (true, Success) -> cond_row cond_list' colnames n
		| _ -> res
 *)

(* [find cond_list t] finds rows satisfies condions in
 * [cond_list] in table [t].
 *)
let find (cond_list: cond_tree) (t: table): (node list) * status =
	let rec helper (n : node option) (l: node list): (node list) * status =
		match n with
		| None -> (l,Success)
		| Some r ->
			match cond_row cond_list t.colnames r with
			| (true, Success) ->
				(* move to front *)
				helper r.next (r::l)
			| (false, Success) -> helper r.next l
		| (_, error) -> ([], error)
	in
	let (l, s) = helper t.first [] in
	List.iter (fun n -> ignore (delete n t); ignore (insert n t)) l;
	(l, s)


(* [list_to_table node_list] makes a copy of each node in [node_list]
 * and insert them into a new table and return that table
 *)
let list_to_table (node_list: node list) (table_name: string)
(colnames: (colname * t) list) : status * table =
	let t = empty_table table_name colnames in
	let rec helper = function
		| [] -> Success
		| hd::tl ->
			let r = create_node hd.value in
			(match insert r t with
				| Success -> helper tl
				| DBError e -> DBError e )
	in match helper node_list with
		| Success -> (Success, t)
		| DBError e -> (DBError e, t)


(* [delete_list n_list t] deletes a list of rows [n_list] from table [t]. *)
let rec delete_list (n_list: node list) (t:table) : status =
	match n_list with
		| [] -> Success
		| hd::tl -> (
			match delete hd t with
				| Success -> delete_list tl t
				| DBError e -> DBError e
			)

(* [delete_find cond_list t] delete all the rows that satisfy [cond_list]
 * in table [t]
 *)
let delete_find (cond_list: cond_tree) (t: table): status =
	let (node_list, s) = find cond_list t in
	match s with
		| Success -> delete_list node_list t
		| x -> x

(* [iter f t] iterate the table [t] and apply [f] on every row
 *)
let iter (f: node -> unit) (t:table): unit =
	let rec helper f node_o : unit =
		match node_o with
			| Some node ->
				let nxt = node.next in
				(f node); helper f nxt
			| None -> ()
	in helper f t.first

(* [fold_left f a t] iterate through table [t], apply [f] on every row
 * return: [a] accumulator
 *)
let fold_left (f: 'a -> node -> 'a) (a: 'a) (t:table): 'a =
	let rec helper f a node_o=
		match node_o with
			| Some node ->
				let nxt = node.next in
				helper f (f a node) nxt
			| None -> a
	in helper f a t.first

(* [table_to_list t] iterate the table [t] return a list of nodes
 * the most recent node is the first
 *)
let table_to_list (t: table): (node list) =
	let rec helper l node_o  =
		match node_o with
			| Some node -> helper (node::l) (node.prev)
			| None -> l
	in helper [] t.last

(* [list_to_table names colnames rows] converts a list to a table.
 * regard the first row in list as the most recent one
 * [name] - table name
 * [colnames] - colnames
 * [rows] - a list of node
 *)
let list_to_table (name :string) (colnames: (colname * t) list)
	(rows: node list) : status * table =
	let t = empty_table name colnames in
	(List.fold_right
		(fun n a ->
			match a with
			 	| Success -> insert n t
			  | DBError e -> a
		) rows Success,
	t)


(* [get_col_i t col_name] gets the col [col_name] as a t list *)
let get_col (t: table) (col_name: colname): t list =
	let index = get_col_i t col_name in
	fold_left (fun a n -> !(List.nth (n.value))::a) [] t


(* [node_equal n1 n2] compare two nodes and return true if they are equal
 * structrually
 *)
let node_equal (n1: node) (n2 :node) : bool =
  List.fold_left2
  (fun a v1 v2-> if !v1 = !v2 then a else false)
  true n1.value n2.value

(* [node_list_equal node lst] compare the value of a node with a list and
 * return true if they are equal structrually
 *)
let node_list_equal node lst =
  let nl = List.map (fun x -> !x) node.value in
  nl = lst

(* [table_equal t1 t2] returns true if each node in two table equals
 * structurally
 *)
let table_equal t1 t2 =
  let l1 = table_to_list t1 in
  let l2 = table_to_list t2 in
  List.fold_left2
  (fun a n1 n2 -> if node_equal n1 n2 then a else false) true l1 l2

