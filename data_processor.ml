open Table

(*
SQL:
CREATE TABLE table_name
(s
column_name1 data_type(size),
column_name2 data_type(size),
column_name3 data_type(size),
....
);

create a table with name [table_name], specify the type and
column name of each column by [col_name_list]
*)
let create_table (table_name: string) (col_name_list: (colname * t) list)
: status * table = empty_table table_name col_name_list


(*
SQL:
INSERT INTO table_name
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
into table [t] and return a subtable
*)
let insert_values (val_list: t list) (t: table) : status =
  if List.(length val_list > length (get_colnames t)) then
  DBError "Insert: inserting more columns in a row than defined columns"
  else if List.(length val_list < length (get_colnames t)) then
  DBError "Insert: inserting less columns in a row than defined columns"
  else let row = create_node (List.map (fun x -> ref x) val_list) in
  insert row t

(*
SQL:
INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);

insert a new row, both the column names and the values to be
inserted [val_list] are speicified in [col_list]
*)
let insert_col_values (col_list : (colname * t) list)
(t:table) : status =
  let (names, _) = List.split col_list in
  let colnames = get_colnames t in
  let not_empty = List.length col_list <> 0 in
  let no_duplicate_colnames =
    let (_, has_dup) =  List.(fold_left
      (fun (l, b) n ->
      if mem n l then (n::l, false)
      else (n::l, b) )
      ([], true) names)
    in has_dup in
  let cols_defined =
    List.(length (filter (fun n -> mem_assoc n colnames) names) =
      length names) in
  match (not_empty, cols_defined, no_duplicate_colnames) with
    | (false, _, _) -> DBError "Insert: empty insert list"
    | (true, false, _) ->

      let undefined_col =
      List.(fold_left
        (fun a n -> if a <> "" then a ^ ", " ^ n else n)
        ""
        (filter (fun n-> mem_assoc n colnames = false) names)
      )
      in DBError ("Insert: column(s) " ^ undefined_col ^ " not defined")
    | (true, true, false) ->
      DBError ("Insert: inserting to same column multiple times")
    | (true, true, true) -> (
      let colnames = List.map (fun (x, y) -> (x, ref y)) (get_colnames t) in
      List.(iter
        (fun (x, y) -> if mem_assoc x col_list then y:=assoc x col_list) colnames
      );
      let col = List.map (fun (x,y) -> y) colnames in
      let row = create_node col in
      insert row t
    )

let update_node (n: node) (colnames: (colname * t) list)
(pair_list : (colname * t) list): unit =
  List.(iter2
    (fun (c, ts) t ->
    if mem_assoc c pair_list
    then (t := (assoc c pair_list))
    else () )
  colnames n.value)

(*
SQL:
SELECT column_name, column_name FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
let select_col (col_list :colname list) (t: table): status * table =
  let colnames = get_colnames t in
  let out_cols = List.filter (fun (x,_)-> List.mem x col_list) colnames in
  let (_, out_tb) = empty_table (get_tablename t) out_cols in
  let out_index = List.map (get_col_i t) col_list in
  let get_vals val_list = List.map (List.nth val_list) out_index in
  let out node =
    let pairs = List.combine (col_list) (List.map (!) (get_vals node.value)) in
    insert_col_values pairs out_tb in
  let rec helper orig_n stat =
    match stat with
      | DBError e -> (DBError e, out_tb)
      | Success -> match orig_n with
        | Some node -> let next = node.next in
          helper next (out node)
        | None -> (Success, out_tb) in
   helper t.first Success

(*
SELECT TOP number|percent column_name(s)
FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
let select_top (top:top_t) (col_list:colname list) (t: table): status * table =
  let colnames = get_colnames t in
  let out_cols = List.filter (fun (x,_)-> List.mem x col_list) colnames in
  let (_, out_tb) = empty_table (get_tablename t) out_cols in
  let out_index = List.map (get_col_i t) col_list in
  let get_vals val_list =
    List.map (fun n -> !(List.nth val_list n)) out_index in
  let out node =
    let pairs = List.combine (col_list) (get_vals node.value) in
    insert_col_values pairs out_tb in
  let rec helper orig_n stat count_down =
    if count_down <= 0 then (Success, out_tb) else
      match stat with
        | DBError e -> (DBError e, out_tb)
        | Success -> match orig_n with
          | Some node -> let next = node.next in
            helper next (out node) (count_down-1)
          | None -> (Success, out_tb) in
  match top with
    | TopNum num -> if (num<0) then
      (DBError "Top number cannot be negative.", out_tb)
      else helper t.first Success num
    | TopPercent p -> if p>100 || p<0
      then (DBError "Percentage out of range", out_tb)
      else helper t.first Success (t.numrow*p/100)

(*
SQL:
SELECT DISTINCT column_name FROM table_name;

get all the distinct values of a column with the name of [col_name]
from table t and return a subtable
*)
let distinct (col_name :colname) (t :table) : status * table =
  if not (col_in_table t col_name)
    then (DBError "column names not found.", trivial_table ())
    else
      let colnames = get_colnames t in
      let out_cols = List.find (fun (x,_)-> x = col_name) colnames in
      let (_, out_tb) = empty_table (get_tablename t) [out_cols] in
      let out_index = get_col_i t col_name in
      let get_vals val_list = !(List.nth val_list out_index) in
      let out node =
        let pairs = (col_name,get_vals node.value) in
        insert_col_values [pairs] out_tb in
      let rec helper orig_n stat val_buffer =
        match stat with
          | DBError e -> (DBError e, out_tb)
          | Success -> match orig_n with
            | Some node -> let next = node.next in
              let node_val = get_vals node.value in
              if List.mem node_val val_buffer
                then helper next Success val_buffer
                else helper next (out node) (node_val::val_buffer)
            | None -> (Success, out_tb) in
       helper t.first Success []


(*
SQL:
SELECT *
FROM table_name
WHERE column_name operator value;

filter the table [t] according to the conditions in [cond_list]
and return a subtable
*)
let where (cond_list: cond_tree) (t :table) :status * table =
  match find cond_list t with
  | (nl, Success) -> (
  	let (_, new_t) = create_table (get_tablename t) (get_colnames t) in
  	List.iter (fun n -> ignore (insert (create_node n.value) new_t)) nl;
  	(Success,new_t) )
  | (_, DBError e) -> (DBError e, trivial_table () )



let get_cmp (o: order) (i: int) =
  match o with
  | ASC -> (
    fun n1 n2 ->
      let v1 = List.nth n1 i in
      let v2 = List.nth n2 i in
      if v1 > v2 then 1
      else if v1 = v2 then 0
      else -1)
  | DESC -> (
    fun n1 n2 ->
      let v1 = List.nth n1 i in
      let v2 = List.nth n2 i in
      if v1 < v2 then 1
      else if v1 = v2 then 0
      else -1)

(*
SQL:
SELECT *
FROM table_name
ORDER BY column_name ASC|DESC;

sort the table [t] in the ascending or descending order of [o]
of column [col_name] and return a subtable
*)
let sort (col_name: colname) (o: order) (t: table) : status * table =
  let colnames = get_colnames t in
  if (List.mem_assoc col_name colnames) = false then
    (DBError "sort: col_name not found", t)
  else
    let l_node = table_to_list t in
    let l = List.map (fun n -> n.value) l_node in
    let index = ref 0 in
    List.iteri
    (fun i (c, _) -> if c = col_name then index:= i else ())
    colnames;
    let sorted = List.sort (get_cmp o !index) l in
    let node_list = List.map (fun v -> create_node v) sorted in
    list_to_table (get_tablename t) (get_colnames t) node_list





(* check if each columns in pair_list can be found in colnames
         return:
                empty string "" if all columns found,
                string of names of the columns unfound otherwise
 *)
let colname_check (pair_list : (colname * t) list)
(colnames : (colname * t) list): string =
	List.(fold_left
		(fun s (c, _) ->
			if mem_assoc c colnames then s
			else if s = "" then c
			else s ^ ", " ^ c )
	"" pair_list)

(* check if each columns in pair_list has the same type as originally
         defined by table in colnames
         return:
                empty string "" if all column types match,
                string of names of the columns that don't type match
 *)
let type_check (pair_list : (colname * t) list)
(colnames : (colname * t) list): string =
	List.(fold_left
		(fun s (c, t) ->
			if match_type t (assoc c colnames) then s
			else if s = "" then c
			else s ^ ", " ^ c)
	"" pair_list)

(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...;

update all the rows in the table [t] according to the column
and value specified by [pair_list]
*)
let update_all (pair_list : (colname * t) list) (t:table) : status =
  let (names, _) = List.split pair_list in
  let colnames = get_colnames t in
  let not_empty = List.length pair_list <> 0 in
  let no_duplicate_colnames =
    let (_, has_dup) =  List.(fold_left
      (fun (l, b) n ->
      if mem n l then (n::l, false)
      else (n::l, b) )
      ([], true) names)
    in has_dup in
  let cols_defined =
    List.(length (filter (fun n -> mem_assoc n colnames) names) =
      length names) in
  match (not_empty, cols_defined, no_duplicate_colnames) with
    | (false, _, _) -> DBError "Update: empty insert list"
    | (true, false, _) ->
      let undefined_col =
      List.(fold_left
        (fun a n -> if a <> "" then a ^ ", " ^ n else n)
        ""
        (filter (fun n-> mem_assoc n colnames = false) names)
      )
      in DBError ("Update: column(s) " ^ undefined_col ^ " not defined")
    | (true, true, false) ->
      DBError ("Update: updating same column multiple times")
    | (true, true, true) ->
      let e_type = type_check pair_list colnames in
      if e_type <> "" then
        DBError
        ("Update_all: type of column "^e_type^" not match defined types")
      else
        let _ = iter (fun n -> update_node n colnames pair_list) t in
        Success

(*
SQL:
UPDATE Customers
SET ContactName='Alfred Schmidt', City='Hamburg'
WHERE CustomerName='Alfreds Futterkiste';

update all the rows that satisfy the conditions in [cond_list]
in the table [t] according to the column and value specified
by [pair_list]
*)
let update_where (cond_list: cond_tree) (pair_list :(colname * t) list)
(t:table) : status =
  match find cond_list t with
  | (nl, Success) -> (
    let colnames = get_colnames t in
    let e_find = colname_check pair_list colnames in
    let e_type = type_check pair_list colnames in
    match (e_find, e_type) with
      | ("", "") ->
              List.iter (fun n -> update_node n colnames pair_list) nl; Success
      | ("", _) ->
        DBError ("update_all: type of column "^e_type^" cannot be modified")
      | _ -> DBError ("update_all: can't find columns: " ^ e_find)
    )
  | (_, DBError e) -> DBError e

(*
SQL:
DELETE FROM table_name;

delete all rows in the table [t]
*)
let delete_all (t:table) : status =
  fold_left
  (fun a n ->
    match a with
      | Success -> delete n t
      | DBError e -> a
  ) Success t

(*
SQL:
DELETE FROM table_name
WHERE some_column=some_value;

delete all rows in the table [t] that satisfies the conditions in
[cond_list], disable the row under the hood
*)
let delete_where (cond_list: cond_tree) (t:table) : status =
	match find cond_list t with
	| (nl, Success) -> List.iter (fun n -> ignore (delete n t)) nl; Success
	| (_, DBError e) -> DBError e



(*
SQL:
SELECT * FROM table1
UNION ALL
SELECT * FROM table2;

concatenate two tables [t1] and [t2] given [t1] and [t2] the same
number of columns. Assign the concatenated table column names
specified in [col_name_list]
*)
let union_rows (t1: table) (t2: table) (col_name_list: colname list)
: status * table =
  (* check colnames in both table, throw error if not exist *)
  let (s1, t1_cols) = select_col col_name_list t1 in
  let (s2, t2_cols) = select_col col_name_list t2 in
  match (s1, s2) with
    | (DBError e, _) | (_, DBError e) -> (DBError e, t1)
    | (Success, Success) ->
      iter (fun x -> ignore (insert x t1)) t2;
      (Success, t1)

let split_name n =
  let start = (String.index n '#') + 1 in
  String.sub n start ((String.length n) - start)

let inner_join t1 t2 pathnames (path1, path2) =
  let (c1, c2) =
    if fst path1 = get_tablename t1 then (snd path1, snd path2)
    else (snd path2, snd path1) in

  let make_colnames t =
    List.map (fun (x, typ) ->
      (get_tablename t) ^ "#" ^ x, typ) (get_colnames t) in

  let (sts, t) = create_table "join" (make_colnames t1 @ (make_colnames t2)) in
  match sts with
  | DBError e -> (sts, t)
  | Success -> (
    let c1_index = snd (List.fold_left
  			(fun (i, found) v ->
  			  (i + 1, if fst v = c1 then i else found))
  			    (0, -1) (get_colnames t1)) in

    if c1_index >= 0 then
      let status = ref Success in
      iter (fun t1elt ->
        let c1_entry = !(List.nth t1elt.value c1_index) in
        match where (Cond (c2, EQ, c1_entry)) t2 with
        | (Success, matches) ->
  	 iter (fun mtch ->
  	   let node =
  	     {prev = None; next = None; value = t1elt.value @ mtch.value} in
  	   (match insert node t with
  	   | Success -> ()
  	   | DBError _ -> status := DBError "Could not join tables"
  	   )
  	 ) matches
        | (DBError _, _) -> status := DBError "Could not join tables"
      ) t1;
      let joined_colnames = List.map (fun (t, c) -> t ^ "#" ^ c) pathnames in
      match select_col joined_colnames t with
      | (Success, t') ->
         let cn = List.map (fun (n, typ) -> (split_name n, typ)) t'.colnames in
	 (if !status = Success then
	   status := fst (empty_table "#" cn)); (* no duplicate column names *)
	 let (_, t'') = list_to_table "join" cn (table_to_list t') in
         (!status, t'')
      | (DBError _, sel_t) -> (!status, sel_t)
    else (DBError "Join: column name not found", trivial_table ())
  )
