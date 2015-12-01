open Table

(* type of value *)
(* type t = Table.t
type operator = Table.operator
type table = Table.table
type condition = Table.condition *)
type order = DESC | ASC
type top_t = TopNum of int | TopPercent of int

(*
SQL:
SELECT column_name, column_name FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
let select_col (col_list :colname list) (t: table): table =
	failwith "unimplemented"


(*
SELECT TOP number|percent column_name(s)
FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
let select_top (top: top_t) (t: table): table =
	failwith "unimplemented"

(*
SQL:
SELECT DISTINCT column_name FROM table_name;

get all the distinct values of a column with the name of [col_name]
from table t and return a subtable
*)
let distinct (col_name :colname) (t :table) :table =
	failwith "unimplemented"


(*
SQL:
SELECT *
FROM table_name
WHERE column_name operator value;

filter the table [t] according to the conditions in [cond_list]
and return a subtable
*)
let where (cond_list: condition list) (t :table) :table =
	failwith "unimplemented"


(*
SQL:
SELECT *
FROM table_name
ORDER BY column_name ASC|DESC;

sort the table [t] in the ascending or descending order of [o]
of column [col_name] and return a subtable
*)
let sort (col_name: colname) (o: order) (t: table) : table * status =
	let colnames = get_colnames t in
	if (List.mem_assoc col_name colnames) = false then
		(t, DBError "sort: col_name not found")
	else
		let l = table_to_list t in
		let sorted = List.sort
		(fun n1 n2 ->
			if (List.assoc col_name n1) > (List.assoc col_name n2) then 1
			else if (List.assoc col_name n1) = (List.assoc col_name n2) then 0
			else -1) l in
		list_to_table (get_tablename table) (get_colnames table) sorted


(*
SQL:
INSERT INTO table_name
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
into table [t] and return a subtable
*)
let insert (val_list: t list) (t: table) : unit =
	let row = create_node (List.map (fun x -> ref x) val_list) in
	insert row t

(*
SQL:
INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);

insert a new row, both the column names and the values to be
inserted [val_list] are speicified in [col_list]
*)
let insert_col (col_list : (colname * t) list)
(t:table) : unit =
	let colnames = List.map (fun (x, y) -> (x, ref y)) (get_colnames t) in
	List.(iter
		(fun (x, y) -> if mem_assoc x col_list then y:=assoc x col_list) colnames
	);
	let col = List.map (fun (x,y) -> y) colnames in
	let row = create_node col in
	insert row t
s

(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...;

update all the rows in the table [t] according to the column
and value specified by [pair_list]
*)
let update_all (pair_list :colname * t list) (t:table) :table =
	failwith "unimplemented"

(*
SQL:
UPDATE Customers
SET ContactName='Alfred Schmidt', City='Hamburg'
WHERE CustomerName='Alfreds Futterkiste';

update all the rows that satisfy the conditions in [cond_list]
in the table [t] according to the column and value specified
by [pair_list]
*)
let update (cond_list: condition list) (pair_list :colname * t list)
(t:table) :table =
	failwith "unimplemented"


(*
SQL:
DELETE FROM table_name;

delete all rows in the table [t], disable the row under the hood
*)
let delete_all (t:table) :table =
	failwith "unimplemented"


(*
SQL:
DELETE FROM table_name
WHERE some_column=some_value;

delete all rows in the table [t] that satisfies the conditions in
[cond_list], disable the row under the hood
*)
let delete (cond_list: condition list) (t:table) :table =
	failwith "unimplemented"


(*
SQL:
CREATE TABLE table_name
(
column_name1 data_type(size),
column_name2 data_type(size),
column_name3 data_type(size),
....
);

create a table with name [table_name], specify the type and
column name of each column by [col_name_list]
*)
let create_table (table_name: string) (col_name_list: (colname * t) list)
: table = empty_table table_name col_name_list



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
: table =
	failwith "unimplemented"

