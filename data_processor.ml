
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
let where (cond_list: condition lst) (t :table) :table =
	failwith "unimplemented"


(*
SQL: 
SELECT *
FROM table_name
ORDER BY column_name ASC|DESC;

sort the table [t] in the ascending or descending order of [o]
of column [col_name] and return a subtable
*)
let sort (col_name: colname) (o: order) (t: table) :table =
	failwith "unimplemented"




(*
SQL:
INSERT INTO table_name
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
into table [t] and return a subtable
*)
let insert (val_list: t list) (t: table) :table =
	failwith "unimplemented"

(*
SQL:
INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
[col_list] into table [t] and return a subtable
*)
let insert_col (col_list :colname list) (val_list:t list) 
(t:table) :table =
	failwith "unimplemented"


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
let update (cond_list: condition lst) (pair_list :colname * t list) 
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
let delete (cond_list: condition lst) (t:table) :table =
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
let create_table (col_name_list: colname * datatype list) 
(table_name: string): table =
	failwith "unimplemented"


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


