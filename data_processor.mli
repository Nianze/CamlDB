(* type of value *)
type t 
type operator
type table
type condition
type condition lst
type order
type top_t

(*
SQL: 
SELECT column_name, column_name FROM table_name;

select some particular columns in a list of colunms [col_list] 
from table [t] and return a subtable 
*)
val select_col: colname list -> table -> table

(*
SELECT TOP number|percent column_name(s)
FROM table_name;

select some particular columns in a list of colunms [col_list] 
from table [t] and return a subtable 
*)
val select_top: top_t -> table -> table

(*
SQL: 
SELECT DISTINCT column_name FROM table_name;

get all the distinct values of a column with the name of [col_name]
from table t and return a subtable
*)
val distinct: colname -> table -> table

(*
SQL: 
SELECT *
FROM table_name
WHERE column_name operator value;

filter the table [t] according to the conditions in [cond_list]
and return a subtable
*)
val where: condition list -> table -> table

(*
SQL: 
SELECT *
FROM table_name
ORDER BY column_name ASC|DESC;

sort the table [t] in the ascending or descending order of [o]
of column [col_name] and return a subtable
*)
val sort: colname -> order -> table -> table

(*
SQL:
INSERT INTO table_name
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
into table [t]
*)
val insert: t list -> table -> table

(*
SQL:
INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
[col_list] into table [t] and return a subtable
*)
val insert_col: colname list -> t list -> table -> table 

(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...;

update all the rows in the table [t] according to the column
and value specified by pair_list
*)
val update_all: colname * t list -> table -> table

(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...
WHERE condition list;

update all the rows that satisfy the conditions in [cond_list] 
in the table [t] according to the column and value specified 
by [pair_list]
*)
val update: condition lst -> colname * t list -> table -> table


(*
SQL:
DELETE FROM table_name;

delete all rows in the table [t], disable the row under the hood
*)
val delete_all: table -> table


(*
SQL:
DELETE FROM table_name
WHERE condition list;

delete all rows in the table [t] that satisfies the conditions in
[cond_list], disable the row under the hood
*)
val delete: condition lst -> table -> table


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
val create_table: colname * t list -> string -> table



(*
SQL:
SELECT * FROM table1
UNION ALL
SELECT * FROM table2;

concatenate two tables [t1] and [t2] given [t1] and [t2] the same 
number of columns. Assign the concatenated table column names
specified in [col_name_list]
*)
val union_rows: table -> table -> colname list ->  table 
