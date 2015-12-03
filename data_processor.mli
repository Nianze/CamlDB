open Table
(* type of value *)
(* type t
type operator
type table
type condition
type condition lst
type order
type top_t
*)

(*
SQL:
SELECT column_name, column_name FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
val select_col: colname list -> table -> status * table

(*
SELECT TOP number|percent column_name(s)
FROM table_name;

select some particular columns in a list of colunms [col_list]
from table [t] and return a subtable
*)
val select_top: top_t -> colname list -> table -> status * table

(*
SQL:
SELECT DISTINCT column_name FROM table_name;

get all the distinct values of a column with the name of [col_name]
from table t and return a subtable
*)
val distinct: colname -> table -> status * table

(*
SQL:
SELECT *
FROM table_name
WHERE column_name operator value;

filter the table [t] according to the conditions in [cond_list]
and return a subtable
*)
val where: cond_tree -> table -> status * table

(*
SQL:
SELECT *
FROM table_name
ORDER BY column_name ASC|DESC;

sort the table [t] in the ascending or descending order of [o]
of column [col_name] and return a subtable
*)
val sort: colname -> order -> table -> status * table

(*
SQL:
INSERT INTO table_name
VALUES (value1,value2,value3,...);

insert a new row with values [val_list] in the order of columns
into table [t]
*)
val insert_values: t list -> table -> status

(*
SQL:
INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);

insert a new row, both the column names and the values to be
inserted [val_list] are speicified in [col_list]
*)
val insert_col_values: (colname * t) list -> table -> status


(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...;

update all the rows in the table [t] according to the column
and value specified by pair_list
*)
val update_all: (colname * t) list -> table -> status

(*
SQL:
UPDATE table_name
SET column1=value1,column2=value2,...
WHERE condition list;

update all the rows that satisfy the conditions in [cond_list]
in the table [t] according to the column and value specified
by [pair_list]
*)
val update_where: cond_tree -> (colname * t) list -> table -> status


(*
SQL:
DELETE FROM table_name;

delete all rows in the table [t]
*)
val delete_all: table -> status


(*
SQL:
DELETE FROM table_name
WHERE condition list;

delete all rows in the table [t] that satisfies the conditions in
[cond_list]
*)
val delete_where: cond_tree -> table -> status


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
val create_table: string -> (colname * t) list -> table



(*
SQL:
SELECT * FROM table1
UNION ALL
SELECT * FROM table2;

concatenate two tables [t1] and [t2] given [t1] and [t2] the same
number of columns. Assign the concatenated table column names
specified in [col_name_list]
*)
val union_rows: table -> table -> colname list -> status * table



(*
SQL:
SELECT tb1.col1, tb2.col2 FROM tb1
JOIN tb2
ON tb1.col3=tb2.col2;

perform an inner join on tables [tb1] and [tb2], selecting all rows
from both tables as long as there is a match between the columns.
*)
val inner_join: table -> table -> colname list ->
  (colname * colname) -> status * table
