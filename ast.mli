(** store column names, table names *)
type name = string

(** store column types*)
type typ =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string

(** the binary operators *)
type operator =
  | Gt | Lt | Eq | GE | LE | NE  (** >, <, =, >=, <=, <> *)

(** (OCaml) values of type expr represent SQL expressions.
    Here are some examples of how expressions are represented:
     - Int 7 represents 7
     - Percent 60.1 represents 60.1%
     - Bool true represents true
     - Order true represents Ascending, false represents descending order
     - TbName "GPA" represents table_name "GPA"
     - String "Tom" represents value "Tom"
     - BinOp (Gt, e1, e2) represents e1 > e2
     - SelCol (e1, e2, e3) represents SELECT col_name, col_name FROM table_name
     - SelTop (e1, e2) represents SELECT TOP 11% col_name
     - Distin (e1, e2) represents SELECT DISTINCT col_name FROM table_name
     - Where (e1, e2) represents WHERE col_name true|false
     - Sort (e1, e2) represents ORDER BY col_name ASC|DESC
     - Insert (e, [e1;e2;...])
       represents INSERT INTO table_name VALUES (val1,val2,...)
     - InsCol (e,[e11;e12;...], [e21;e22;...]) represents
       INSERT INTO table_name (col1,col2,...) VALUES (val1, val2,...)
     - Update (e, [e1;e2;...], [e1';e2';...]) represents
       UPDATE table_name SET col1=val1, col2=val2, ...
       WHERE condition1, condition2, ...
     - UpdAll (e, [e1;e2;...]) represents
       UPDATE table_name SET col1=val1, col2=val2, ...
     - Delete (e, [e1;e2;...]) represents
       DELETE FROM table_name WHERE condition1, condition2,...
     - DelAll (TbName GPA) represents DELETE FROM "GPA"
     - Create (e,[e1;e2;...]) represents
       CREATE TABLE table_name
       (col_name1 type1, col_name2, type2, ...)
     - Union (e1,e2) represents
       SELECT * FROM table1
       UNION ALL
       SELECT * FROM table2;
     *)
type expr =
  | Int      of int
  | Float    of float
  | Percent  of float
  | Bool     of bool
  | Order    of bool
  | TbName   of name
  | ColName  of name
  | ColType  of typ
  | String   of string
  | BinOp    of operator * expr * expr
  | SelCol   of expr * expr * expr
  | SelTop   of expr * expr
  | Distin   of expr * expr
  | Where    of expr * expr
  | Sort     of expr * expr
  | Insert   of expr * expr list
  | InsCol   of expr * expr list * expr list
  | Update   of expr * expr list * expr list
  | UpdAll   of expr * expr list
  | Delete   of expr * expr list
  | DelAll   of expr
  | Create   of expr * (expr * expr) list
  | Union    of expr * expr
