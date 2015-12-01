open Table

(** store column names, table names *)
type name = string

(** store column types*)
type typ = Table.t

(** the binary operators *)
type operator = Table.operator

(* ex. ("column1", Eq, 3) *)
type cond = Table.condition

type c_tree = Table.cond_tree

(** SELECT TOP int (PERCENT) *)
type top_t = TopNum of int | TopPercent of int

type datatype = TString | TInt

type order = DESC | ASC

(** (OCaml) values of type expr represent SQL expressions.
    Here are some examples of how expressions are represented:
     - Int 7 represents 7
     - Bool true represents true
     - Order: ASC represents Ascending, DESC represents descending order
     - TbName "GPA" represents table_name "GPA"
     - String "Tom" represents value "Tom"
     - BinOp (Gt, e1, e2) represents e1 > e2
     - SelCol (e1, e2, e3) represents SELECT col_name, col_name FROM table_name
     - SelTop (e1, e2) represents SELECT TOP (11%||2) col_name
       e2:Top (11,true) represents TOP 11 PERCENT
          Top (2,false) represents TOP 2
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
  | Bool     of bool
  | String   of string
  | TbName   of name
  | ColName  of name
  | Path     of expr * expr
  | SelCol   of expr list * expr
  | SelTop   of top_t * expr
  | Distin   of expr * expr
  | Where    of c_tree * expr
  | Sort     of expr * order * expr
  | InsRow   of typ list * expr
  | InsCol   of expr list * typ list * expr
  | UpdAll   of (expr * typ) list * expr
  | Update   of c_tree * (expr * typ) list * expr
  | DelAll   of expr
  | Delete   of c_tree * expr
  | Create   of expr * (expr * typ) list
  | Union    of expr * expr
  | Joins    of expr * expr * expr list * (expr * expr) (* (Tb1, Tb2, path list, (path1,path2)) *)
