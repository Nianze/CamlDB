(*** Abstract syntax of CamlDB ***)

open Table

(** store column names, table names *)
type name = string

(** store column types*)
type typ = Table.t

(** the binary operators *)
type operator = Table.operator

(** ex. ("column1", Eq, 3) *)
type cond = Table.condition

(** condition tree type
   note that AND has a higher precedence than OR
 *)
type c_tree = Table.cond_tree

(** SELECT TOP int (PERCENT) *)
type top_t = Table.top_t

(** ASC|DESC indicate how to sort the output data *)
type order = Table.order

(** plot method:
  VisNone means none graph
  Scatter2d means scatter graph
  Hist2d means histogram graph
  BarGraph means bar graph
  LineGraph means line graph
 *)
type plot = Visualizer.vis_method

(** (OCaml) values of type expr represent SQL expressions.
    Here are some examples of how expressions are represented:
     - Int 7 represents 7
     - Bool true represents true
     - String "Tom" represents value "Tom"
     - TbName "Student" represents table_name Student
     - ColName "GPA" represents column name "GPA"
     - Path (tb,col) represents the column [col] in table [tb]
     - SelCol ([col1;col2], tb, plot) represents SELECT col1,col2 FROM tb
        (# plot);
     - SelTop (top, [col1;col2], tb, plot)
         represents SELECT TOP top col1,col2 FROM tb (#plot);
         top = TopNum int || TopPercent int
     - Distin (col, tb, plot) represents SELECT DISTINCT col FROM tb (#plot);
     - Where (cond_tree, tb) represents WHERE cond_tree FROM tb;
     - Sort (col, ord, tb)
         represents FROM tb ORDER BY col ord, where ord = ASC|DESC;
     - InsRow ([Int 1; String "a"], tb)
         represents INSERT INTO tb VALUES (1,"a");
     - InsCol ([col1;col2],[Int 1;Bool true], tb)
         represents INSERT INTO tb (col1,col2) VALUES (1,true);
     - UpdAll ([col1=1;col2=Bool false], tb)
         represents UPDATE tb SET col1=1, col2=false;
     - Update (cond_tree, [col1=1;col2=true], tb)
         represents UPDATE tb SET col1=1, col2=true WHERE cond_tree;
     - DelAll tb represents DELETE FROM tb;
     - Delete (cond_tree, tb) represents DELETE FROM tb WHERE cond_tree;
     - Create (tb,[(col1,INT 0);(col2,BOOL false),(col3, String "")])
         represents CREATE TABLE tb (col1 INT, col2 BOOL, col3 STRING);
     - Union (e1,e2) where e1= SelCol ([col1;col2], tb1, VisNone),
         e2= SelCol ([col3], tb2, VisNone),
         represents SELECT col1,col2 FROM tb1 UNION ALL SELECT col3 FROM tb2;
     - Joins(tb1, tb2, [tb1.col1;tb2.col3], (tb1.col3,tb2.col4)) represents
         SELECT tb1.col1,tb2.col3 FROM tb1 JOIN tb2 ON tb1.col3=tb2.col4;
     *)
type expr =
  | Int      of int
  | Bool     of bool
  | String   of string
  | TbName   of name
  | ColName  of name
  | Path     of expr * expr
  | SelCol   of expr list * expr * plot
  | SelTop   of top_t * expr list * expr * plot
  | Distin   of expr * expr * plot
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
  | Joins    of expr * expr * expr list * (expr * expr)
  | Err