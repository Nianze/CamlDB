open Ast
open Table
open Storage
open Data_processor
(*
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
  | Where    of cond list * expr
  | Sort     of expr * order * expr
  | InsRow   of typ list * expr
  | InsCol   of expr list * typ list * expr
  | UpdAll   of (expr * typ) list * expr
  | Update   of cond list * (expr * typ) list * expr
  | DelAll   of expr
  | Delete   of cond list * expr
  | Create   of expr * (expr * datatype) list
  | Union    of expr * expr
  | Joins    of expr * expr * expr list * (expr * expr) (* (Tb1, Tb2, path list, (path1,path2)) *)
*)

let open_tables = ref []

let table_named n =
  if List.mem_assoc n !open_tables then
    List.assoc n !open_tables
  else
    let t = load_table n in
    open_tables := (n, t)::!open_tables;
    t

let name_of_expr = function
  | ColName n -> n
  | TbName n -> n
  | _ -> failwith "Syntax error"

let error_table = empty_table "" []

let proc_singleton_status = function
  | Success -> ()
  | DBError x -> print_endline ("Database Error: " ^ x); ()

let proc_status (s, t) = match s with
  | Success -> t
  | DBError x -> print_endline ("Database Error: " ^ x); error_table
     
let rec eval = function
  | TbName n ->
     table_named n
  | SelCol (lst, n) ->
     proc_status (select_col (List.map name_of_expr lst) (eval n))
  | SelTop (top, n) ->
     proc_status (select_top top (eval n))
  | Distin (colname, n) ->
     proc_status (distinct (name_of_expr colname) (eval n))
  | Where (cond_lst, expr) ->
     proc_status (where cond_lst (eval expr))
  | Sort (colname, order, expr) ->
     proc_status (sort (name_of_expr colname) order (eval expr))
  | InsRow (typ_lst, expr) ->
     proc_singleton_status (insert_values typ_lst (eval expr));
     (eval expr)
  | InsCol (es, typ_lst, expr) ->
     let colnames = List.map name_of_expr es in
     proc_singleton_status (insert_col_values (List.combine colnames typ_lst)
			   (eval expr));
     (eval expr)
  | UpdAll (lst, expr) ->
     let (es, typs) = List.split lst in
     let colnames = List.map name_of_expr es in
     proc_singleton_status (update_all (List.combine colnames typs)
			      (eval expr));
     (eval expr)
  | Update (conds, lst, expr) ->
     let (es, typs) = List.split lst in
     let colnames = List.map name_of_expr es in
     proc_singleton_status (update conds (List.combine colnames typs)
			      (eval expr));
     (eval expr)
  | DelAll e ->
     proc_singleton_status (delete_all (eval e));
     (eval e)
  | Delete (cond_lst, e) ->
     proc_singleton_status (delete cond_lst (eval e));
     (eval e)
  | Create (n, lst) ->
     let (es, typs) = List.split lst in
     let colnames = List.map name_of_expr es in
     create_table (name_of_expr n) (List.combine colnames typs)
  | Union (e1, e2) ->
     let t1 = (eval e1) in
     proc_status (union_rows t1 (eval e2) (fst (List.split (get_colnames t1))))
  | Joins _ -> failwith "unimplemented"
  | _ -> failwith "TODO"
