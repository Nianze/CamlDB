open Table
open Data_processor

let is_error = function DBError _ -> true | _ -> false


(******** Create, insert, update, delete, union rows ********)
let t =
  create_table "name" [("c1", Int 0); ("c2", String ""); ("c3", Bool true)]

TEST "create_table" =
  t =
  {
    name = "name";
    colnames = [("c1", Int 0); ("c2", String ""); ("c3", Bool true)];
    numcol = 3;
    numrow = 0;
    first = None;
    last = None
  }


TEST "insert_values" =
  let s = insert_values [Int 1; String "1"; Bool false] t in
  s = Success &&
  t =
  {
    name = "name";
    colnames = [("c1", Int 0); ("c2", String ""); ("c3", Bool true)];
    numcol = 3;
  numrow = 1;
  first =
    Some
     {prev = None; next = None;
      value =
       [{contents = Int 1}; {contents = String "1"}; {contents = Bool false}]};
  last =
    Some
     {prev = None; next = None;
      value =
       [{contents = Int 1}; {contents = String "1"}; {contents = Bool false}]}
  }


TEST "insert_values" =
  let s1 = insert_values [Int 2; String "2"; Bool false] t in
  let s2 = insert_values [Int 3; String "3"; Bool false] t in
  let s3 = insert_values [Int 4; String "4"; Bool false] t in
  s1 = Success && s2 = Success && s3 = Success &&
  node_list_equal (in_some t.first) [Int 4; String "4"; Bool false] &&
  node_list_equal (in_some t.last) [Int 1; String "1"; Bool false]


TEST "insert_col_values" =
  let s1 = insert_col_values [("c2", String "2")] t in
  s1 = Success &&
  node_list_equal (in_some t.first) [Int 0; String "2"; Bool true]


TEST "insert_col_values" =
  let s1 = insert_col_values [] t in
  s1 = Success &&
  node_list_equal (in_some t.first) [Int 0; String ""; Bool true]


TEST "insert_col_values" =
  let s1 = insert_col_values
  [("c1", Int 12);("c2", String "12");("c3", Bool false)] t in
  s1 = Success &&
  node_list_equal (in_some t.first) [Int 12; String "12"; Bool false]

TEST "insert_col_values" =
  let s1 = insert_col_values
  [("c2", Int 12)] t in
  is_error s1

TEST "insert_col_values" =
  let s1 = insert_col_values
  [("c2", Int 12);("c2", String "12")] t in
  is_error s1

TEST "update_all" =
  let s = update_all [("c1", Int 0); ("c2", String ""); ("c3", Bool true)] t in
  s = Success &&
  fold_left
  (fun a n ->
    if node_list_equal n [Int 0; String ""; Bool true] then a else false)
  true t

TEST "update_all" =
  let s =
  update_all [("c1", String "0"); ("c2", String ""); ("c3", Bool true)] t in
  is_error s

TEST "delete_all" =
  let s = delete_all t in
  s = Success &&
  t =
  {
    name = "name";
    colnames = [("c1", Int 0); ("c2", String ""); ("c3", Bool true)];
    numcol = 3;
    numrow = 0;
    first = None;
    last = None
  }

let new_table n =
  let t =
  create_table "name" [("c1", Int 0); ("c2", String ""); ("c3", Bool true)] in
  for i = 1 to n do
    ignore (insert_values [Int i; String (string_of_int i); Bool false] t)
  done;
  t

let t9 = new_table 9
let t5 = new_table 5
let s1 =
  delete_where (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5"))) t9
let s2 =
  delete_where (And (Cond ("c1", LE, Int 2), Cond ("c2", LT, String "6"))) t5
let s3 =
  delete_where
  (And (Cond ("error", LE, Int 2), Cond ("error", LT, String "6")))
  t5


TEST "delete_where" =
  s1 = Success && s2 = Success && (is_error s3)
  && (table_equal t9 t5)


let t9' = new_table 9
let t5' = new_table 5
let s1 =
  update_where (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5")))
  [("c3", Bool true)]
  t9'
let s2 =
  update_where (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5")))
  [("c3", Bool true)]
  t5'

let s3 = delete_where (Cond ("c3", EQ, Bool true)) t9'
let s4 = delete_where (Cond ("c3", EQ, Bool true)) t5'
let s5 =
  update_where (Or (Cond ("error", LE, Int 2), Cond ("c2", GT, String "5")))
  [("c3", Bool true)]
  t9'




TEST "update_where" =
  s1 = Success && s2 = Success && s3 = Success && s4 = Success &&
  (is_error s5) && (table_equal t9 t9') && (table_equal t5 t5')
  && (table_equal t9' t5')

(******** Sort, Union rows ********)

let t100 = new_table 100
let t100' = new_table 100
let _ = find (Cond ("c1", EQ, Int 3)) t100'
let _ = find (Cond ("c1", LT, Int 2)) t100'
let _ = find (Cond ("c2", LE, String "5")) t100'
let _ = find (Cond ("c3", GT, Bool false)) t100'
TEST "sort" =
  table_equal t100 t100' = false
TEST "sort" =
  let s, t100'' = sort "c1" DESC t100' in
  table_equal t100 t100'' = true && s = Success
TEST "sort" =
  let s, t100_a' = sort "c1" ASC t100' in
  let s, t100_a  = sort "c1" ASC t100 in
  table_equal t100_a t100_a' = true && s = Success

let t3 = new_table 3
let t9 = new_table 9
let t9' = new_table 9
let s = delete_where (Cond ("c1", LE, Int 3)) t9

TEST "union_rows" =
  let (s1, t_u) =  union_rows t9 t3 ["c1"; "c2"; "c3"] in
  let (s2, t2) = sort "c1" ASC t_u in
  let (s3, t3) = sort "c1" ASC t9' in
  let (s4, t4) = sort "c1" DESC t_u in
  let (s5, t5) = sort "c1" DESC t9' in
  (table_equal t2 t3) && (table_equal t4 t5) && (s1 = Success) && (s = Success)
  && (s2 = Success) && (s3 = Success) && (s4 = Success) && (s5 = Success)

(******** Select, Select Top, Select Distinct, Where ********)
TEST "SELECT" =
  failwith "TODO"

TEST "SELECT TOP" =
  failwith "TODO"

TEST "SELECT DISTINCT" =
  failwith "TODO"

TEST "WHERE" =
  failwith "TODP"
