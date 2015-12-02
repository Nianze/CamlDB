open Table
open Data_processor

let is_error = function DBError _ -> true | _ -> false


(******************* Create, insert, update, delete *******************)
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
delete (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5"))) t9
let s2 =
delete (And (Cond ("c1", LE, Int 2), Cond ("c2", LT, String "6"))) t5
let l1 = table_to_list t9
let l2 = table_to_list t5

TEST "delete" =
  s1 = Success && s2 = Success &&
  List.fold_left2
  (fun a n1 n2 -> if node_equal n1 n2 then a else false)
  true l1 l2

let t9' = new_table 9
let t5' = new_table 5
let s1 =
update (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5")))
[("t3", Bool false)]
t9'
let s2 =
update (Or (Cond ("c1", LE, Int 2), Cond ("c2", GT, String "5")))
[("t3", Bool false)]
let s3 = delete (Cond ("c3", EQ, Bool false)) t9'
let s4 = delete (Cond ("c3", EQ, Bool false)) t5'
let l1' = table_to_list t9'
let l2' = table_to_list t5'
TEST "delete" =
  s1 = Success && s2 = Success && s3 = Success && s4 = Success &&
  List.fold_left2
  (fun a n1 n2 -> if node_equal n1 n2 then a else false)
  true l1' l2' &&
  List.fold_left2
  (fun a n1 n2 -> if node_equal n1 n2 then a else false)
  true l1' l1 &&
  List.fold_left2
  (fun a n1 n2 -> if node_equal n1 n2 then a else false)
  true l2' l2






