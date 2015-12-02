open Table

let is_error = function DBError _ -> true | _ -> false
(******************* Basic data type *******************)
TEST "match_type1" = match_type (Int 3) (Int 41) = true
TEST "match_type2" = match_type (String "3") (Int 41) = false
TEST "match_type3" = match_type (Bool true) (Float 231.1) = false

(******************* Node *******************)
TEST "create_node1" = create_node [] = {prev = None; next = None; value = []}
TEST "create_node2" =
  create_node [ref (Int 0); ref (String "0")]
  = {prev = None; next = None;
  value = [{contents = Int 0}; {contents = String "0"}]}

let n1 = create_node [ref (Int 0); ref (String "0")]
let n2 = create_node [ref (Int 1); ref (String "1")]
let n3 = create_node [ref (Int 2); ref (String "2")]
let n4 = create_node [ref (Int 3); ref (String "3")]
let n5 = create_node [ref (Int 3); ref (String "3")]

(******************* Compare *******************)
TEST "compare_value" = compare_value EQ 3 3 = true
TEST "compare_value" = compare_value NE false true = true
TEST "compare_value" = compare_value GT 3.8 3.2 = true
TEST "compare_value" = compare_value LT "b" "a" = false
TEST "compare_value" = compare_value GE 3.2 3.2 = true
TEST "compare_value" = compare_value LE "aaa" "bbb" = true
TEST "compare" = compare EQ (Int 3) (Int 3) = (true, Success)
TEST "compare" = compare NE (Bool false) (Bool true) = (true, Success)
TEST "compare" = compare GT (Float 3.8) (Float 3.2) = (true, Success)
TEST "compare" = compare LT (String "b") (String "a") = (false, Success)
TEST "compare" = compare GE (Float 3.2) (Float 3.2) = (true, Success)
TEST "compare" = compare LE (String "aaa") (String "bbb") = (true, Success)
TEST "compare" =
  let (_, x) = compare LE (Int 3) (String "bbb") in is_error x


(******************* Table *******************)
let t1 = empty_table "test1" [("c1", Int 0);("c2", String "")]
TEST "empty_table" =
  empty_table "test1" [("c1", Int 0);("c2", String "")]
  = {name = "test1"; colnames = [("c1", Int 0); ("c2", String "")];
    numcol = 2; numrow = 0; first = None; last = None}

TEST "" = (find (Cond ("c2", GT, String "0")) t1) = ([], Success)

let _ = insert n1 t1
let _ = insert n2 t1
let _ = insert n3 t1
let _ = insert n4 t1
TEST "insert" = t1.numrow = 4

let _ = delete (in_some (in_some t1.first).next) t1
TEST "delete" = t1.numrow = 3

let _ = delete (in_some t1.last) t1
TEST "delete" = t1.numrow = 2

let _ = delete (in_some t1.first) t1
TEST "delete" = t1.numrow = 1

let _ = delete (in_some t1.first) t1
TEST "delete" = t1.numrow = 0


let n1 = create_node [ref (Int 0); ref (String "0")]
let n2 = create_node [ref (Int 1); ref (String "1")]
let n3 = create_node [ref (Int 2); ref (String "2")]
let n4 = create_node [ref (Int 3); ref (String "3")]
let t2 = empty_table "test2" [("c1", Int 0);("c2", String "")]
let _ = insert n1 t2
let _ = insert n2 t2
let _ = insert n3 t2
let _ = insert n4 t2

TEST "cond_row" = cond_row (Cond ("c2", GE, String "3")) (get_colnames t1)
n4 = (true, Success)

TEST "cond_row" = cond_row (Cond ("c1", LT, Int 3)) (get_colnames t1)
n4 = (false, Success)

TEST "cond_row" = cond_row
  (Cond ("c2", GE, String "3"))
  (get_colnames t1)
n4 = (true, Success)

TEST "cond_row" = cond_row
  (Or (Cond ("c1", LT, Int 3), Cond ("c2", GE, String "3")))
  (get_colnames t1)
n4 = (true, Success)


TEST "cond_row" = cond_row
  (And (Cond ("c1", LT, Int 3), Cond ("c2", GE, String "3")))
  (get_colnames t1)
n4 = (false, Success)

TEST "cond_row" = cond_row
(Or (Cond ("c2", GT, String "3"), Cond ("c1", GT, Int 5))) (get_colnames t1)
n4 = (false, Success)

TEST "cond_row" = cond_row
(Or (
  And (Cond ("c2", GT, String "2"), Cond ("c1", LE, Int 3)),
  Cond ("c1", GT, Int 5))) (get_colnames t1)
n4 = (true, Success)

TEST "cond_row" = cond_row
(And (Cond ("c2", GT, String "3"), Cond ("c1", GT, Int 2))) (get_colnames t1)
n4 = (false, Success)

TEST "find" =
  let (l, s) =
  find (And (Cond ("c2", EQ, String "2"), Cond ("c1", EQ, Int 2))) t2 in
  !(List.hd (List.hd l).value) = Int 2


TEST "find" =
  let (l, s) =
  find (And (Cond ("c2", GT, String "0"), Cond ("c1", GT, Int 1))) t2 in
  List.length l = 2


TEST "delete_find" =
  let _ = delete_find
  (And (Cond ("c2", GT, String "0"), Cond ("c1", GT, Int 2)))
  t2
  in t2.numrow = 3

let new_table () =
  let n11 = create_node [ref (Int 0); ref (String "0")] in
  let n12 = create_node [ref (Int 1); ref (String "1")] in
  let n13 = create_node [ref (Int 2); ref (String "2")] in
  let n14 = create_node [ref (Int 3); ref (String "3")] in
  let t = empty_table "t" [("c1", Int 0);("c2", String "")] in
  let _ = insert n11 t in
  let _ = insert n12 t in
  let _ = insert n13 t in
  let _ = insert n14 t in
  ([n14;n13;n12;n11], t)

let (nl, t3) = new_table ()
let (nl2, t5) = new_table ()

TEST "table_to_list" =
  let nl1 = table_to_list t3 in
  List.fold_left2 (fun a n1 n2 -> if n1 == n2 then a else false) true nl1 nl

TEST "least recently used" =
  let n11 = create_node [ref (Int 0); ref (String "0")] in
  let n12 = create_node [ref (Int 1); ref (String "1")] in
  let n13 = create_node [ref (Int 2); ref (String "2")] in
  let n14 = create_node [ref (Int 3); ref (String "3")] in
  let t3 = empty_table "t" [("c1", Int 0);("c2", String "")] in
  let _ = insert n11 t3 in
  let _ = insert n12 t3 in
  let _ = insert n13 t3 in
  let _ = insert n14 t3 in

  let (l, s) =
  find (And (Cond ("c2", GT, String "0"), Cond ("c1", LE, Int 1))) t3 in
  let nl1 = table_to_list t3 in
  let nl2 = [n12; n14; n13; n11] in
  List.length l = 1 &&
  List.fold_left2 (fun a n1 n2 -> if n1 == n2 then a else false) true nl1 nl2

(* let node_equal n1 n2 =
  List.fold_left2
  (fun a v1 v2-> if !v1 = !v2 then a else false)
  true n1.value n2.value *)

TEST "list_to_table" =
  let nl1 = table_to_list t3 in
  let (s, t4) = list_to_table "t4" (get_colnames t3) nl1 in
  let nl2 = table_to_list t4 in
  List.fold_left2 (fun a n1 n2 -> if node_equal n1 n2 then a else false)
  true nl1 nl2


TEST "iter" =
  let nl1 = table_to_list t3 in
  let (s, t4) = list_to_table "t4" (get_colnames t3) nl1 in
  iter (fun n -> ignore (delete n t4)) t4;
  t4.numrow = 0



TEST "fold_left" =
  let nl1 = table_to_list t5 in
  let (s, t4) = list_to_table "t4" (get_colnames t5) nl1 in
  fold_left
  (fun a n ->
    match a with
      | Success -> delete n t4
      | DBError e -> a
  ) Success t4 = Success && t4.numrow = 0

TEST "col_in_table" =
  let (_, t) = new_table () in
  (col_in_table t "c1" = true) && (col_in_table t "c2" = true) &&
  (col_in_table t "error" = false)

TEST "get_col_i" =
  let (_, t) = new_table () in
  (get_col_i t "c1" = 0) && (get_col_i t "c2" = 1) &&
  (get_col_i t "error" = -1)

TEST "get_col" =
  let (_, t) = new_table () in
  (get_col t "c1" = [Int 3; Int 2; Int 1; Int 0]) &&
  (get_col t "c2" = [String "3"; String "2"; String "1"; String "0"]) &&
  (get_col t "error" = [])
