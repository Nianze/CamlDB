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

TEST "" = (find [("c2", GT, String "0")] t1) = ([], Success)

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

let _ = insert n1 t1
let _ = insert n2 t1
let _ = insert n3 t1
let _ = insert n4 t1
TEST "find" =
  let (l, s) =
  find [("c2", EQ, String "2"); ("c1", EQ, Int 2)] t1 in
  !(List.hd (List.hd l).value) = Int 2

TEST "find" =
  let (l, s) =
  find [("c2", GT, String "0"); ("c1", GT, Int 1)] t1 in
  List.length l = 2

TEST "delete_find" =
  let _ = delete_find [("c2", GT, String "0"); ("c1", GT, Int 2)] t1
  in t1.numrow = 3
