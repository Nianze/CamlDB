open Table
open Data_processor

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

