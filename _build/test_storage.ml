open Storage
open Table

let file_contents f =
  let ch = open_in f in
  let length = in_channel_length ch in
  let s = String.make length (Char.chr 0) in
  let _ = input ch s 0 length in
  close_in ch;
  s
  
let _ =
  let t1 = empty_table "test1" [("c1", Int 0);("c2", String "")] in
  
  let n1 = create_node [ref (Int 0); ref (String "0")] in
  let n2 = create_node [ref (Int 1); ref (String "1")] in
  let n3 = create_node [ref (Int 2); ref (String "2")] in
  let n4 = create_node [ref (Int 3); ref (String "3")] in
  let n5 = create_node [ref (Int 3); ref (String "3")] in

  let _ = insert n1 t1 in
  let _ = insert n2 t1 in
  let _ = insert n3 t1 in
  let _ = insert n4 t1 in
  let _ = insert n5 t1 in
  save_table t1 "table.json";
  let t2 = load_table "table.json" in
  save_table t2 "table2.json";
  assert (file_contents "table.json" = file_contents "table2.json")
