open Visualizer
open Table

let _ =
  let t1 = empty_table "test1" [("Temperature", Int 0);("Day", String "")] in
  
  let n1 = create_node [ref (Int 45); ref (String "Mon")] in
  let n2 = create_node [ref (Int 12); ref (String "Tue")] in
  let n3 = create_node [ref (Int 6); ref (String "Wed")] in
  let n4 = create_node [ref (Int 17); ref (String "Thu")] in
  let n5 = create_node [ref (Int (-3)); ref (String "Fri")] in

  let _ = insert n1 t1 in
  let _ = insert n2 t1 in
  let _ = insert n3 t1 in
  let _ = insert n4 t1 in
  let _ = insert n5 t1 in

  print_tabular t1
