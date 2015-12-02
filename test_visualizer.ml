open Visualizer
open Table

let _ =
  print_endline "Drawing table.";
  let t1 = empty_table "test1" [("Temperature", Int 0);("Day", String "")] in
  
  let n1 = create_node [ref (Int 45); ref (String "Monday")] in
  let n2 = create_node [ref (Int 12); ref (String "Tuesday")] in
  let n3 = create_node [ref (Int 6); ref (String "Wednesday")] in
  let n4 = create_node [ref (Int 17); ref (String "Thursday")] in
  let n5 = create_node [ref (Int (-3)); ref (String "Friday")] in

  let _ = insert n1 t1 in
  let _ = insert n2 t1 in
  let _ = insert n3 t1 in
  let _ = insert n4 t1 in
  let _ = insert n5 t1 in

  print_tabular t1

let _ =
  print_endline "Drawing 2d scatter.";
  let t1 = empty_table "test1" [("Temperature", Int 0);("Day", String "")] in

  let n1 = create_node [ref (Int 45); ref (Float 20.)] in
  let n2 = create_node [ref (Int 12); ref (Float 15.)] in
  let n3 = create_node [ref (Int 6); ref (Float (-3.2))] in
  let n4 = create_node [ref (Int 17); ref (Float 7.)] in
  let n5 = create_node [ref (Int (-3)); ref (Float 4.)] in

  let _ = insert n1 t1 in
  let _ = insert n2 t1 in
  let _ = insert n3 t1 in
  let _ = insert n4 t1 in
  let _ = insert n5 t1 in

  visualize t1 Scatter2d;
  print_endline "Drawing line graph.";
  visualize t1 LineGraph
