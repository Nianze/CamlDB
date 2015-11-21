open Table

let t1 = empty_table "test1" [("c1", Int 0);("c2", String "")]


let n1 = create_node [ref (Int 0); ref (String "0")]
let n2 = create_node [ref (Int 1); ref (String "1")]
let n3 = create_node [ref (Int 2); ref (String "2")]
let n4 = create_node [ref (Int 3); ref (String "3")]

let _ = insert n1 t1
let _ = insert n2 t1
let _ = insert n3 t1
let _ = insert n4 t1
