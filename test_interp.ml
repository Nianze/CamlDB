open Interp

let exec s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast
    
let _ =
  let db = empty_table "db" [("a", Int 0); ("b", String "")] in
  assert (exec "CREATE TABLE db (a INT, b STRING);" = db)
