open Interp
open Data_processor
open Visualizer
open Table

let exec s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast
    
let _ =
  let t = empty_table "t" [("a", Int 0); ("b", String "")] in
  assert (table_equal (exec "CREATE TABLE t (a INT, b STRING);") t);
  ignore (insert_values [Int 3; String "hi"] t);
  assert (table_equal (exec "INSERT INTO t VALUES (3, \"hi\");") t);
  ignore (insert_values [Int 4; String "bye"] t);
  assert (table_equal (exec "INSERT INTO t VALUES (4, \"bye\");") t);
  ignore (insert_values [Int 4; String "asdf"] t);
  assert (table_equal (exec "INSERT INTO t VALUES (4, \"asdf\");") t);
  let t1 = snd (select_col ["a"] t) in
  assert (table_equal (exec "SELECT a FROM t;") t1);
  let t2 = snd (select_top (TopPercent 50) ["a"; "b"] t) in
  assert (table_equal (exec "SELECT TOP 50 PERCENT a, b FROM t;") t2);
  let t3 = snd (distinct "a" t) in
  assert (table_equal (exec "SELECT DISTINCT a FROM t;") t3);
  let t4 = snd (where (Cond ("a", EQ, Int 3)) t) in
  assert (table_equal (exec "SELECT a, b FROM t WHERE a = 3;") t4);
  assert (table_equal (exec "SELECT * FROM t WHERE a = 3;") t4);
  let v = empty_table "v" [("a", Int 0); ("c", Int 0)] in
  ignore (exec "CREATE TABLE v (a INT, c INT);");
  ignore (insert_values [Int 3; Int 4] v);
  ignore (exec "INSERT INTO v VALUES (3, 4);");
  ignore (insert_values [Int 3; Int 5] v);
  ignore (exec "INSERT INTO v VALUES (3, 5);");
  let join = snd (inner_join t v [("t", "a"); ("v", "c")] (("t", "a"), ("v", "a"))) in
  assert (table_equal (exec "SELECT t.a, v.c FROM t JOIN v ON t.a=v.a;") join);
  ignore (delete_where (Cond ("a", EQ, Int 3)) t);
  assert (table_equal (exec "DELETE FROM t WHERE a=3;") t)
