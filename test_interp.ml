open Interp
open Table

let exec s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast
    
let _ =
  let db = empty_table "db" [("a", Int 0); ("b", String "")] in
  assert (exec "CREATE TABLE db (a INT, b STRING);" = db)
(*
  parse "SELECT col1,col2 FROM tb;";
  parse "SELECT col1,col2 FROM tb #BAR;";
  parse "SELECT TOP 10 FROM tb;";
  parse "SELECT TOP 10 FROM tb #LINE;";
  parse "SELECT TOP 50 PERCENT FROM tb;";
  parse "SELECT TOP 13 PERCENT FROM tb #HISTOGRAM;";
  parse "SELECT TOP 20 PERCENT FROM tb WHERE col1 = 2 OR col2 = 4;";
  parse "SELECT DISTINCT col1 FROM tb #SCATTER;";
  parse "SELECT DISTINCT col1 FROM tb WHERE col1 = 2 OR col2 = 4;";
  parse "SELECT col1,col2 FROM a WHERE col1 = 2 AND col2 < 3 OR col3 > 1 AND col4 = 5;";
  parse "SELECT col1,col2 FROM a WHERE col1 = 2 OR col2 < 3 AND col3 > 1 OR col4 = 5;";
  parse "SELECT col1,col2 FROM a WHERE col1 = 2 AND (col2 < 3 OR col3 > 1) AND col4 = 5;";
  parse "SELECT col1,col2 FROM a ORDER BY col2 ASC;";
  parse "SELECT col1,col2 FROM a ORDER BY col4 DESC;";
  parse "SELECT col1,col2 FROM a WHERE col1=10 ORDER BY col3 DESC;";
  parse "INSERT INTO tb VALUES (\"val1\",2,true,false);";
  parse "INSERT INTO tb (col1,col2,col3) VALUES (\"val1\",2,true,false);";
  parse "UPDATE tb SET col1=1, col2=2;";
  parse "UPDATE tb SET col1=1, col2=2 WHERE col3<10;";
  parse "DELETE FROM tb1;";
  parse "DELETE FROM tb1 WHERE col1=1 AND col2<4;";
  parse "CREATE TABLE tb1 (col1_name INT, col2_name STRING, col3 BOOL);";
  parse "SELECT tb1.col1, tb2.col2 FROM tb1 JOIN tb2 ON tb1.col3=tb2.col2;"

*)
