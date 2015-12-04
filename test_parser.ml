(*How to test parser:
  1.ocamlbuild -use-menhir test_parser.byte
  2.In utop, #use "run_test_parser.ml"
  3.To test "SELECT * FROM tb;", type in utop:
    parse "SELECT * FROM tb;";;
*)

open Ast

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* A few test cases *)
let run_tests () =
  parse "SELECT * FROM tb;";
  parse "SELECT col1,col2 FROM tb #BAR;";
  parse "SELECT TOP 10 col1 FROM tb;";
  parse "SELECT TOP 10 * FROM tb #LINE;";
  parse "SELECT TOP 50 PERCENT co1,col2 FROM tb;";
  parse "SELECT TOP 13 PERCENT col1 FROM tb #HISTOGRAM;";
  parse "SELECT TOP 20 PERCENT * FROM tb WHERE col1 = 2.0 OR col2 = 4;";
  parse "SELECT DISTINCT col1 FROM tb #SCATTER;";
  parse "SELECT DISTINCT col1 FROM tb WHERE col1 = 2 OR col2 = 4;";
  parse "SELECT col1,col2 FROM a
         WHERE col1 = 2 AND col2 < 3.1e+10 OR col3 > 1E-2 AND col4 = 5;";
  parse "SELECT col1,col2 FROM a
         WHERE col1 = 2 OR col2 < 3 AND col3 > 1 OR col4 = 5;";
  parse "SELECT col1,col2 FROM a
         WHERE col1 = 2 AND (col2 < 3 OR col3 > 1) AND col4 = 5;";
  parse "SELECT col1,col2 FROM a ORDER BY col2 ASC;";
  parse "SELECT col1,col2 FROM a ORDER BY col4 DESC;";
  parse "SELECT col1,col2 FROM a WHERE col1=10 ORDER BY col3 DESC;";
  parse "INSERT INTO tb VALUES (\"val1\",2,true,false);";
  parse "INSERT INTO tb (col1,col2,col3)
         VALUES (\"val1\",2,true,false);";
  parse "UPDATE tb SET col1=1, col2=2;";
  parse "UPDATE tb SET col1=1, col2=2 WHERE col3<10;";
  parse "DELETE FROM tb1;";
  parse "DELETE FROM tb1 WHERE col1=1 AND col2<4;";
  parse "CREATE TABLE tb1
        (col1_name INT,
         col2_name STRING,
         col3_name BOOL
         col4_name FLOAT
       );";
  parse "SELECT col1,col3 FROM tb1 UNION ALL SELECT * FROM tb2;";
  parse "SELECT tb1.col1, tb2.col2 FROM tb1
         JOIN tb2 ON tb1.col3=tb2.col2;"
