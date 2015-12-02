#directory "_build";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "parser_test.cmo";;
open Ast
open Parser_test
let _ = run_tests();;