open Interp

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec repl () =
  print_string "> ";
  (try (
    let input = read_line () in
    if String.lowercase input = "exit" then (
      print_endline "Saving all tables.";
      shutdown_interp ();
      print_endline "Exiting.";
      exit 0
    );
    let _ = input |> parse |> eval in ()
  ) with
    _ -> print_endline "Syntax error.");
  repl ()

let _ = repl ()    
