open Interp

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec read_input s =
  (if s = "" then print_string "> ");
  let input = String.trim (read_line ()) in
  if String.length input > 0 &&
    String.get input ((String.length input) - 1) = ';' then s ^ input
  else
    read_input (s ^ input ^ " ")
      
let rec repl () =
   (try (
     let input = String.trim (read_input "") in
     if String.lowercase input = "save;" then (
       save_open_tables ();
       print_endline "Saved open tables."
     ) else if String.lowercase input = "exit;" then (
       print_endline "Saving all tables.";
       save_open_tables ();
       print_endline "Exiting.";
       exit 0
     );
     let _ = input |> parse |> eval in ()
   ) with
   | Failure e -> print_endline e
   | Invalid_argument _ -> print_endline "Invalid query."
   | Sys_error _ -> print_endline "Table does not exist."
   |  _ -> print_endline "Syntax error.");
  repl ()
   
