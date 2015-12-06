open Interp
open Lexer

(* Parse a string into an ast *)
let parse s =
  let helper s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
  ast in
  try helper s with
    | Failure "int_of_string" -> failwith "Input integer out of range"
    | Failure _ -> failwith "Invalid input."
    | _ -> helper s

let rec read_input s =
  (if s = "" then print_string "> ");
  let input = String.trim (read_line ()) in
  if String.length input > 0 &&
    String.get input ((String.length input) - 1) = ';' then s ^ input
  else
    read_input (s ^ input ^ " ")

let get_command s =
  let lower = String.lowercase s in
  let trim = String.trim lower in
  let has_semi = String.get trim ((String.length trim) - 1) = ';' in
  let rm_semi =
    if String.length trim = 0 then ""
    else String.sub trim 0 ((String.length trim) - 1) in
  let trim_again = String.trim rm_semi in
  trim_again ^ (if has_semi then "" else "`")

let rec repl () =
   (try (
     let input = String.trim (read_input "") in
     if get_command input = "save" then (
       save_open_tables ();
       print_endline "Saved all open tables."
     ) else if get_command input = "exit" then (
       print_endline "Saving all tables.";
       save_open_tables ();
       print_endline "Exiting.";
       exit 0
     ) else (
       let _ = input |> parse |> eval in ()
     )
   ) with
   | Failure e -> print_endline e
   | Invalid_argument _ -> print_endline "Invalid query."
   | SyntaxError e -> print_endline ("Syntax error. " ^ e)
   | Sys_error _ -> print_endline "Table does not exist."
   |  _ -> print_endline "Syntax error.");
  repl ()

