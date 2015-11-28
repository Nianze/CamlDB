(* header *)

{
open Parser
}

(* identifiers *)
let white = ['' '\t']+
let digit = ['0'-'9']
let int = '-'?digit+
let letter = ['a'-'z' 'A'-'Z']
let str = letter+

(* token difinition *)

rule read =
  parse
  | white { read lexbuf }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | "="   { EQ }
  | "<>"  { NE }
  | "<="  { LE }
  | ">="  { GE }
  | "<"   { LT }
  | ">"   { GT }
  | "*"   { ANY }
  | ","   { COMMA }
  | ";"   { SEMICOLON }
  | "SELECT" { SEL }
  | "FROM"   { FROM }
  | "TOP"    { TOP }
  | "PERCENT" { PERCENT }
  | "DISTINCT" { DISTINCT }
  | "WHERE"  { WHERE }
  | "ORDER"  { ORDER }
  | "BY"     { BY }
  | "ASC"    { ASC }
  | "DESC"   { DESC }
  | "INSERT" { INSERT }
  | "INTO"   { INTO }
  | "VALUES" { VALUES }
  | "UPDATE" { UPDATE }
  | "SET"    { SET }
  | "DELETE" { DELETE }
  | "CREATE" { CREATE }
  | "TABLE"  { TABLE }
  | "UNION"  { UNION }
  | "ALL"    { ALL }
  | "JOINS"   { JOINS }
  | str as id   { ID (Lexing.lexeme lexbuf) }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' ([^'"']* as str) '"' { STRING str }
  | eof   { EOF }
