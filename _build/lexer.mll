(* header *)

{
open Parser
}

(* identifiers *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'?digit+
let letter = ['a'-'z' 'A'-'Z']
let ident = (['a'-'z'] | '_') (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
(*let str = letter+*)

(* token difinition *)

rule read =
  parse
  | white { read lexbuf }
  | "("   { LPAREN }
  | ")"   { RPAREN }
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
  | "AND"    { AND }
  | "OR"     { OR }
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
  | "JOIN"   { JOIN }
  | "ON"     { ON }
  | "."      { DOT }
  | "INT"    { TINT }
  | "STRING" { TSTRING }
  | "BOOL"   { TBOOL }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | ident    { ID (Lexing.lexeme lexbuf) }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' ([^'"']* as str) '"' { STRING str }
  | "#"       { PLOT }
  | "SCATTER" { SCATT }
  | "LINE"    { LINE }
  | "BAR"     { BAR }
  | "HISTOGRAM" { HISTOG }
  | eof   { EOF }
