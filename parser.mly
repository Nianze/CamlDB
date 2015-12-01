(* *header* *)
%{
open Ast
open Lexing
open Table
%}

(* *declarations* *)

%token <int> INT
%token TRUE
%token FALSE
%token <string> ID
%token <string> STRING
%token GT  (*>*)
%token LT  (*<*)
%token GE  (*>=*)
%token LE  (*<=*)
%token EQ  (*=*)
%token NE  (*<>*)
%token ANY (* '*' *)
%token SEL
%token COMMA
%token FROM
%token TOP
%token PERCENT
%token DISTINCT
%token WHERE
%token AND
%token OR
%token ORDER
%token BY
%token ASC
%token DESC
%token INSERT
%token INTO
%token VALUES
%token UPDATE
%token SET
%token DELETE
%token CREATE
%token TINT
%token TSTRING
%token TBOOL
%token TABLE
%token LPAREN RPAREN
%token UNION
%token ALL
%token JOIN
%token ON
%token DOT
%token SEMICOLON
%token EOF

(* info about precedence & associativity *)
%left OR
%left AND
%nonassoc INT STRING TRUE FALSE LPAREN

(* declare the starting point *)
%start <Ast.expr> prog

(* ends of the declarations section *)
%%

(* *rules* section *)

prog:
  | e = expr; EOF {e}
  ;

expr:
  | s = statement; SEMICOLON { s }
  ;

statement:
  | SEL; cols = col_list; FROM; tb = ID { SelCol (cols,TbName tb)}
  | SEL; TOP; i = INT; PERCENT; FROM; tb = ID  { SelTop(TopPercent i,TbName tb) }
  | SEL; TOP; i = INT; FROM; tb = ID { SelTop(TopNum i,TbName tb) }
  | SEL; DISTINCT; col = ID; FROM; tb = ID { Distin(ColName col,TbName tb) }
  | FROM; tb = ID; WHERE; conds = cond_list  { Where (conds,TbName tb) }
  | FROM; tb = ID; ORDER; BY; col = ID; ASC {Sort (ColName col, ASC,TbName tb) }
  | FROM; tb = ID; ORDER; BY; col = ID; DESC {Sort (ColName col, DESC,TbName tb) }
  | INSERT; INTO; tb = ID; VALUES; LPAREN; vals = val_list;RPAREN {InsRow (vals,TbName tb)}
  | INSERT; INTO; tb = ID; LPAREN; cols = col_list; RPAREN; VALUES; LPAREN; vals = val_list;RPAREN {InsCol (cols,vals,TbName(tb))}
  | UPDATE; tb = ID; SET; pairs = pair_list {UpdAll (pairs,TbName tb) }
  | UPDATE; tb = ID; SET; pairs = pair_list; WHERE; conds = cond_list {Update (conds,pairs,TbName tb) }
  | DELETE; FROM; tb = ID { DelAll (TbName tb) }
  | DELETE; FROM; tb = ID; WHERE; conds = cond_list { Delete (conds,TbName tb) }
  | CREATE; TABLE; tb = ID; LPAREN; colsets = col_typ_list; RPAREN { Create (TbName tb, colsets) }
  | SEL; ANY; FROM; tb1 = ID; UNION; ALL; SEL; ANY; FROM; tb2 = ID { Union (TbName tb1,TbName tb2) }
  | SEL; cols = col_list;FROM; tb1 = ID; JOIN; tb2 = ID;ON; j_cond = join_cond { Joins (TbName tb1,TbName tb2, cols, j_cond) }
  ;

col_list:
  cols = separated_list(COMMA, col_field) { cols };

col_field:
  | col = ID { ColName col }
  | tb = ID; DOT; col = ID { Path(TbName tb, ColName col) }
  ;

cond_list:
  | LPAREN; cond = cond_list; RPAREN { cond }
  | c = cond_tree; { c }
  ;

cond_tree:
  | LPAREN; c = cond_tree; RPAREN  { c }
  | single = cond_field  { (Cond single) }
  | left = cond_tree; AND; right = cond_tree { (And (left,right)) }
  | left = cond_tree; OR ; right = cond_tree { (Or  (left,right)) }
  ;

cond_field:
  | e1 = ID; GT; e2 = value_field { (e1,GT,e2) }
  | e1 = ID; LT; e2 = value_field { (e1,LT,e2) }
  | e1 = ID; GE; e2 = value_field { (e1,GE,e2) }
  | e1 = ID; LE; e2 = value_field { (e1,LE,e2) }
  | e1 = ID; EQ; e2 = value_field { (e1,EQ,e2) }
  | e1 = ID; NE; e2 = value_field { (e1,NE,e2) }
  ;

value_field:
  | i = INT { Int i }
  | s = STRING { String s }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
  ;

val_list:
  vl = separated_list(COMMA, value_field)  { vl };

pair_list:
  pr = separated_list(COMMA, pair_field)   { pr };

pair_field:
  col = ID; EQ; vl = value_field      { (ColName col, vl) };

col_typ_list:
  typs = separated_list(COMMA, typ_field)  { typs };

typ_field:
  | col = ID; TINT { (ColName col, Int 0) }
  | col = ID; TSTRING { (ColName col, String "") }
  | col = ID; TBOOL  { (ColName col, Bool false) }
  ;

join_cond:
  tb1 = ID; DOT; col1 = ID; EQ;
  tb2 = ID; DOT; col2 = ID { (Path(TbName tb1, ColName col1),Path(TbName tb2, ColName col2)) }
