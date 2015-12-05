(* *header* *)
%{
open Ast
open Lexing
open Table
open Visualizer
%}

(* *declarations* *)

%token <int> INT
%token <float> FLOAT
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
%token TFLOAT
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
%token PLOT
%token SCATT
%token LINE
%token BAR
%token HISTOG
%token EOF

(* info about precedence & associativity *)
%left OR
%left AND

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
  | SEL; cols = col_list; FROM; f_tb = filtered_table { SelCol (cols, f_tb, VisNone)}
  | SEL; cols = col_list; FROM; f_tb = filtered_table; plot=vis_method { SelCol (cols, f_tb, plot)}
  | SEL; TOP; top = top_field; cols = col_list; FROM; f_tb = filtered_table { SelTop(top, cols, f_tb, VisNone) }
  | SEL; TOP; top = top_field; cols = col_list; FROM; f_tb = filtered_table; plot=vis_method  { SelTop(top, cols, f_tb, plot) }
  | SEL; DISTINCT; col = ID; FROM; f_tb = filtered_table { Distin(ColName col, f_tb, VisNone) }
  | SEL; DISTINCT; col = ID; FROM; f_tb = filtered_table; plot=vis_method { Distin(ColName col, f_tb, plot) }
  | INSERT; INTO; tb = ID; VALUES; LPAREN; vals = val_list;RPAREN {InsRow (vals,TbName tb)}
  | INSERT; INTO; tb = ID; LPAREN; cols = col_list; RPAREN; VALUES; LPAREN; vals = val_list;RPAREN {InsCol (cols,vals,TbName(tb))}
  | UPDATE; tb = ID; SET; pairs = pair_list {UpdAll (pairs,TbName tb) }
  | UPDATE; tb = ID; SET; pairs = pair_list; WHERE; conds = cond_list {Update (conds,pairs,TbName tb) }
  | DELETE; FROM; tb = ID { DelAll (TbName tb) }
  | DELETE; FROM; tb = ID; WHERE; conds = cond_list { Delete (conds,TbName tb) }
  | CREATE; TABLE; tb = ID; LPAREN; colsets = col_typ_list; RPAREN { Create (TbName tb, colsets) }
  | SEL; cols1 = col_list; FROM; tb1 = filtered_table; UNION; ALL; SEL; cols2 = col_list; FROM; tb2 = filtered_table { Union (SelCol (cols1, tb1, VisNone), SelCol(cols2,tb2, VisNone)) }
  | SEL; cols = col_list; FROM; tb1 = filtered_table; JOIN; tb2 = filtered_table; ON; j_cond = join_cond { Joins (tb1,tb2, cols, j_cond) }
  ;

top_field:
  | i = INT; PERCENT { TopPercent i}
  | i = INT; { TopNum i }
  ;

filtered_table:
  | tb = ID  { TbName tb }
  | tb = ID; WHERE; conds = cond_list  { Where (conds,TbName tb) }
  | tb = ID; WHERE; conds = cond_list; ORDER; BY; col = ID; ASC { Where (conds, Sort (ColName col, ASC, TbName tb) ) }
  | tb = ID; WHERE; conds = cond_list; ORDER; BY; col = ID; DESC { Where (conds, Sort (ColName col, DESC, TbName tb) ) }
  | tb = ID; ORDER; BY; col = ID; ASC  { Sort (ColName col, ASC, TbName tb) }
  | tb = ID; ORDER; BY; col = ID; DESC { Sort (ColName col, DESC,TbName tb) }
  ;

vis_method:
  | PLOT; SCATT  { Scatter2d }
  | PLOT; LINE   { LineGraph }
  | PLOT; BAR    { BarGraph }
  | PLOT; HISTOG { Hist2d }
  ;

col_list:
  | cols = separated_list(COMMA, col_field) { cols }
  ;

col_field:
  | ANY { ColName "*" }
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
  | f = FLOAT { Float f }
  | s = STRING { String s }
  | TRUE { Bool true }
  | FALSE { Bool false }
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
  | col = ID; TFLOAT { (ColName col, Float 0.0) }
  | col = ID; TSTRING { (ColName col, String "") }
  | col = ID; TBOOL  { (ColName col, Bool false) }
  ;

join_cond:
  tb1 = ID; DOT; col1 = ID; EQ;
  tb2 = ID; DOT; col2 = ID { (Path(TbName tb1, ColName col1),Path(TbName tb2, ColName col2)) }
