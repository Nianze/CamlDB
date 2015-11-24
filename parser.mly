(* *header* *)
%{
open Ast
open Lexing
open Table
%}

(* *declarations* *)

%token <int> INT
%token <float> FLOAT
%token <float> TOP
%token PERCENT
%token <string> STRING
%token <string> COLNAME
%token <string> TBNAME
%token FALSE
%token TRUE
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
%token DISTINCT
%token WHERE
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
%token TABLE
%token LPAREN
%token RPAREN
(*%token UNION*)
(*%token JOIN*)
%token COLON
%token EOF

(* info about precedence & associativity *)
(*%nonassoc UNION*)
%nonassoc SEL INSROW INSCOL UPDATE DELETE CREATE
%nonassoc FROM WHERE ORDER
%nonassoc COMMA COLON
%left GT LT LE GE EQ NE ANY
%nonassoc TOP PERCENT
%nonassoc ASC DESC
%nonassoc TRUE FALSE INT STRING TBNAME COLNAME
%nonassoc LPAREN RPAREN

(* declare the starting point *)
%start <Ast.expr> prog

(* ends of the declarations section *)
%%

(* *rules* section *)

prog:
  | e = expr; EOF {e}
  ;

expr:
  | LPAREN; e = expr; RPAREN { e }
  | t = TRUE { Bool true }
  | f = FALSE { Bool false }
  | a = ASC { Order true }
  | d = DESC { Order false }
  | tb = TBNAME { TbName tb }
  | col = COLNAME { ColName col }
  | s = statement { s }
  ;

statement:
  | SEL; cols = col_list; FROM; tb = TBNAME; COLON { SelCol (List(cols),tb)}
  | SEL; TOP; f = FLOAT; PERCENT; FROM; tb = TBNAME; COLON  { SelTop(Top (f,true),tb) }
  | SEL; TOP; f = FLOAT; FROM; tb = TBNAME; COLON { SelTop(Top(f,false),tb) }
  | SEL; DISTINCT; col = COLNAME; tb = TBNAME; COLON { Distin(col,tb) }
  | FROM; tb = TBNAME; WHERE; conds = cond_list; COLON  { Where (List(conds),tb) }
  | FROM; tb = TBNAME; ORDER; BY; col = COLNAME; ACS; COLON {Sort (col,Order(true),tb) }
  | FROM; tb = TBNAME; ORDER; BY; col = COLNAME; DESC; COLON {Sort (col,Order(false),tb) }
  | INSERT; INTO; tb = TBNAME; VALUES; LPAREN; vals = val_list;RPAREN; COLON {InsRow (List(vals),tb)}
  | INSERT; INTO; tb = TBNAME; LPAREN; cols = col_list; RPAREN; VALUES; LPAREN; vals = val_list;RPAREN; COLON {InsCol (List(cols),List(vals),tb)}
  | UPDATE; tb = TBNAME; SET; pairs = pair_list; COLON {UpdAll (List(pairs),tb) }
  | UPDATE; tb = TBNAME; SET; pairs = pair_list; WHERE; conds = cond_list; COLON {Update (List(conds),List(pairs),tb) }
  | DELETE; FROM; tb = TBNAME; COLON { DelAll tb }
  | DELETE; FROM; tb = TBNAME; WHERE; conds = cond_list; COLON {Delete (List(conds),tb) }
  | CREATE; TABLE; tb = TBNAME; LPAREN; colsets = col_typ_list; RPAREN; COLON {Create (tb,List(colsets))}
  ;
(*  | UNION *)
(*  | JOIN  *)
col_list:
  col = separated_list(COMMA, col_field)   { col };

col_field:
  c = COLNAME { c };

cond_list:
  cond = separated_list(COMMA, cond_field)  { cond };

cond_field:
  | e1 = TBNAME; GT; e2 = value_field { BinOp (GT,e1,e2) }
  | e1 = TBNAME; LT; e2 = value_field { BinOp (LT,e1,e2) }
  | e1 = TBNAME; GE; e2 = value_field { BinOp (GE,e1,e2) }
  | e1 = TBNAME; LE; e2 = value_field { BinOp (LE,e1,e2) }
  | e1 = TBNAME; EQ; e2 = value_field { BinOp (EQ,e1,e2) }
  | e1 = TBNAME; NE; e2 = value_field { BinOp (NE,e1,e2) }
  ;

value_field:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  ;

val_list:
  vl = separated_list(COMMA, value_field)  { vl };

pair_list:
  pr = separated_list(COMMA, pair_field)   { pr };

pair_field:
  col = COLNAME; EQ; vl = value_field  { (col, vl) };
