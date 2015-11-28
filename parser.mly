(* *header* *)
%{
open Ast
open Lexing
open Table
%}

(* *declarations* *)

%token <int> INT
%token <int> TOP
%token PERCENT
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
%token UNION
%token ALL
%token JOINS
%token SEMICOLON
%token EOF

(* info about precedence & associativity *)
%nonassoc UNION JOIN
%nonassoc SEL INSROW INSCOL UPDATE DELETE CREATE
%nonassoc FROM WHERE ORDER
%nonassoc COMMA SEMICOLON
%left GT LT LE GE EQ NE ANY
%nonassoc TOP PERCENT
%nonassoc ASC DESC
%nonassoc INT ID
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
  | a = ASC { Order true }
  | d = DESC { Order false }
(*  | id = ID  { ID id }*)
  | str = STRING { STRING str }
  | s = statement { s }
  ;

statement:
  | SEL; cols = col_list; FROM; tb = ID; SEMICOLON { SelCol (List(cols),TbName(tb))}
  | SEL; TOP; i = INT; PERCENT; FROM; tb = ID; SEMICOLON  { SelTop(Top (i,true),TbName(tb)) }
  | SEL; TOP; i = FLOAT; FROM; tb = ID; SEMICOLON { SelTop(Top(i,false),TbName(tb)) }
  | SEL; DISTINCT; col = COLNAME; tb = ID; SEMICOLON { Distin(ColName(col),TbName(tb)) }
  | FROM; tb = ID; WHERE; conds = cond_list; SEMICOLON  { Where (List(conds),TbName(tb)) }
  | FROM; tb = ID; ORDER; BY; col = ID; ACS; SEMICOLON {Sort (ColName(col),Order(true),TbName(tb)) }
  | FROM; tb = ID; ORDER; BY; col = ID; DESC; SEMICOLON {Sort (ColName(col),Order(false),TbName(tb)) }
  | INSERT; INTO; tb = ID; VALUES; LPAREN; vals = val_list;RPAREN; SEMICOLON {InsRow (List(vals),TbName(tb))}
  | INSERT; INTO; tb = ID; LPAREN; cols = col_list; RPAREN; VALUES; LPAREN; vals = val_list;RPAREN; SEMICOLON {InsCol (List(cols),List(vals),TbName(tb))}
  | UPDATE; tb = ID; SET; pairs = pair_list; SEMICOLON {UpdAll (List(pairs),TbName(tb)) }
  | UPDATE; tb = ID; SET; pairs = pair_list; WHERE; conds = cond_list; SEMICOLON {Update (List(conds),List(pairs),TbName(tb)) }
  | DELETE; FROM; tb = ID; SEMICOLON { DelAll TbName(tb) }
  | DELETE; FROM; tb = ID; WHERE; conds = cond_list; SEMICOLON {Delete (List(conds),TbName(tb)) }
  | CREATE; TABLE; tb = ID; LPAREN; colsets = col_typ_list; RPAREN; SEMICOLON {Create (TbName(tb),List(colsets))}
  | SELECT; ANY; FROM; tb1 = ID; UNION; ALL;
    SELECT; ANY; FROM; tb2 = ID; SEMICOLON {UNION (TbName(tb1),TbName(tb2))}
  | JOINS {}
  ;

col_list:
  col = separated_list(COMMA, col_field)   { col };

col_field:
  c = ID { ColName c };

cond_list:
  cond = separated_list(COMMA, cond_field)  { cond };

cond_field:
  | e1 = ID; GT; e2 = value_field { BinOp (GT,ColName(e1),e2) }
  | e1 = ID; LT; e2 = value_field { BinOp (LT,ColName(e1),e2) }
  | e1 = ID; GE; e2 = value_field { BinOp (GE,ColName(e1),e2) }
  | e1 = ID; LE; e2 = value_field { BinOp (LE,ColName(e1),e2) }
  | e1 = ID; EQ; e2 = value_field { BinOp (EQ,ColName(e1),e2) }
  | e1 = ID; NE; e2 = value_field { BinOp (NE,ColName(e1),e2) }
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
  col = ID; EQ; vl = value_field      { (ColName(col), vl) };

col_typ_list:
  typs = separated_list(COMMA, typ_field)  { typs }

typ_field:
  col = ID; tp = TYPE; LPAREN; i = INT; RPAREN { (ColName col,tp) }
