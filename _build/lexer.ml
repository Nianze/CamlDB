# 3 "lexer.mll"
 
open Lexing
open Parser

exception SyntaxError of string
(*exception LexErr of string*)
(*
let error msg start finish  =
    Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum
          (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

let lex_error lexbuf =
    raise ( LexErr (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))
*)

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

# 25 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\201\255\202\255\002\000\004\000\207\255\002\000\080\000\
    \092\000\114\000\194\000\022\001\106\001\190\001\125\000\002\000\
    \000\000\010\000\006\000\000\000\107\000\011\000\074\000\019\000\
    \023\000\025\000\074\000\108\000\118\000\243\255\244\255\245\255\
    \002\000\065\000\251\255\252\255\253\255\254\255\001\000\005\000\
    \249\255\250\255\248\255\059\000\069\000\113\000\226\255\085\000\
    \088\000\072\000\242\255\085\000\081\000\117\000\215\255\107\000\
    \108\000\126\000\116\000\206\255\120\000\121\000\124\000\241\255\
    \138\000\120\000\216\255\139\000\126\000\240\255\131\000\139\000\
    \223\255\127\000\143\000\142\000\134\000\129\000\239\255\138\000\
    \133\000\133\000\145\000\141\000\153\000\138\000\238\255\154\000\
    \157\000\231\255\141\000\158\000\225\255\159\000\147\000\161\000\
    \237\255\155\000\165\000\166\000\236\255\232\255\221\255\219\255\
    \167\000\167\000\155\000\234\255\156\000\160\000\233\255\164\000\
    \214\255\204\255\169\000\162\000\185\000\173\000\173\000\230\255\
    \229\255\209\000\201\000\218\000\205\000\228\255\217\000\250\000\
    \254\000\236\000\253\000\227\255\245\000\247\000\222\255\011\001\
    \016\001\254\000\014\001\224\255\011\001\007\001\220\255\014\002\
    \024\002\034\002\046\002\065\002\149\002\233\002\061\003\145\003\
    \229\003\057\004\141\004\208\255\112\001\008\001\044\001\205\255\
    \031\001\031\001\037\001\047\001\065\001\083\001\072\001\203\255\
    ";
  Lexing.lex_backtrk = 
   "\046\000\255\255\255\255\053\000\053\000\255\255\053\000\053\000\
    \045\000\046\000\044\000\044\000\044\000\044\000\037\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\255\255\255\255\255\255\
    \009\000\008\000\255\255\255\255\255\255\255\255\001\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\038\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \046\000\046\000\255\255\044\000\044\000\042\000\044\000\044\000\
    \044\000\043\000\044\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\255\255\255\255\000\000\156\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\156\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\039\000\037\000\037\000\000\000\038\000\039\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \039\000\000\000\006\000\005\000\155\000\039\000\000\000\000\000\
    \036\000\035\000\031\000\000\000\030\000\009\000\014\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\000\000\029\000\033\000\034\000\032\000\042\000\
    \000\000\022\000\020\000\016\000\024\000\007\000\027\000\121\000\
    \003\000\019\000\015\000\160\000\004\000\157\000\114\000\021\000\
    \025\000\140\000\135\000\028\000\026\000\017\000\018\000\023\000\
    \126\000\103\000\127\000\093\000\079\000\104\000\073\000\010\000\
    \080\000\010\000\010\000\010\000\010\000\011\000\012\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\013\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\146\000\055\000\146\000\040\000\041\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\144\000\067\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\097\000\051\000\
    \099\000\068\000\048\000\049\000\050\000\098\000\052\000\053\000\
    \144\000\143\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\108\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\143\000\
    \060\000\043\000\109\000\045\000\054\000\047\000\061\000\056\000\
    \057\000\143\000\143\000\058\000\110\000\046\000\059\000\064\000\
    \062\000\063\000\044\000\065\000\066\000\070\000\069\000\071\000\
    \072\000\074\000\075\000\076\000\077\000\078\000\087\000\143\000\
    \081\000\082\000\083\000\084\000\085\000\088\000\086\000\090\000\
    \089\000\091\000\143\000\092\000\094\000\095\000\096\000\102\000\
    \101\000\010\000\100\000\105\000\106\000\107\000\113\000\111\000\
    \112\000\120\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\116\000\115\000\117\000\118\000\
    \001\000\119\000\255\255\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\122\000\123\000\124\000\
    \125\000\010\000\132\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\128\000\129\000\
    \130\000\146\000\131\000\146\000\133\000\134\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \136\000\137\000\138\000\139\000\141\000\142\000\158\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\159\000\161\000\162\000\163\000\010\000\164\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\155\000\165\000\166\000\167\000\000\000\000\000\
    \000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
    \000\000\010\000\000\000\150\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\000\000\000\000\000\000\000\000\010\000\000\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \147\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\146\000\000\000\146\000\000\000\000\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\000\000\143\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\000\000\000\000\143\000\000\000\000\000\
    \000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\148\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\010\000\000\000\010\000\010\000\
    \010\000\010\000\149\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\010\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\151\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\152\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\010\000\000\000\010\000\010\000\
    \010\000\010\000\153\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\010\000\000\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\038\000\255\255\000\000\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\006\000\039\000\255\255\255\255\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\032\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
    \000\000\000\000\000\000\003\000\000\000\004\000\019\000\000\000\
    \000\000\015\000\016\000\000\000\000\000\000\000\000\000\000\000\
    \017\000\021\000\017\000\023\000\024\000\021\000\025\000\000\000\
    \024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\043\000\007\000\033\000\033\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\008\000\026\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\022\000\044\000\
    \022\000\026\000\047\000\048\000\049\000\022\000\051\000\052\000\
    \009\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\020\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\009\000\
    \027\000\028\000\020\000\028\000\053\000\045\000\027\000\055\000\
    \056\000\008\000\014\000\057\000\020\000\045\000\058\000\060\000\
    \061\000\062\000\028\000\064\000\065\000\067\000\068\000\070\000\
    \071\000\073\000\074\000\075\000\076\000\077\000\079\000\009\000\
    \080\000\081\000\082\000\083\000\084\000\079\000\085\000\087\000\
    \088\000\090\000\014\000\091\000\093\000\094\000\095\000\097\000\
    \098\000\010\000\099\000\104\000\105\000\106\000\108\000\109\000\
    \111\000\115\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\114\000\114\000\116\000\117\000\
    \000\000\118\000\006\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\121\000\122\000\123\000\
    \124\000\010\000\126\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\011\000\127\000\128\000\
    \129\000\011\000\130\000\011\000\132\000\133\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \135\000\136\000\137\000\138\000\140\000\141\000\157\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\158\000\160\000\161\000\162\000\011\000\163\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\012\000\156\000\164\000\165\000\166\000\255\255\255\255\
    \255\255\255\255\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\255\255\255\255\255\255\
    \255\255\012\000\255\255\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\013\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\255\255\255\255\255\255\255\255\013\000\255\255\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\143\000\255\255\143\000\255\255\255\255\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\255\255\144\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \147\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \156\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\255\255\255\255\144\000\255\255\255\255\
    \255\255\255\255\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\255\255\255\255\255\255\255\255\
    \147\000\255\255\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\148\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \255\255\255\255\255\255\255\255\148\000\255\255\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \149\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\255\255\255\255\255\255\255\255\
    \149\000\255\255\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\150\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \255\255\255\255\255\255\255\255\150\000\255\255\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \151\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\255\255\255\255\255\255\255\255\
    \151\000\255\255\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\152\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \255\255\255\255\255\255\255\255\152\000\255\255\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \153\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\255\255\255\255\255\255\255\255\
    \153\000\255\255\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\154\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \255\255\255\255\255\255\255\255\154\000\255\255\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec read lexbuf =
    __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 42 "lexer.mll"
             ( read lexbuf )
# 475 "lexer.ml"

  | 1 ->
# 43 "lexer.mll"
             ( next_line lexbuf; read lexbuf )
# 480 "lexer.ml"

  | 2 ->
# 44 "lexer.mll"
          ( LPAREN )
# 485 "lexer.ml"

  | 3 ->
# 45 "lexer.mll"
          ( RPAREN )
# 490 "lexer.ml"

  | 4 ->
# 46 "lexer.mll"
          ( EQ )
# 495 "lexer.ml"

  | 5 ->
# 47 "lexer.mll"
          ( NE )
# 500 "lexer.ml"

  | 6 ->
# 48 "lexer.mll"
          ( LE )
# 505 "lexer.ml"

  | 7 ->
# 49 "lexer.mll"
          ( GE )
# 510 "lexer.ml"

  | 8 ->
# 50 "lexer.mll"
          ( LT )
# 515 "lexer.ml"

  | 9 ->
# 51 "lexer.mll"
          ( GT )
# 520 "lexer.ml"

  | 10 ->
# 52 "lexer.mll"
          ( ANY )
# 525 "lexer.ml"

  | 11 ->
# 53 "lexer.mll"
          ( COMMA )
# 530 "lexer.ml"

  | 12 ->
# 54 "lexer.mll"
          ( SEMICOLON )
# 535 "lexer.ml"

  | 13 ->
# 55 "lexer.mll"
             ( SEL )
# 540 "lexer.ml"

  | 14 ->
# 56 "lexer.mll"
             ( FROM )
# 545 "lexer.ml"

  | 15 ->
# 57 "lexer.mll"
             ( TOP )
# 550 "lexer.ml"

  | 16 ->
# 58 "lexer.mll"
               ( PERCENT )
# 555 "lexer.ml"

  | 17 ->
# 59 "lexer.mll"
               ( DISTINCT )
# 560 "lexer.ml"

  | 18 ->
# 60 "lexer.mll"
             ( WHERE )
# 565 "lexer.ml"

  | 19 ->
# 61 "lexer.mll"
             ( AND )
# 570 "lexer.ml"

  | 20 ->
# 62 "lexer.mll"
             ( OR )
# 575 "lexer.ml"

  | 21 ->
# 63 "lexer.mll"
             ( ORDER )
# 580 "lexer.ml"

  | 22 ->
# 64 "lexer.mll"
             ( BY )
# 585 "lexer.ml"

  | 23 ->
# 65 "lexer.mll"
             ( ASC )
# 590 "lexer.ml"

  | 24 ->
# 66 "lexer.mll"
             ( DESC )
# 595 "lexer.ml"

  | 25 ->
# 67 "lexer.mll"
             ( INSERT )
# 600 "lexer.ml"

  | 26 ->
# 68 "lexer.mll"
             ( INTO )
# 605 "lexer.ml"

  | 27 ->
# 69 "lexer.mll"
             ( VALUES )
# 610 "lexer.ml"

  | 28 ->
# 70 "lexer.mll"
             ( UPDATE )
# 615 "lexer.ml"

  | 29 ->
# 71 "lexer.mll"
             ( SET )
# 620 "lexer.ml"

  | 30 ->
# 72 "lexer.mll"
             ( DELETE )
# 625 "lexer.ml"

  | 31 ->
# 73 "lexer.mll"
             ( CREATE )
# 630 "lexer.ml"

  | 32 ->
# 74 "lexer.mll"
             ( TABLE )
# 635 "lexer.ml"

  | 33 ->
# 75 "lexer.mll"
             ( UNION )
# 640 "lexer.ml"

  | 34 ->
# 76 "lexer.mll"
             ( ALL )
# 645 "lexer.ml"

  | 35 ->
# 77 "lexer.mll"
             ( JOIN )
# 650 "lexer.ml"

  | 36 ->
# 78 "lexer.mll"
             ( ON )
# 655 "lexer.ml"

  | 37 ->
# 79 "lexer.mll"
             ( DOT )
# 660 "lexer.ml"

  | 38 ->
# 80 "lexer.mll"
             ( TINT )
# 665 "lexer.ml"

  | 39 ->
# 81 "lexer.mll"
             ( TFLOAT )
# 670 "lexer.ml"

  | 40 ->
# 82 "lexer.mll"
             ( TSTRING )
# 675 "lexer.ml"

  | 41 ->
# 83 "lexer.mll"
             ( TBOOL )
# 680 "lexer.ml"

  | 42 ->
# 84 "lexer.mll"
             ( TRUE )
# 685 "lexer.ml"

  | 43 ->
# 85 "lexer.mll"
             ( FALSE )
# 690 "lexer.ml"

  | 44 ->
# 86 "lexer.mll"
             ( ID (Lexing.lexeme lexbuf) )
# 695 "lexer.ml"

  | 45 ->
# 87 "lexer.mll"
             ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 700 "lexer.ml"

  | 46 ->
# 88 "lexer.mll"
             ( FLOAT (float_of_string (Lexing.lexeme lexbuf)) )
# 705 "lexer.ml"

  | 47 ->
let
# 89 "lexer.mll"
                    str
# 711 "lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 89 "lexer.mll"
                             ( STRING str )
# 715 "lexer.ml"

  | 48 ->
# 90 "lexer.mll"
                ( PLOT )
# 720 "lexer.ml"

  | 49 ->
# 91 "lexer.mll"
                ( SCATT )
# 725 "lexer.ml"

  | 50 ->
# 92 "lexer.mll"
                ( LINE )
# 730 "lexer.ml"

  | 51 ->
# 93 "lexer.mll"
                ( BAR )
# 735 "lexer.ml"

  | 52 ->
# 94 "lexer.mll"
                ( HISTOG )
# 740 "lexer.ml"

  | 53 ->
# 95 "lexer.mll"
      ( raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) )
# 745 "lexer.ml"

  | 54 ->
# 96 "lexer.mll"
                ( EOF )
# 750 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

