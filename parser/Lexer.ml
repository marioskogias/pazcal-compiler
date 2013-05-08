# 1 "Lexer.mll"
 
open Parser

# 6 "Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\236\255\237\255\001\000\005\000\240\255\241\255\242\255\
    \243\255\244\255\245\255\246\255\014\000\019\000\022\000\015\000\
    \025\000\016\000\017\000\028\000\016\000\026\000\022\000\017\000\
    \255\255\018\000\254\255\021\000\253\255\252\255\033\000\032\000\
    \028\000\251\255\039\000\250\255\249\255\039\000\031\000\248\255\
    \238\255\002\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\019\000\016\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\041\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\000\000\000\000\255\255\255\255\000\000\
    \000\000\041\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\004\000\040\000\040\000\004\000\004\000\004\000\
    \000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\000\000\000\000\000\000\000\000\004\000\000\000\003\000\
    \009\000\008\000\005\000\007\000\000\000\006\000\000\000\000\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\000\000\000\000\000\000\010\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\016\000\011\000\017\000\015\000\018\000\011\000\
    \011\000\014\000\011\000\011\000\019\000\011\000\011\000\011\000\
    \020\000\011\000\011\000\011\000\013\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\037\000\036\000\034\000\030\000\029\000\
    \027\000\025\000\021\000\022\000\023\000\024\000\026\000\028\000\
    \031\000\032\000\033\000\035\000\038\000\039\000\000\000\000\000\
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
    \002\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\041\000\000\000\004\000\004\000\
    \255\255\255\255\004\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\004\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\000\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\014\000\015\000\016\000\017\000\
    \018\000\019\000\020\000\021\000\022\000\023\000\025\000\027\000\
    \030\000\031\000\032\000\034\000\037\000\038\000\255\255\255\255\
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
    \000\000\003\000\041\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
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

let rec lexer lexbuf =
    __ocaml_lex_lexer_rec lexbuf 0
and __ocaml_lex_lexer_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 10 "Lexer.mll"
             ( T_print )
# 128 "Lexer.ml"

  | 1 ->
# 11 "Lexer.mll"
             ( T_let )
# 133 "Lexer.ml"

  | 2 ->
# 12 "Lexer.mll"
             ( T_for )
# 138 "Lexer.ml"

  | 3 ->
# 13 "Lexer.mll"
             ( T_do )
# 143 "Lexer.ml"

  | 4 ->
# 14 "Lexer.mll"
             ( T_begin )
# 148 "Lexer.ml"

  | 5 ->
# 15 "Lexer.mll"
             ( T_end )
# 153 "Lexer.ml"

  | 6 ->
# 16 "Lexer.mll"
             ( T_if )
# 158 "Lexer.ml"

  | 7 ->
# 17 "Lexer.mll"
             ( T_then )
# 163 "Lexer.ml"

  | 8 ->
# 19 "Lexer.mll"
             ( T_const )
# 168 "Lexer.ml"

  | 9 ->
# 20 "Lexer.mll"
             ( T_var )
# 173 "Lexer.ml"

  | 10 ->
# 22 "Lexer.mll"
             ( T_eq )
# 178 "Lexer.ml"

  | 11 ->
# 23 "Lexer.mll"
             ( T_lparen )
# 183 "Lexer.ml"

  | 12 ->
# 24 "Lexer.mll"
             ( T_rparen )
# 188 "Lexer.ml"

  | 13 ->
# 25 "Lexer.mll"
             ( T_plus )
# 193 "Lexer.ml"

  | 14 ->
# 26 "Lexer.mll"
             ( T_minus )
# 198 "Lexer.ml"

  | 15 ->
# 27 "Lexer.mll"
             ( T_times )
# 203 "Lexer.ml"

  | 16 ->
# 29 "Lexer.mll"
                         ( lexer lexbuf )
# 208 "Lexer.ml"

  | 17 ->
# 30 "Lexer.mll"
                         ( lexer lexbuf )
# 213 "Lexer.ml"

  | 18 ->
# 32 "Lexer.mll"
                  ( T_eof )
# 218 "Lexer.ml"

  | 19 ->
let
# 33 "Lexer.mll"
          chr
# 224 "Lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 33 "Lexer.mll"
                  ( Printf.eprintf "invalid character: '%c' (ascii: %d)"
                      chr (Char.code chr);
                    lexer lexbuf )
# 230 "Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_lexer_rec lexbuf __ocaml_lex_state

;;

