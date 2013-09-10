type token =
  | T_and
  | T_bool of (Types.typ)
  | T_break
  | T_case
  | T_char of (Types.typ)
  | T_const
  | T_continue
  | T_default
  | T_do
  | T_DOWNTO
  | T_else
  | T_false
  | T_FOR
  | T_FORM
  | T_FUNC
  | T_if
  | T_int of (Types.typ)
  | T_MOD
  | T_NEXT
  | T_not
  | T_or
  | T_PROC
  | T_PROGRAM
  | T_REAL of (Types.typ)
  | T_return
  | T_STEP
  | T_switch
  | T_TO
  | T_true
  | T_while
  | T_WRITE
  | T_WRITELN
  | T_WRITESP
  | T_WRITESPLN
  | T_eq
  | T_lparen
  | T_rparen
  | T_plus
  | T_minus
  | T_times
  | T_equal
  | T_greater
  | T_less
  | T_less_equal
  | T_greater_equal
  | T_not_equal
  | T_mod
  | T_mod_equal
  | T_plus_equal
  | T_minus_equal
  | T_div_equal
  | T_times_equal
  | T_minus_minus
  | T_plus_plus
  | T_OR
  | T_AND
  | T_NOT
  | T_div
  | T_ampersand
  | T_semicolon
  | T_fullstop
  | T_colon
  | T_comma
  | T_lbracket
  | T_rbracket
  | T_lbrace
  | T_rbrace
  | T_name of (string)
  | T_real_const
  | T_const_char
  | T_string_const
  | T_int_const of (string)
  | T_eof

val pmodule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
