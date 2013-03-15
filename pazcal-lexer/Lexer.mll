{
type token =
  | T_eof | T_const | T_var 
  | T_print | T_let | T_for | T_do | T_begin | T_end | T_if | T_then
  | T_eq | T_lparen | T_rparen | T_plus | T_minus | T_times
}

let digit  = ['0'-'9']
let letter = ['a'-'z']
let white  = [' ' '\t' '\r' '\n']



rule lexer = parse
    "and"  { T_and }
  | "bool"    { T_bool }
  | "break"    { T_break }
  | "case"     { T_case }
  | "char"  { T_char }
  | "const"    { T_const }
  | "continue"     { T_continue }
  | "default"   { T_default }
  | "do"   { T_do }
  | "DOWNTO"   { T_DOWNTO }
  | "else"   { T_else }
  | "false"   { T_false }
  | "FOR"   { T_FOR }
  | "FORM"   { T_FORM }
  | "FUNC"   { T_FUNC }
  | "if"   { T_if }
  | "int"   { T_int }
  | "MOD"   { T_MOD }
  | "NEXT"   { T_NEXT }
  | "not"   { T_not }
  | "or"   { T_or }
  | "PROC"   { T_PROC }
  | "PROGRAM"   { T_PROGRAM }
  | "REAL"   { T_REAL }
  | "return"   { T_return }
  | "STEP"   { T_STEP }
  | "switch"   { T_switch }
  | "TO"   { T_TO }
  | "true"   { T_true }
  | "while"   { T_while }
  | "WRITE"   { T_WRITE }
  | "WRITELN"   { T_WRITELN }
  | "WRITESP"   { T_WRITESP }
  | "WRITESPLN"   { T_WRITESPLN }

  | digit+   { T_const }
  | letter   { T_var }

  | '='      { T_eq }
  | '('      { T_lparen }
  | ')'      { T_rparen }
  | '+'      { T_plus }
  | '-'      { T_minus }
  | '*'      { T_times }

  | white+               { lexer lexbuf }
  | "'" [^ '\n']* "\n"   { lexer lexbuf }

  |  eof          { T_eof }
  |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d)"
                      chr (Char.code chr);
                    lexer lexbuf }

{
  let string_of_token token =
    match token with
      | T_eof    -> "T_eof"
      | T_const  -> "T_const"
      | T_var    -> "T_var"
      | T_print  -> "T_print"
      | T_let    -> "T_let"
      | T_for    -> "T_for"
      | T_do     -> "T_do"
      | T_begin  -> "T_begin"
      | T_end    -> "T_end"
      | T_if     -> "T_if"
      | T_then   -> "T_then"
      | T_eq     -> "T_eq"
      | T_lparen -> "T_lparen"
      | T_rparen -> "T_rparen"
      | T_plus   -> "T_plus"
      | T_minus  -> "T_minus"
      | T_times  -> "T_times"

  let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
        (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}
