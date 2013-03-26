{
type token =
  | T_eof | T_name | T_int_const | T_real_const | T_const_char | T_string_const 
  | T_and | T_bool | T_break | T_case | T_char | T_const | T_continue | T_default | T_do | T_DOWNTO 
  | T_else | T_false | T_FOR | T_FORM | T_FUNC| T_if | T_int | T_MOD | T_NEXT | T_not | T_or | T_PROC 
  | T_PROGRAM | T_REAL | T_return | T_STEP | T_switch | T_TO | T_true | T_while | T_WRITE | T_WRITELN 
  | T_WRITESP | T_WRITESPLN
  | T_eq | T_lparen | T_rparen | T_plus | T_minus | T_times
  | T_equal | T_greater | T_less | T_less_equal 
  | T_greater_equal | T_not_equal | T_mod | T_mod_equal | T_plus_equal | T_minus_equal | T_div_equal 
  | T_minus_minus | T_plus_plus | T_OR | T_AND | T_NOT | T_div | T_ampersand | T_semicolon | T_fullstop 
  | T_colon| T_comma| T_lbracket| T_rbracket | T_lbrace| T_rbrace

}

let digit  = ['0'-'9']
let letter = ['A'-'Z''a'-'z']
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

  | letter+(letter* digit* '_'*)* { T_name }
  | (['1'-'9']+ digit*) | ('0'+)	{ T_int_const }
  | digit+'.'digit+(('e'|'E')('+'|'-')? digit+)?	{ T_real_const }
  | "'" ([^ '\'' '\"' '\\' ] | ("\\n" | "\\t" | "\\r" | "\\0" | "\\\'" | "\\\\" | "\\\""))  "'"   { T_const_char }
  | '"' ([^ '\'' '\"' '\\' ] | ("\\t" | "\\r" | "\\0" | "\\\'" | "\\\\" | "\\\""))* "\\n"? '"' { T_string_const }
  | "//" [^ '\n']* "\n"   { lexer lexbuf }
  | "/*"(_|white)* "*/"  { lexer lexbuf }

  | '='      { T_eq }
  | '('      { T_lparen }
  | ')'      { T_rparen }
  | '+'      { T_plus }
  | '-'      { T_minus }
  | '*'      { T_times }
  | "=="     { T_equal }
  | '>'      { T_greater }
  | '<'      { T_less }
  | "<="     { T_less_equal }
  | ">="     { T_greater_equal }
  | "!="     { T_not_equal }
  | '%'      { T_mod }
  | "%="     { T_mod_equal }
  | "+="     { T_plus_equal }
  | "-="     { T_minus_equal }
  | "/="     { T_div_equal }
  | "--"     { T_minus_minus }
  | "++"     { T_plus_plus }
  | "||"     { T_OR }
  | "&&"     { T_AND }
  | '!'      { T_NOT }
  | '/'      { T_div }
  | '&'      { T_ampersand }
  | ';'      { T_semicolon }
  | '.'      { T_fullstop }
  | ':'      { T_colon}
  | ','      { T_comma}
  | '['      { T_lbracket}
  | ']'      { T_rbracket }
  | '{'      { T_lbrace}
  | '}'      { T_rbrace }
  | white+   { lexer lexbuf }

  |  eof          { T_eof }
  |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d)\n"
                      chr (Char.code chr);
                    lexer lexbuf }

{
  let string_of_token token =
    match token with
	| T_eof    -> "T_eof"
	| T_name    -> "T_name"
	| T_int_const -> "T_int_const"
	| T_real_const -> "T_real_const"
	| T_const_char -> "T_const_char"
	| T_string_const -> "T_string_const"
	| T_and -> "T_and"
	| T_bool -> "T_bool"
	| T_break -> "T_break"
	| T_case -> "T_case"
	| T_char -> "T_char"
	| T_const -> "T_const"
	| T_continue -> "T_continue"
	| T_default -> "T_default"
	| T_do -> "T_do"
	| T_DOWNTO -> "T_DOWNTO"
	| T_else -> "T_else"
	| T_false -> "T_false"
	| T_FOR -> "T_FOR"
	| T_FORM -> "T_FORM"
	| T_FUNC -> "T_FUNC"
	| T_if -> "T_if"
	| T_int -> "T_int"
	| T_MOD -> "T_MOD"
	| T_NEXT -> "T_NEXT"
	| T_not -> "T_not"
	| T_or -> "T_or"
	| T_PROC -> "T_PROC"
	| T_PROGRAM -> "T_PROGRAM"
	| T_REAL -> "T_REAL"
	| T_return -> "T_return"
	| T_STEP -> "T_STEP"
	| T_switch -> "T_switch"
	| T_TO -> "T_TO"
	| T_true -> "T_true"
	| T_while -> "T_while"
	| T_WRITE -> "T_WRITE"
	| T_WRITELN -> "T_WRITELN"
	| T_WRITESP -> "T_WRITESP"
	| T_WRITESPLN -> "T_WRITESPLN"
	| T_eq -> "T_eq"
	| T_lparen -> "T_lparen" 
	| T_rparen -> "T_rparen"
	| T_plus -> "T_plus"
	| T_minus -> "T_minus"
	| T_times -> "T_times"
	| T_equal -> "T_equal"
	| T_greater -> "T_greater"
	| T_less -> "T_less"
	| T_less_equal -> "T_less_equal"
	| T_greater_equal -> "T_greater_equal"
	| T_not_equal -> "T_not_equal"
	| T_mod -> "T_mod"
	| T_mod_equal -> "T_mod_equal"
	| T_plus_equal -> "T_plus_equal"
	| T_minus_equal -> "T_minus_equal"
	| T_div_equal -> "T_div_equal"
	| T_minus_minus -> "T_minus_minus"
	| T_plus_plus -> "T_plus_plus"
	| T_OR -> "T_OR"
	| T_AND -> "T_AND"
	| T_NOT -> "T_NOT"
	| T_div -> "T_div"
	| T_ampersand -> "T_ampersand"
	| T_semicolon -> "T_semicolon"
	| T_fullstop -> "T_fullstop"
	| T_colon -> "T_colon"
	| T_comma -> "T_comma"
	| T_lbracket -> "T_lbracket"
	| T_rbracket -> "T_rbracket"
	| T_lbrace -> "T_lbrace"
	| T_rbrace -> "T_rbrace"
  let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
        (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}
