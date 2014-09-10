{
open Parser
open Types

let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = (String.length str) - sub_len
  and reg = Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then n else
      try
        let pos = Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0
 

}


let digit  = ['0'-'9']
let letter = ['A'-'Z''a'-'z']
let white  = [' ' '\t' '\r']
let new_line = ['\n']

rule lexer = parse
    "and"  { T_and }
  | "bool"    { T_bool(TYPE_bool) }
  | "break"    { T_break }
  | "case"     { T_case }
  | "char"  { T_char(TYPE_char) }
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
  | "int"   { T_int(TYPE_int) }
  | "MOD"   { T_MOD }
  | "NEXT"   { T_NEXT }
  | "not"   { T_not }
  | "or"   { T_or }
  | "PROC"   { T_PROC }
  | "PROGRAM"   { T_PROGRAM }
  | "REAL"   { T_REAL(TYPE_real) }
  | "return"   { T_return }
  | "STEP"   { T_STEP }
  | "switch"   { T_switch }
  | "TO"   { T_TO }
  | "true"   { T_true }
  | "while"   { T_while }
  | "WRITE"   { T_WRITE ("write")}
  | "WRITELN"   { T_WRITELN ("writeln")}
  | "WRITESP"   { T_WRITESP ("writesp")}
  | "WRITESPLN"   { T_WRITESPLN ("writespln")}

  | letter+(letter* digit* '_'*)* as name { T_name(name) }
  | (['1'-'9']+ digit*) | ('0'+) as value { T_int_const(value) }
  | digit+'.'digit+(('e'|'E')('+'|'-')? digit+)? as value { T_real_const(value) }
  | "'" ([^ '\'' '\"' '\\' ] | ("\\n" | "\\t" | "\\r" | "\\0" | "\\\'" | "\\\\" | "\\\""))  "'"  as value{ T_const_char(value) }
  | '"' ([^ '\'' '\"' '\\' ] | ("\\t" | "\\r" | "\\0" | "\\\'" | "\\\\" | "\\\"" | "\\n"))* '"' as value { T_string_const(value) }
  | "//" [^ '\n']* "\n"   { Lexing.new_line lexbuf ; lexer lexbuf }
  | "/*"(_|white|new_line)* "*/"  {for i = 1 to (count_substring (Lexing.lexeme lexbuf) "\n") do Lexing.new_line lexbuf done;
				    lexer lexbuf }

  | '=' as eq    
             { T_eq(Char.escaped eq) }
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
  | "%=" as eq    
             { T_eq(eq) }
  | "+="  as eq    
             { T_eq(eq) }
  | "-="   as eq    
             { T_eq(eq) }
  | "/="    as eq    
             { T_eq(eq) }
  | "*="     as eq    
             { T_eq(eq) }
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
  | new_line { Lexing.new_line lexbuf; lexer lexbuf }

  |  eof          { T_eof }
  |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d) in line %n\n"
                      chr (Char.code chr) (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum) ;
                    lexer lexbuf }

