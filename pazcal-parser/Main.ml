let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.pmodule Lexer.lexer lexbuf;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1


