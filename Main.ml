let dummy_optimize q_list = q_list 

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let quad_list = Parser.pmodule Lexer.lexer lexbuf; in
    let optimized_quads = dummy_optimize quad_list in
    FinalCode.print_final_code stdout optimized_quads;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1  

