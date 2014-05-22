let optimize quad_l = quad_l

let print_final_code file_d code = Printf.fprintf file_d "Final code is coming soon...\n"

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let quad_list = Parser.pmodule Lexer.lexer lexbuf; in
    let optimized_quads = optimize quad_list in
    print_final_code stdout optimized_quads;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1  

