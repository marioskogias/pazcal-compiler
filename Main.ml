let dummy_optimize q_list = q_list 

let rec absolute_jumps acc idx = function
    | h::t -> 
      begin
        match h with
          | QuadTypes.Quad_jump (x)
          | QuadTypes.Quad_cond (_, _, _, x)
          -> x := !x + idx; absolute_jumps (acc@[h]) (idx+1) t
          | _ -> absolute_jumps (acc@[h]) (idx+1) t
      end
    | [h] -> 
      begin
        match h with
          | QuadTypes.Quad_jump (x)
          | QuadTypes.Quad_cond (_, _, _, x)
          -> x := !x + idx; acc@[h]
          | _ -> acc@[h]
      end
    | [] -> acc

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let quad_list = List.rev(Parser.pmodule Lexer.lexer lexbuf) in
    let abs_jumps_quad_list = absolute_jumps [] 0 quad_list in
    let optimized_quads = dummy_optimize abs_jumps_quad_list in
    ignore(List.map print_string (List.map Quads.string_of_quad_t abs_jumps_quad_list));
    FinalCode.print_final_code stdout optimized_quads;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1  

