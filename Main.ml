let rec optimize block_code =                                                   
    (* First optimization is allways immediate backward *)
    Optimizations.immediate_backward_propagation block_code;
 
    (* Constant Folding - No longer needed *)                                   
    (* Optimizations.constant_folding block_code; *)                            
                                                                                
    (* Unreachable simple deletions *)                                          
    CodeElimination.perform_deletions block_code;                               
                                                                                
    (* Simplify jumps *)                                                        
    Optimizations.jump_simplification block_code;                               
                                                                                
    (* Dummy elimination *)                                                     
    Optimizations.dummy_elimination block_code;                                 
                                                                                
    (* Convert to flowgraph for further optimizations *)                        
    let flowgraphs = ControlFlow.flowgraph_array_of_quads block_code in         
                                                                                
    (* Unreachable Code Elimination *)                                          
    CodeElimination.delete_unreachable_blocks flowgraphs;                       
                                                                                
    (* Copy Propagation *)                                                      
    Array.iter CopyPropagation.copy_propagation flowgraphs;                     
                                                                                
    (* Convert back *)                                                          
    let block_code = ControlFlow.convert_back_to_quads flowgraphs in            
                                                                                
    (* Dummy Elimination again to lighten code after eliminations *)            
    Optimizations.dummy_elimination block_code;                                 
                                                                                
    block_code                                                             

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    (*
    let quad_list = List.rev (Parser.pmodule Lexer.lexer lexbuf) in
    let optimized_quads = dummy_optimize quad_list in
    *)
    let quad_list = List.rev(Parser.pmodule Lexer.lexer lexbuf) in
    ignore(List.map print_string (List.map Quads.string_of_quad_t quad_list));
    let block_code = Blocks.blocks_of_quad_t_list quad_list in
    let opt_code = optimize block_code in
    let final_list = MergeBlocks.make_list opt_code in
    FinalCode.print_final_code stdout final_list;
    ignore(List.map print_string (List.map Quads.string_of_quad_t final_list));
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1  
