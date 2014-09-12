let rec optimize block_code =                                                   
    (* First optimization is allways immediate backward *)
    Optimizations.immediate_backward_propagation block_code;
                                                                                
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

let get_name name = 
    let point_index = String.index name '.' in
    String.sub name 0 point_index

let should_optimize = ref false
let fflag = ref false
let iflag = ref false
let in_file = ref ""
let usage = "This is th usage:"
let anon_fun name = in_file := name

let speclist = Arg.align [
    "-o", Arg.Unit (function () -> should_optimize := true)
        , "Optimize intermediate code";
    "-f", Arg.Unit (function () -> fflag := true)
    , "Input stdin, stdout: final code";
    "-i", Arg.Unit (function () -> iflag := true)
    , "Input stdin, stdout: intermediate code"]

let main =
  Arg.parse speclist anon_fun usage;
  let inbuffer = if (!fflag || !iflag) then (in_file := "a.pas"; stdin) 
                 else open_in !in_file in
  let name = get_name !in_file in 
  let asm_name = String.concat "" [name; ".asm"] in
  let quads_name = String.concat "" [name; ".imm"] in
  let outass = if (!fflag) then stdout else open_out asm_name in
  let outquads = if (!iflag) then stdout else open_out quads_name  in
  let lexbuf = Lexing.from_channel inbuffer in
  try
    let quad_list = List.rev(Parser.pmodule Lexer.lexer lexbuf) in

    (*Substitute constant values*)
    let no_constants = ConstantProp.constant_optimize quad_list in
    
    (*either optimize or not need block code for jumps*)
    let block_code = Blocks.blocks_of_quad_t_list no_constants in
    let opt_code = if (!should_optimize) then optimize block_code 
                     else block_code in
    let final_list = MergeBlocks.make_list opt_code in
    
    (*create assembly code*)
    let assembly_code = FinalCode.create_final_code final_list in
    
    (*Ouput assembly*)
    FinalCode.print_assembly outass assembly_code;
    
    (*Ouput quads*)
    Quads.print_quads outquads final_list;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d \n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ;
    exit 1
