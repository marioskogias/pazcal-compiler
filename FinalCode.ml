open FinalSupport
open Symbol
open QuadTypes
open Error

(* this it the global bp. Go there to access global data *)
let global_bp = ref 0

(* Implementation of basic helper functions *)

(* Function to check if an entry is local to a function *)
(* Entries can either be local or global and we have no nested functions *)
let local e = 
    match e.entry_scope.sco_parent with
    | None -> false
    | _ -> true

let get_info = function 
  |ENTRY_variable (info) ->
    let offset = info.variable_offset in
    (offset, false)
  |ENTRY_temporary (info) ->
    let offset = info.temporary_offset in
    (offset, false)
  |ENTRY_parameter (info) ->
    let offset = info.parameter_offset in
    let mode = (info.parameter_mode = PASS_BY_REFERENCE)
    in (offset,mode)

(* get_AR function to get bp for non local data -> global data *)
let get_ar = [ Mov (Register Si, Num (string_of_int !global_bp)) ]

(* Update links but no nested functions so np = nx *)
let update_al = 
    [Push (Register Ax); Mov (Register Ax, Mem_loc ("word", Bp, 4))]

(* load a to reg *)
let rec load a reg = 
    match a with
    |Quad_int(str) -> [Mov (Register reg, Num str)]
    |Quad_char(str) -> let asci = string_of_int (Char.code str.[0]) in
        [Mov (Register reg, Num asci)]
    (*Missing boolean*)
    |Quad_entry(e) ->( let l = local e in
        let (offset, is_reference) = get_info e.entry_info in
        match (l, is_reference) with
        |(true, false) -> [Mov (Register reg, Mem_loc("word", Bp, offset))]
        |(true, true) ->  [Mov (Register reg, Mem_loc("word", Si, 0)); 
            Mov (Register Si, Mem_loc("word", Bp, offset))]
        |(false,_) -> let ar = get_ar in 
            (Mov(Register reg, Mem_loc("word", Si,offset))::ar)
    )
    |Quad_valof(e) -> let l = load (Quad_entry(e)) Di in
        ((Mov(Register reg, Mem_loc("word", Di, 0)))::l)
    (* {x}? *)

(* load address in reg *)
let load_addr addr reg = 
    match addr with
    |Quad_string(s) -> [] (* missing constant string handle *)
    |Quad_valof(ent) -> load (Quad_entry(ent)) reg
    |Quad_entry(e) ->( let l = local e in
        let (offset, is_reference) = get_info e.entry_info in
        match (l, is_reference) with
        |(true, false) -> [Lea (Register reg, Mem_loc("word", Bp, offset))] 
        |(true, true) ->  [Mov (Register reg, Mem_loc("word", Bp, offset))] 
        |(false,_) -> let ar = get_ar in 
            (Mov(Register reg, Mem_loc("word", Si,offset))::ar)
    )

(* store from reg to mem *)
let store a reg = 
    match a with
    |Quad_entry(e) ->( let l = local e in
        let (offset, is_reference) = get_info e.entry_info in
        match (l, is_reference) with
        |(true, false) -> [Mov (Mem_loc("word", Bp, offset), Register reg)] 
        |(true, true) ->  [Mov (Mem_loc("word", Si, 0), Register reg); 
                            Mov(Register Si, Mem_loc("word", Bp, offset))] 
        |(false,_) -> let ar = get_ar in 
            (Mov(Mem_loc("word", Si,offset), Register reg)::ar)
    )
    |Quad_valof(e) -> let l = load (Quad_entry(e)) Di in
        (Mov(Mem_loc("word", Di, 0), Register reg)::l)

(* function labels follow _p_num naming format *)
let func_labels = Hashtbl.create 30
let func_count = ref 0  

(* get or set function label *)
let name n = 
    try
        Hashtbl.find func_labels n
    with
        Not_found -> 
            incr(func_count);
            let l = Printf.sprintf "_%s_%d" n !func_count in
            Hashtbl.add func_labels n l;
            l

(* end of routine label *)
let endof n = 
    let l = Hashtbl.find func_labels n in
    Printf.sprintf "@%s" l

(* label help routine 
 with a given number return @<number> else 
 produce the next label
 *)
let quad_count = ref 0
let label = function
    |Some a -> Printf.sprintf "@%d" a
    |None ->incr (quad_count);
    Printf.sprintf "@%d" !quad_count

(* Start code *)
let start_code program_label= 
  let start = Printf.sprintf "\
  xseg\tsegment\tpublic 'code'\n\
  \tassume\tcs : xseg, ds : xseg, ss : xseg\n\
  \torg\t100h\n\
  main\tproc\tnear\n\
  \tcall\tnear ptr %s\n\
  \tmov\tax, 4C00h\n\
  \tint\t21h\n\
  main endp\n"
    program_label
  in [(Start start)]


(* End code *)
let end_code = [End "xseg ends\n\tend  main\n"]

let rec merge_lists = function
    |(l, []) -> l
    |(l1, ((h1::tail1)::tail)) -> merge_lists((h1::l1), (tail1::tail))
    |(l1, ([]::tail)) -> merge_lists(l1, tail)

let final_code_of_quad = function 
    |Quad_set(q,e) -> merge_lists([], [ store e Ax ;load q Ax ])
    |Quad_array(x,y,z) ->
        let lval = match x with
            |Quad_entry x -> x
            |_ -> internal "Error"; raise Terminate
        in let size = 2 
        in let code = [store (Quad_entry(z)) Ax; 
                      [Add (Action_reg Ax, Action_reg Cx )];
                      load_addr x Cx;
                      [IMul Cx]; 
                      [Mov(Register Cx, Num(string_of_int size))]]
        in merge_lists([], code)
    |Quad_calc(op,x,y,z) ->(
        match op with
        |"+"-> let code = [store z Ax;
                          [Add (Action_reg Ax, Action_reg Dx)];
                          load y Dx;
                          load x Ax]
              in merge_lists([], code)
        |"+"-> let code = [store z Ax;
                          [Sub (Action_reg Ax, Action_reg Dx)];
                          load y Dx;
                          load x Ax]
              in merge_lists([], code)
        |"*"-> let code = [store z Ax;
                          [IMul Cx];
                          load y Cx;
                          load x Ax]
              in merge_lists([], code)
        |"*"-> let code = [store z Ax;
                          [IDiv Cx];
                          load y Cx;
                          [Cwd];
                          load x Ax]
              in merge_lists([], code)
        |"%"-> let code = [store z Dx;
                          [IDiv Cx];
                          load y Cx;
                          [Cwd];
                          load x Ax]
              in merge_lists([], code)
       |_ -> internal "No operator"; raise Terminate 
    )
    |Quad_cond(op,x,y,l) -> 
        let jump_kind = match op with
                        |"==" -> "je"
                        |"!=" -> "jne"
                        |"<=" -> "jle"
                        |"<" -> "jl"
                        |">=" -> "jge"
                        | ">" -> "jg"
                        |_ -> internal "Not a comparator"; 
                              raise Terminate
        in let code = [[Cond_jump(jump_kind, label (Some(!l)))];
             [Cmp(Ax, Dx)];load y Dx;load x Ax] 
        in merge_lists([], code)
    |Quad_jump(l) -> [Jump(label (Some(!l)))]
    |Quad_unit(x) -> []
    |_ -> []
    
let rec create_assembly = function
    | ([], assembly_list) -> assembly_list
    | (a::quad_list, assembly_list) -> 
            let assembly_so_far = (final_code_of_quad a)@ assembly_list
            in create_assembly (quad_list, assembly_so_far)

let rec print_final_code file_d code =
    let rec print_help d = function
        | (h::tail) -> Printf.fprintf d "%s" h; print_help d tail 
        | [] -> () 
    in 
    let assembly = end_code in
    let assembly_list = create_assembly(code, assembly) in
    let final_assembly = merge_lists(assembly_list, [start_code "main_prog"]) in 
    let assembly_string = (List.map string_of_final_t final_assembly) in
    Printf.fprintf file_d "\n\n\n\nFinal code is \n";
    print_help file_d assembly_string 
