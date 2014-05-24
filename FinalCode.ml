open FinalSupport
open Symbol
open QuadTypes

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

(* label help routine *)
let quad_count = ref 0
let label = incr (quad_count);
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
  in (Start start)

(* End code *)
let end_code = End "xseg ends\n\tend  main\n"
let final_code_of_quad = function 
    |_ -> [start_code "test"]
    
let rec create_assembly = function
    | ([], assembly_list) -> assembly_list
    | (a::quad_list, assembly_list) -> create_assembly (quad_list, assembly_list@(final_code_of_quad a))

let rec print_final_code file_d code =
    let rec print_help d = function
        | (h::tail) -> Printf.fprintf d "%s" h; print_help d tail 
        | [] -> () 
    in 
    let assembly_list = create_assembly(code, []) in
    let assembly_string = (List.map string_of_final_t assembly_list) in
    print_help file_d assembly_string ; 
    Printf.fprintf file_d "Final code is coming soon...\n"
