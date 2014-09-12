open FinalSupport
open Symbol
open QuadTypes
open Error
open Identifier
open Types
open Quads
open CharString

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
  |_ -> internal "Info for not a var"; raise Terminate

let const_strings = Queue.create()
let add_string str = Queue.add str const_strings;
                     let serial = (Queue.length const_strings) in
                     Printf.sprintf "@str%d" serial

(* Strings come with quotes at the beg and end so remove them *)
let strip str = 
  let size = String.length str in
    String.sub str 1 (size-2)

(* get_AR function to get bp for non local data -> global data *)
(* we have no nested funtions so ncur = na + 1 where na global scope *)
let get_ar = [ Mov (Register Si, Mem_loc("word", Bp, 4)) ]

(* Update links but no nested functions so np = nx *)
let update_al = 
    [Push (Register Ax); Mov (Register Ax, Mem_loc ("word", Bp, 4))]

(* load a to reg *)
let rec load a reg = 
  let clear_reg = [Mov(Register reg, Num "0")] in
  let code = 
    match a with
      |Quad_int(str) -> [Mov (Register reg, Num str)]
      |Quad_char(str) -> let asci =
         match (String.length str) with
           |3 -> string_of_int (Char.code str.[1]) 
           |4 -> special_char_asci str.[2]
           |_ -> internal "Not a char"; raise Terminate
       in 
         [Mov (Register get_register(reg, TYPE_char), Num asci)]
      |Quad_bool(str) -> (match str with
                            |"true" -> [Mov (Register get_register(reg, TYPE_bool), Num "1")]
                            |"false" -> [Mov (Register get_register(reg, TYPE_bool), Num "0")]
                            |_ -> internal "Unknown bool value"; raise Terminate
       )
      |Quad_entry(e) ->( let l = local e in
                         let (offset, is_reference) = get_info e.entry_info in
                         let entry_type = get_entry_type e in
                         let length = mem_size entry_type in 
                           match (l, is_reference) with
                             |(true, false) -> [Mov (Register get_register(reg, entry_type), Mem_loc(length, Bp, offset))]
                             |(true, true) ->  [Mov (Register get_register(reg, entry_type), Mem_loc(length, Si, 0)); 
                                                Mov (Register Si, Mem_loc("word", Bp, offset))]
                             |(false,_) -> let ar = get_ar in 
                                (Mov(Register get_register(reg, entry_type), Mem_loc(length, Si,offset))::ar)
       )
      |Quad_valof(e) -> 
          let entry_type = get_entry_type e in
          let length = mem_size entry_type in 
          let l = load (Quad_entry(e)) Di in
            ((Mov(Register get_register(reg, entry_type), Mem_loc(length, Di, 0)))::l)
      |_ -> internal "Load from sth unloadable"; raise Terminate
  in code@clear_reg
(* {x}? *)

(* load address in reg *)
let load_addr addr reg =  
  match addr with
    |Quad_string(s) ->let addr = add_string (strip  s) in
       [Lea (Register reg, String_addr addr)]
    |Quad_valof(ent) -> load (Quad_entry(ent)) reg
    |Quad_entry(e) ->( let l = local e in
                       let (offset, is_reference) = get_info e.entry_info in
                       let entry_type = get_entry_type e in
                       let length = mem_size entry_type in
                         match (l, is_reference) with
                           |(true, false) -> [Lea (Register reg, Mem_loc(length, Bp, offset))] 
                           |(true, true) ->  [Mov (Register reg, Mem_loc("word", Bp, offset))] 
                           |(false,_) -> let ar = get_ar in 
                              (Mov(Register reg, Mem_loc("word", Si,offset))::ar)
     )
    |_ -> internal "Not an address to load"; raise Terminate

(* store from reg to mem *)
let store a reg = 
    match a with
    |Quad_entry(e) ->( let l = local e in
        let (offset, is_reference) = get_info e.entry_info in
        let entry_type = get_entry_type e in
        let length = mem_size entry_type in 
        match (l, is_reference) with
        |(true, false) -> [Mov (Mem_loc(length, Bp, offset), Register get_register(reg, entry_type))] 
        |(true, true) ->  [Mov (Mem_loc(length, Si, 0), Register get_register(reg, entry_type)); 
                            Mov(Register Si, Mem_loc("word", Bp, offset))] 
        |(false,_) -> let ar = get_ar in 
            (Mov(Mem_loc(length, Si,offset), Register get_register(reg, entry_type))::ar)
    )
    |Quad_valof(e) -> let l = load (Quad_entry(e)) Di in
        let entry_type = get_entry_type e in
        let length = mem_size entry_type in 
        (Mov(Mem_loc(length, Di, 0), Register get_register(reg, entry_type))::l)
    |_ -> internal "Can not store to non entry"; raise Terminate

(* function labels follow _p_num naming format *)
let func_labels = Hashtbl.create 30
let func_count = ref 0  

(* get or set function label the input is a string *)
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
let quad_count = ref (-1)
let label = function
    |Some a -> Printf.sprintf "@%d" a
    |None ->incr (quad_count);
    Printf.sprintf "@%d:\n" !quad_count

(* functions to register lib functions *)
let lib_funcs = ["putchar";"puts";
                 "WRITE_INT";"WRITE_CHAR";
                 "WRITE_BOOL";"WRITE_STRING";
                 "READ_INT";"READ_BOOL";"getchar";
                 "READ_STRING";
                 "strlen";"strcmp";"strcpy";"strcat"]

let rec register_lib_functions = function
  |[] -> ()
  |(h::t) ->  
      let help_reg k v = Hashtbl.add func_labels k v in
      match h with
     |"putchar" -> ignore(help_reg "putchar" "_print_s_char"); register_lib_functions t
     |"puts" -> ignore(help_reg "puts" "_print_string"); register_lib_functions t
     |"READ_INT" -> ignore(help_reg "READ_INT" "_read_int"); register_lib_functions t
     |"READ_BOOL" -> ignore(help_reg "READ_BOOL" "_read_bool"); register_lib_functions t
     |"getchar" -> ignore(help_reg "getchar" "_read_char"); register_lib_functions t
     |"WRITE_INT" -> ignore(help_reg "WRITE_INT" "_print_int"); register_lib_functions t
     |"WRITE_CHAR" -> ignore(help_reg "WRITE_CHAR" "_print_char"); register_lib_functions t
     |"WRITE_BOOL" -> ignore(help_reg "WRITE_BOOL" "_print_bool"); register_lib_functions t
     |"WRITE_STRING" -> ignore(help_reg "WRITE_STRING" "_write_string"); register_lib_functions t
     |"READ_STRING" -> ignore(help_reg "READ_STRING" "_readString"); register_lib_functions t
     |"strlen" -> ignore(help_reg "strlen" "_strlen"); register_lib_functions t
     |"strcmp" -> ignore(help_reg "strcmp" "_strcmp"); register_lib_functions t
     |"strcpy" -> ignore(help_reg "strcpy" "_strcpy"); register_lib_functions t
     |"strcat" -> ignore(help_reg "strcat" "_strcat"); register_lib_functions t
     |_ -> ignore(help_reg h h); register_lib_functions t

let declare_lib_functions () = 
    let command k = Printf.sprintf "\textrn %s : proc\n" (name k) in
    let rec help_lib l = function
      |[] -> l
      |(h::t) -> let lib = command h in
            help_lib (lib^l) t
    in help_lib "" lib_funcs

(*create a dummy ar to begin with and register the globals*)
let register_globals size = 
    let code = Printf.sprintf "\
    \tmov BP, SP\n\tsub BP, 4\n\
    \tpush BP\n\tsub SP, 2\n\
    \tmov BP, SP\n\tsub SP, %d\n" size
    in code

(* Start code *)
let start_code program_label global_size = 
  let start = Printf.sprintf "\
  xseg\tsegment\tpublic 'code'\n\
  \tassume\tcs : xseg, ds : xseg, ss : xseg\n\
  \torg\t100h\n\
  %s\nmain\tproc\tnear\n\
  %s\tcall\tnear ptr %s\n\
  \tmov\tax, 4C00h\n\
  \tint\t21h\n\
  main endp\n"
  (declare_lib_functions ())
   (register_globals global_size) program_label
  in [(Start start)]


(* End code *)
let end_code = [End "xseg ends\n\tend  main\n"]

let rec merge_lists = function
    |(l, []) -> l
    |(l1, ((h1::tail1)::tail)) -> merge_lists((h1::l1), (tail1::tail))
    |(l1, ([]::tail)) -> merge_lists(l1, tail)

let param_size x = 
        let size = match x.entry_info with
          | ENTRY_function (info) -> - info.function_scope.sco_negofs
          | _ -> internal "Function not a function"; raise Terminate
        in size

let current_fun = ref "fun_name"

let final_code_of_quad = function 
  |Quad_set(q,e) -> merge_lists([], [ store e Ax ;load q Ax ])
  |Quad_array(x,y,z) ->
      let lval = match x with
        |Quad_entry x -> x
        |_ -> internal "Error"; raise Terminate
      in let size = 
        match lval.entry_info with
          |ENTRY_variable(info)-> sizeOfArrayType info.variable_type
          |ENTRY_parameter(info) ->sizeOfArrayType info.parameter_type
          |_ -> internal "Called array with not an array"; raise Terminate
      in let code = [store (Quad_entry(z)) Ax; 
                     [Add (Action_reg Ax, Action_reg Cx )];
                     load_addr x Cx;
                     [IMul Cx]; 
                     [Mov(Register Cx, Num(string_of_int size))];
                     load y Ax]
      in merge_lists([], code)
  |Quad_calc(op,x,y,z) ->
        (
          match op with
            |"+"-> let code = [store z Ax;
                               [Add (Action_reg Ax, 
                                     Action_reg Dx)];
                               load y Dx;
                               load x Ax]
             in merge_lists([], code)
            |"-"-> let code = [store z Ax;
                               [Sub (Action_reg Ax, 
                                     Action_reg Dx)];
                               load y Dx;
                               load x Ax]
             in merge_lists([], code)
            |"*"-> let code = [store z Ax;
                               [IMul Cx];
                               load y Cx;
                               load x Ax]
             in merge_lists([], code)
            |"/"-> let code = [store z Ax;
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
      let op_type = get_type x in 
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
                     [Cmp(get_register(Ax, op_type), get_register(Dx, op_type))];
                     load y Dx;
                     load x Ax] 
      in merge_lists([], code)
  |Quad_jump(l) -> [Jump(label (Some(!l)))]
  |Quad_unit(x) -> 
      let size = param_size x in
      let fun_real_name = id_name x.entry_id in
      let fun_name = name fun_real_name in
        current_fun := fun_real_name;
        let code = [[Sub(Action_reg Sp, Constant size)];
                    [Mov(Register Bp, Register Sp)]; 
                    [Push(Register Bp)];
                    [Proc(fun_name)]]
        in merge_lists([], code)
  |Quad_endu(x) -> let r_name = id_name x.entry_id in
   let u_name = name r_name in
   let ending = Printf.sprintf "%s\tendp\n" u_name in
   let code = [[Misc ending];
               [Ret];
               [Pop(Bp)];
               [Mov(Register Sp, Register Bp)];
               [Label(endof r_name)]]
   in merge_lists([], code)
  |Quad_call(e,_) -> let function_type  = match e.entry_info with
     | ENTRY_function info ->
         info.function_result
     | _ -> internal "Call not a funtion";
            raise Terminate
   in
   let sub_size = if (function_type = TYPE_proc) then 2 else 0 in
   (*let p_size = param_size e in *)     (* FIXME size of the parameters not local vars *)             
   let p_size = get_function_param_size e in
   let code = [[Add(Action_reg Sp, Constant(p_size+4))];
               [Call name(id_name e.entry_id)];
               update_al;
               [Sub(Action_reg Sp, Constant sub_size)]]
   in merge_lists([], code)                        

  |Quad_ret -> [Jump endof(!current_fun)]
  |Quad_par(v,pm) -> let v_type = get_type v in (
     match (v_type, pm) with
       |(TYPE_int, PASS_BY_VALUE) -> 
           let code =  [[Push (Register Ax)]; load v Ax] 
           in merge_lists([], code)
       |(_, PASS_BY_VALUE) -> 
           let code = [[Mov(Mem_loc("byte", Si, 0),Register Al)];
                       [Mov(Register Si, Register Sp)];
                       [Sub(Action_reg Sp, Constant 1)];
                       load v Al]
           in merge_lists([], code)
       |(_, PASS_BY_REFERENCE) 
       |(_, PASS_RET) -> 
           let code = [[Push(Register Si)];
                       load_addr v Si]
           in merge_lists([], code)
   )
  |_ -> []
    
let rec create_assembly = function
    | ([], assembly_list) -> assembly_list
    | (a::quad_list, assembly_list) ->
            let quad_label = Misc(label None) in
            let assembly_so_far = assembly_list @ (quad_label::(final_code_of_quad a))
            in create_assembly (quad_list, assembly_so_far)

    
let rec create_final_code code =
    (*register lib functions*)
    register_lib_functions lib_funcs;
    let count = ref 0 in
    let rec string_dec_fn res =
      incr count; 
      try 
        let s = Queue.take const_strings in
        let lb = Printf.sprintf "@str%d" !count in
        let decl = declare_string lb s in
          string_dec_fn decl^res
      with Queue.Empty -> res
    in
    let globals_size = -(!currentScope.sco_negofs) in
    let assembly_start = start_code (name "PROGRAM") globals_size in
    let assembly_mid = create_assembly(code, []) in
    let assembly_end = end_code in 
    let assembly_string_dec = [Misc (string_dec_fn "")] in
    let assembly_all = assembly_start @ assembly_mid @ assembly_string_dec @ assembly_end 
    in assembly_all


let print_assembly fd assembly_list = 
    let rec print_help d = function
        | (h::tail) -> Printf.fprintf d "%s" h; print_help d tail 
        | [] -> () 
    in
    let assembly_string = (List.map string_of_final_t assembly_list) in
    print_help fd assembly_string;
