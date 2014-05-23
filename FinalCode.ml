open FinalSupport
open Symbol

(* this it the global bp. Go there to access global data *)
let global_bp = ref 0

(* Implementation of basic helper functions *)

(* Function to check if an entry is local to a function *)
(* Entries can either be local or global and we have no nested functions *)
let local e = 
    match e.entry_scope.sco_parent with
    | None -> false
    | _ -> true

(* get_AR function to get bp for non local data -> global data *)
let get_ar = [ Mov (Register Si, Num (string_of_int !global_bp)) ] 

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
