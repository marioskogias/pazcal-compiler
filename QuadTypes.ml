open Types
open Identifier
open Symbol
open Error

(* The main element in quads *)
type quad_elem_t =
  |Quad_none                        (* Error Handling              *)
  |Quad_entry of Symbol.entry       (* Symbol Table Entries        *)
  |Quad_valof of Symbol.entry       (* Dereferenced Symbol Entries *)
  |Quad_real of string
  |Quad_int of string               (* Constant Integers           *)
  |Quad_char of string              (* Constant Characters         *)
  |Quad_string of string            (* Constant Strings            *)

let string_of_quad_elem_t = function
  |Quad_none          -> ""
  |Quad_entry ent     -> id_name ent.entry_id
  |Quad_valof ent     -> Printf.sprintf "[%s]" (id_name ent.entry_id)
  |Quad_int str       -> str
  |Quad_real str      -> str
  |Quad_char str      -> str
  |Quad_string str    -> Printf.sprintf "\"%s\"" str

(* All quad types of the intermediate code *)
type quad_t =
  |Quad_dummy  (* For optimization purposes *)
  |Quad_unit of Symbol.entry
  |Quad_endu of Symbol.entry
  |Quad_calc of string * quad_elem_t * quad_elem_t * quad_elem_t
  |Quad_set of quad_elem_t * quad_elem_t
  |Quad_array of quad_elem_t * quad_elem_t * Symbol.entry
  |Quad_cond of string * quad_elem_t * quad_elem_t * (int ref)
  |Quad_jump of (int ref)
  |Quad_call of Symbol.entry * (quad_elem_t list)
  |Quad_tailCall of Symbol.entry
  |Quad_par of quad_elem_t * Symbol.pass_mode
  |Quad_ret

type stmt_ret_type = {
  s_code : quad_t list;
  q_cont : int ref list;
  q_break : int ref list
} 

(* Return Type of expr *)
type superexpr = 
  | Expr of expr_ret_type
  | Cond of cond_ret_type


(* Return Type of an Expression *)
and expr_ret_type = {
  code : quad_t list;
  place : quad_elem_t
}

(* Return Type of a Condition 
 * Jumps are handled as relative jumps at first, converted later *)
and cond_ret_type = {
  c_code : quad_t list; 
  q_true: int ref list;   
  q_false : int ref list
}

type switch_exp_ret_type = {
   case_list : string list;
   jump_list : int ref list
}
 
type inner_switch_ret_type = {
    cond_list : string list;
    true_list : int ref list;
    code_list : quad_t list
}


(* Returning a "null" quad - error handling mostly *)
let return_null () = {code = []; place = Quad_none}

let return_null_cond () = {c_code = []; q_true = []; q_false = []}

let return_null_stmt () = {s_code = []; q_cont = []; q_break = []}

let find_opposite_condition = function
  | "==" -> "!="
  | "!=" -> "=="
  | "<=" -> ">"
  | ">"  -> "<="
  | ">=" -> "<"
  | "<"  -> ">="
  | _ -> internal "Not a conditional string"; raise Terminate

let equal_quad_elems = function
  | Quad_none, Quad_none -> true
(*  | Quad_valof e1, Quad_valof e2 *)
(*  | Quad_entry e1, Quad_entry e2 -> Symbol.equalEntries e1 e2 *)
  | Quad_int s1, Quad_int s2
  | Quad_char s1, Quad_char s2
  | Quad_string s1, Quad_string s2 -> s1 = s2
  | _ -> false
