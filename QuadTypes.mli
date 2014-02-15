type quad_elem_t =
	|Quad_none                        (* Error Handling              *)
	|Quad_entry of Symbol.entry       (* Symbol Table Entries        *)
	|Quad_valof of Symbol.entry       (* Dereferenced Symbol Entries *)
	|Quad_real of string
    |Quad_int of string               (* Constant Integers           *)
    |Quad_char of string              (* Constant Characters         *)
	|Quad_string of string	          (* Constant Strings            *)

val string_of_quad_elem_t : quad_elem_t -> string

type quad_t =
	|Quad_dummy  
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
    q_break : int ref list;
} 

type superexpr = 
    | Expr of expr_ret_type
    | Cond of cond_ret_type

and expr_ret_type = {
	code : quad_t list;
	place : quad_elem_t;
}

and cond_ret_type = {
	c_code : quad_t list;	
	q_true: int ref list;   
	q_false : int ref list;
}

type switch_exp_ret_type = {
   case_list : string list;
   jump_list : int ref list
}
 
type inner_switch_ret_type = {
    cond_list : string list;
    true_list : int ref list;
    code_list : quad_t list;
    false_list : int ref list;
}
val return_null : unit -> expr_ret_type
val return_null_cond : unit -> cond_ret_type 
val return_null_stmt : unit -> stmt_ret_type
val find_opposite_condition : string -> string

val equal_quad_elems : quad_elem_t * quad_elem_t -> bool
