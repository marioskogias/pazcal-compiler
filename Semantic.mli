type num_type = 
  |Int of Types.typ
  |Real of Types.typ

type super_type = 
  | Num of num_type
  | Bool of Types.typ

val get_var_type : Lexing.position -> Types.typ * int -> Types.typ
val check_is_number : ?v:bool -> QuadTypes.superexpr -> Lexing.position -> bool
val check_binop_types : QuadTypes.superexpr ->  QuadTypes.superexpr -> Lexing.position -> Types.typ
val check_bool_binop_types : ?v:bool -> QuadTypes.superexpr ->  QuadTypes.superexpr -> Lexing.position -> bool
val check_int_binop_types : QuadTypes.superexpr -> QuadTypes.superexpr -> Lexing.position -> Types.typ
val check_equalities : QuadTypes.superexpr -> QuadTypes.superexpr -> string -> Lexing.position -> bool
val check_is_bool : ?v:bool -> QuadTypes.superexpr -> Lexing.position -> bool
val table_size : QuadTypes.superexpr -> Lexing.position -> int
val check_function_params : Symbol.entry list -> Types.typ list -> Lexing.position -> bool
val check_assign : string -> QuadTypes.superexpr -> QuadTypes.superexpr -> Lexing.position -> bool
val create_super_type : Types.typ -> super_type
val get_const_val : QuadTypes.superexpr -> Lexing.position -> string
val check_lval : Symbol.entry -> Lexing.position ->bool
val check_return : bool ->Lexing.position -> bool
val check_if_return : Symbol.entry -> bool -> Lexing.position -> bool
val in_loop : int ref
val in_func : bool ref
val did_return : bool ref
