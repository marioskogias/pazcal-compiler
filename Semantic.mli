type num_type = 
  |Int of Types.typ
  |Real of Types.typ

type super_type = 
  | Num of num_type
  | Bool of Types.typ

val check_is_number : QuadTypes.superexpr -> Lexing.position -> bool
val check_binop_types : QuadTypes.superexpr ->  QuadTypes.superexpr -> Lexing.position -> Types.typ
val check_bool_binop_types : Types.typ -> Types.typ -> Lexing.position -> Types.typ
val check_int_binop_types : QuadTypes.superexpr -> QuadTypes.superexpr -> Lexing.position -> Types.typ
val check_equalities : QuadTypes.superexpr -> QuadTypes.superexpr -> Lexing.position -> bool
val check_is_bool : QuadTypes.superexpr -> Lexing.position -> bool
val table_size : Types.typ -> string -> Lexing.position -> int
val check_function_params : Symbol.entry list -> Types.typ list -> Lexing.position -> bool

val check_assign : string -> Types.typ -> Types.typ -> Lexing.position -> bool
val in_loop : int ref
val create_super_type : Types.typ -> super_type
