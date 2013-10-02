val check_binop_types :  Types.typ -> Types.typ -> Types.typ
val check_bool_binop_types : Types.typ -> Types.typ -> Types.typ
val check_int_binop_types :  Types.typ -> Types.typ -> Types.typ
val check_equalities : Types.typ ->  Types.typ -> Types.typ
val check_is_number : Types.typ -> Types.typ
val check_is_bool : Types.typ -> Types.typ
val table_size : Types.typ -> string -> int
val check_function_params : Symbol.entry list -> Types.typ list -> bool

val in_loop : bool ref
