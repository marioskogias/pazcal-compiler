open Types
open Identifier
open Symbol
open Error
open Lexing

(* Semantic checking of values in binary expressions *)
let check_binop_types type_1 type_2 =
	match (type_1, type_2) with

  	|(TYPE_int, TYPE_int) -> TYPE_int
 	|(TYPE_real, TYPE_real) 
  	|(TYPE_int, TYPE_real)
  	|(TYPE_real, TYPE_int)
    	-> TYPE_real
  	|_ -> ignore(error "Wrong types\n"); TYPE_none

let check_bool_binop_types type_1 type_2 =
  	match (type_1, type_2) with

  	|(TYPE_bool, TYPE_bool) -> TYPE_bool
  	|_ -> ignore(error "Wrong types\n"); TYPE_none

let check_int_binop_types type_1 type_2 =
  	match (type_1, type_2) with

  	|(TYPE_int, TYPE_int) -> TYPE_int
  	|_ -> ignore(error "Wrong types\n"); TYPE_none

let check_equalities type_1 type_2 = 
	match (type_1, type_2) with

	|(TYPE_int, TYPE_int)
	|(TYPE_real, TYPE_real)
	|(TYPE_real, TYPE_int)
	|(TYPE_int, TYPE_real) -> TYPE_bool
	|_ -> ignore(error "Wrong types\n"); TYPE_none

let check_is_number type_1 = 
	match type_1 with
	|TYPE_int -> TYPE_int
	|TYPE_real -> TYPE_real
	|_ -> ignore(error "is not a number"); TYPE_none

let check_is_bool type_1 = 
	match type_1 with
	|TYPE_bool -> TYPE_bool
	|_ -> ignore(error "is not a boolean val"); TYPE_none

let table_size val_type value = 
	match val_type with
	|TYPE_int -> 
		try
			int_of_string value
		with Failure "int_of_string" -> 0
	|_ -> ignore(error "Not an integer value"); -1
