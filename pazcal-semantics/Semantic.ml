open Types
open Identifier
open Symbol
open Error
open Lexing
open Output 

(* Semantic checking of values in binary expressions *)
let check_binop_types type_1 type_2 pos=
	match (type_1, type_2) with

  	|(TYPE_int, TYPE_int) -> TYPE_int
 	|(TYPE_real, TYPE_real) 
  	|(TYPE_int, TYPE_real)
  	|(TYPE_real, TYPE_int)
    	-> TYPE_real
  	|_ -> ignore(print_error "Wrong types\n" pos); TYPE_none

let check_bool_binop_types type_1 type_2 pos=
  	match (type_1, type_2) with

  	|(TYPE_bool, TYPE_bool) -> TYPE_bool
  	|_ -> ignore(print_error "Wrong types\n" pos); TYPE_none

let check_int_binop_types type_1 type_2 pos=
  	match (type_1, type_2) with

  	|(TYPE_int, TYPE_int) -> TYPE_int
  	|_ -> ignore(print_error "Wrong types\n" pos); TYPE_none

let check_equalities type_1 type_2 pos= 
	match (type_1, type_2) with

	|(TYPE_int, TYPE_int)
	|(TYPE_real, TYPE_real)
	|(TYPE_real, TYPE_int)
	|(TYPE_int, TYPE_real) -> TYPE_bool
	|_ -> ignore(print_error "Wrong types\n" pos); TYPE_none

let check_is_number type_1 pos= 
	match type_1 with
	|TYPE_int -> TYPE_int
	|TYPE_real -> TYPE_real
	|_ -> ignore(print_error "is not a number" pos); TYPE_none

let check_is_bool type_1 pos= 
	match type_1 with
	|TYPE_bool -> TYPE_bool
	|_ -> ignore(print_error "is not a boolean val" pos); TYPE_none

let table_size val_type value pos= 
    try
        match val_type with
        | TYPE_int -> int_of_string value
        | _ -> ignore(print_error "Not an integer value" pos); -1
	with Failure "int_of_string" -> 0

let check_function_params symbol_table_params_list passed_param_list pos= 
		
	let rec help_check = function
		| ([],[]) -> true
		| ([], l1) -> if (List.length l1) > 0 then (ignore(print_error "wrong params 1" pos); false) else true
		| (l1,[]) -> if (List.length l1) > 0 then (ignore(print_error "wrong params 2" pos);false) else true
		| ((a::b), (c::d)) -> 	
			let par_type =
				match a.entry_info with
          				| ENTRY_parameter inf -> inf.parameter_type
					| _ -> TYPE_none
			in
			if (equalType par_type c) then help_check (b, d)
			else (ignore(print_error "wrong params 3" pos); false)

	in help_check (symbol_table_params_list, passed_param_list)


(*function to check assign operations
  if the operator is not '=' then you need the second operant to be a number 
*)
let check_assign operator type_1 type_2 pos= 
	let a = match operator with
            |"=" -> true
            | _ -> if not((check_is_number type_2 pos) = TYPE_none) then true else false
    in 
    let b = 
        if equalType type_1 type_2 
            then true
        else
            match (type_1, type_2) with
                |(TYPE_real, TYPE_int)
                |(TYPE_char, TYPE_int)
                |(TYPE_int, TYPE_char) ->  true
                |_ -> false
    in
        if (a && b) then true
        else (ignore(print_error "Wrong types in assignment\n" pos);false)

(*bool val if in loop*)
let in_loop = ref false


