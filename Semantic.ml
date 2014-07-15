open Types
open Identifier
open Symbol
open Error
open Lexing
open Output 
open QuadTypes
open Quads

type num_type = 
  |Int of Types.typ
  |Real of Types.typ

type super_type = 
  | Num of num_type
  | Bool of Types.typ

let create_super_type t = 
  match t with
    |TYPE_bool -> Bool t
    |TYPE_real
    |TYPE_int
    |TYPE_char  -> (
       match t with
         |TYPE_real -> Num(Real t)
         |_ -> Num(Int t)
    )
    |_ -> internal "Not a basic type"; raise Terminate

let check_is_number expr pos= 
  match expr with
    |Expr e -> (
       let expr_typ = get_type e.place 
       in let spt = create_super_type expr_typ in
         match spt with
           |Num _ -> true
           |_ -> error  "Line:%d.%d: Not a number or char" (pos.pos_lnum) 
                   (pos.pos_cnum - pos.pos_bol); false
     )
    | _ -> internal "Not an expresion"; raise Terminate

(* Semantic checking of values in binary expressions *)
let check_binop_types expr1 expr2 pos=
  let (type_1, type_2) = match (expr1, expr2) with 
    |(Expr e1, Expr e2) -> (get_type e1.place, get_type e2.place) 
    |_ -> internal "Not expressions"; raise Terminate
  in let sp1 = create_super_type type_1
  in let sp2 = create_super_type type_2
  in       
    match (sp1, sp2) with
      |(Num n1, Num n2) ->(
         match (n1,n2) with
           |(Int _, Int _) -> TYPE_int
           |_ -> TYPE_real
       )
      |_ -> error  "Line:%d.%d: Wrong types" (pos.pos_lnum) 
              (pos.pos_cnum - pos.pos_bol); TYPE_none

let check_int_binop_types expr1 expr2 pos=
    let res = check_binop_types expr1 expr2 pos in
      if (res != TYPE_int) then (
        error  "Line:%d.%d: Wrong types" (pos.pos_lnum) 
          (pos.pos_cnum - pos.pos_bol); 
        TYPE_none)
      else TYPE_int

let check_equalities expr1 expr2 pos =
    if ((check_is_number expr1 pos) && (check_is_number expr2 pos))
    then true
    else ( 
      error "Line:%d.%d: Wrong types in assignment or comparison" (pos.pos_lnum) 
           (pos.pos_cnum - pos.pos_bol);
      false
    )


let check_bool_binop_types type_1 type_2 pos=
  	match (type_1, type_2) with

  	|(TYPE_bool, TYPE_bool) -> TYPE_bool
  	|_ -> error  "Line:%d.%d: Wrong types" (pos.pos_lnum) 
           (pos.pos_cnum - pos.pos_bol); TYPE_none

let check_is_bool expr pos= 
  match expr with
    |Expr e -> (
       let expr_typ = get_type e.place in
         match expr_typ with
           |TYPE_bool -> true
           |_ -> error  "Line:%d.%d: Not a boolean" (pos.pos_lnum) 
                   (pos.pos_cnum - pos.pos_bol); false
     )
    | _ -> internal "Not an expresion"; raise Terminate

let table_size val_type value pos= 
   (* try
        match val_type with
        | TYPE_int -> int_of_string value
        | _ -> ignore(print_error "Not an integer value in table size" pos); -1
	with Failure "int_of_string" -> ignore(print_error "Unknown table size" pos);0 (* if zero then check memory issues...*)
    EXPLAIN
    *)
  match val_type with
    |TYPE_int ->
        try
          int_of_string value
        with Failure "int_of_string" -> 0
    |_ -> error  "Line:%d.%d: Not an integer value" (pos.pos_lnum) 
                  (pos.pos_cnum - pos.pos_bol); -1



let check_function_params symbol_table_params_list passed_param_list pos= 
  let rec help_check = function
    | ([],[]) -> true
    | ([], l1) -> if (List.length l1) > 0 then (error  "Line:%d.%d: Wrong parameters" (pos.pos_lnum) 
                                                  (pos.pos_cnum - pos.pos_bol); false) else true
    | (l1,[]) -> if (List.length l1) > 0 then (error  "Line:%d.%d: Wrong parameters" (pos.pos_lnum) 
                                                 (pos.pos_cnum - pos.pos_bol) ;false) else true
    | ((a::b), (c::d)) -> 	
        let par_type =
          match a.entry_info with
            | ENTRY_parameter inf -> inf.parameter_type
            | _ -> TYPE_none
        in
          if (equalType par_type c) then help_check (b, d)
          else (error  "Line:%d.%d: Wrong parameters" (pos.pos_lnum) 
                  (pos.pos_cnum - pos.pos_bol); false)

  in help_check (symbol_table_params_list, passed_param_list)


(*function to check assign operations
  if the operator is not '=' then you need the second operant to be a number 
*)
let check_assign operator type_1 type_2 pos= 
	let a = match operator with
            |"=" -> true
            | _ -> (*if not((check_is_number type_2 pos) = TYPE_none) then true else*) false
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
        else (error  "Line:%d.%d: Wrong types in assignment" (pos.pos_lnum) 
                  (pos.pos_cnum - pos.pos_bol);false)

(*bool val if in loop*)
let in_loop = ref 0


