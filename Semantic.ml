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

(*function to get variable's type*)
let rec get_var_type pos = function
    |(TYPE_array (t,s), a) -> get_var_type pos (t, a-1)
    |(var_type, 0) -> var_type 
    |_ -> error  "Line:%d.%d: Wrong table sizes" (pos.pos_lnum) 
                   (pos.pos_cnum - pos.pos_bol); TYPE_none

(* Check if expression is a const variable *)
let check_const expr =
  match expr with
    |Expr e -> (
       match e.place with
         | Quad_entry qe -> (
             match qe.entry_info with
               |ENTRY_variable inf -> inf.is_const
               |_ -> false
           )
         |_ -> false
     )
    |_ -> false

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

let check_is_bool expr pos= 
  match expr with
    |Expr e -> (
       let expr_typ = get_type e.place 
       in let spt = create_super_type expr_typ in
         match spt with
           |Bool _ -> true
           |_ -> error  "Line:%d.%d: Not a number or char" (pos.pos_lnum) 
                   (pos.pos_cnum - pos.pos_bol); false
     )
    |Cond c -> true
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


let check_bool_binop_types expr1 expr2 pos=
    if ((check_is_bool expr1 pos) && (check_is_bool expr2 pos))
    then true
    else ( 
      error "Line:%d.%d: Wrong types in boolean operation" (pos.pos_lnum) 
           (pos.pos_cnum - pos.pos_bol);
      false
    )

(*
 * function to check assign operations 
 * if the operator is not '=' then you need the operands to be arithmetic 
 *)
let check_assign operator expr1 expr2 pos= 
  if (check_const expr1) then (
    error  "Line:%d.%d: Assigning to a const variable" (pos.pos_lnum) 
      (pos.pos_cnum - pos.pos_bol);
    false
  ) else (
    let (type_1, type_2) = match (expr1, expr2) with 
      |(Expr e1, Expr e2) -> (get_type e1.place, get_type e2.place)
      |(Expr e1, Cond c2) -> (get_type e1.place, TYPE_bool)
      |(Cond c1, Expr e2) -> (TYPE_bool, get_type e2.place)
      |(Cond c1, Cond c2) -> (TYPE_bool, TYPE_bool)
      |_ -> internal "Not expressions"; raise Terminate
    in 
    let types_match = 
      if equalType type_1 type_2 then true 
      else
        match (type_1, type_2) with
          |(TYPE_real, TYPE_int)
          |(TYPE_char, TYPE_int)
          |(TYPE_int, TYPE_char) ->  true
          |_ -> false
    in
    let res = match operator with
      |"=" -> types_match
      |_ -> (check_is_number expr1 pos) && (check_is_number expr2 pos) && types_match
    in
      if res then true
      else (
        Printf.printf "Type1 = %s type2 = %s\n" (typeToString type_1) (typeToString type_2);
        error  "Line:%d.%d: Wrong types in assignment" (pos.pos_lnum) 
          (pos.pos_cnum - pos.pos_bol);
        false
      )
  )

(* 
 * This is called when constant is a temp variable occuring after calculations
 *)

let calculate_const_val expr pos =
  let rec do_calc  = function
    |[] ->(  (*η τιμή value του place στο expr*)
       match expr.place with
         | Quad_entry qe -> get_var_val qe 
         |_ -> internal "Not an entry"; raise Terminate
     )
    |(h::t) -> match h with
       |Quad_calc (op, op1, op2, res) -> (
          let op1_val = match op1 with
            |Quad_entry e1 -> int_of_string (get_var_val e1)
            |Quad_int e1 -> int_of_string e1
            |_ -> error  "Line:%d.%d: Only integer const calculations are supported" 
                    (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol); 0
          in let op2_val = match op2 with
            |Quad_entry e1 -> int_of_string (get_var_val e1)
            |Quad_int e1 -> int_of_string e1
            |_ -> error  "Line:%d.%d: Only integer const calculations are supported" 
                    (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol); 0
          in let result = match op with
            |"+" -> op1_val + op2_val
            |"-" -> op1_val - op2_val
            |"*" -> op1_val * op2_val
            |"/" -> op1_val / op2_val
            |"%" -> op1_val mod op2_val
            |_ -> internal "Unknown operand"; raise Terminate
          in let res_entry = match res with 
            |Quad_entry e1 -> e1
            |_ -> internal "Result of calc not an entry"; raise Terminate
          in set_var_val res_entry (string_of_int result); do_calc t
        )
       |_ -> do_calc [] 
          in
            match expr.code with
              (*if const value == no code return the value string*)
              |[] -> (if (check_const (Expr(expr))) then 
                        match expr.place with
                          | Quad_entry qe ->( 
                              match qe.entry_info with
                                |ENTRY_variable inf -> inf.value
                                |_ -> internal "Not a variable"; raise Terminate
                            )
                          |_ -> internal "Not an entry"; raise Terminate

                           else( error  "Line:%d.%d: Not a constant value" (pos.pos_lnum) 
                                   (pos.pos_cnum - pos.pos_bol); ""
                           )
               )
              |code -> do_calc (List.rev code)
  
let get_const_val expr pos = 
  match expr with
    |Expr e ->(
       match e.place with
         |Quad_int s
         |Quad_char s
         |Quad_real s
         |Quad_string s
         |Quad_bool s -> s
         |Quad_entry _ -> calculate_const_val e pos
         |_ -> internal "Not valid const quad"; raise Terminate
     )
    |_ -> internal "Const expr not expr"; raise Terminate

let table_size expr pos= 
  let size = get_const_val expr pos in
    try
      int_of_string size
    with Failure "int_of_string" -> error  "Line:%d.%d: Not an integer value as table size" (pos.pos_lnum) 
                                      (pos.pos_cnum - pos.pos_bol); 0

let check_function_params symb_table_param_list given_param_types pos = 
  let get_param_info p = 
    match p.entry_info with
      | ENTRY_parameter inf -> (inf.parameter_type, inf.parameter_mode)
      | _ -> internal "Not a parameter"; raise Terminate
  in 
  let rec help_check = function
    |([], []) -> true
    |([], _)
    |(_, []) -> error "Line:%d.%d: Wrong parameters" (pos.pos_lnum) 
                  (pos.pos_cnum - pos.pos_bol) ;false
    |(a::b, c::d) ->
        let p_type = fst a in
        let p_mode = snd a in
          if (equalType p_type c) then help_check(b, d)
          else
            let is_by_val = (p_mode == PASS_BY_VALUE) in 
            let types_match =
              match (p_type, c) with
                |(TYPE_real, TYPE_int)
                |(TYPE_char, TYPE_int)
                |(TYPE_int, TYPE_char) ->  true
                |_ -> false
            in if (types_match && is_by_val) then help_check(b, d)
            else (error "Line:%d.%d: Wrong parameters" (pos.pos_lnum) 
                    (pos.pos_cnum - pos.pos_bol) ;false)

  in let params_info = List.map get_param_info symb_table_param_list 
  in help_check (params_info, given_param_types)

(*bool val if in loop*)
let in_loop = ref 0
