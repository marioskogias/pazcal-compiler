open Types
open Error
open Symbol
open Identifier
open Semantic
open Lexing
open QuadTypes
open Parsing

(* Get Type of a quad_elem_t *)
let get_type = function
  |Quad_none -> TYPE_none
  |Quad_int (_) -> TYPE_int
  |Quad_real (_) -> TYPE_real
  |Quad_char(_) -> TYPE_char
  |Quad_bool(_) -> TYPE_bool
  |Quad_string (str) -> TYPE_array(TYPE_char, String.length str)
  |Quad_valof (_, t) -> t 
  |Quad_entry (ent) -> get_entry_type ent

(* Small Function To Check if Quad is en entry or not *)
let is_entry quad =
  match quad with
  | Quad_entry(_) -> true
  | _ -> false

let is_entry_or_valof quad =
  match quad with
  | Quad_entry _ 
  | Quad_valof _ -> true
  | _ -> false

let is_not_temporary quad =
  match quad with
  | Quad_entry ent -> (
    match ent.entry_info with
    | ENTRY_variable _ 
    | ENTRY_parameter _ -> true
    | _ -> false
    )
  | _ -> false

let is_temporary quad =
  match quad with
  | Quad_entry ent -> (
    match ent.entry_info with
    | ENTRY_temporary _ -> true
    | _ -> false
  )
  | Quad_valof _ -> true
  | _ -> false

let is_valof quad =
  match quad with
  | Quad_valof _ -> true
  | _ -> false

let is_not_local_var f quad =
  match quad with
  | Quad_entry ent -> 
      f.entry_scope.sco_nesting + 1 != ent.entry_scope.sco_nesting
  | _ -> true

let is_parameter_by_reference quad =
  match quad with
  | Quad_entry ent -> (
    match ent.entry_info with
    | ENTRY_parameter par_info ->
        par_info.parameter_mode != PASS_BY_VALUE
    | _ -> false
  )
  | _ -> false
 
let rec handle_array var_type = function
  | [] -> 
  {code = []; place = Quad_none} 
  | [h] ->
    begin
      match h with
      | Expr e ->
      (
        match var_type with
          | TYPE_array (array_type, size) ->
            let temp = newTemporary TYPE_int in
              {
                code = [Quad_calc("*", Quad_int(string_of_int size), e.place, Quad_entry(temp))];
                place = Quad_entry(temp);
              }
          | _ -> {code = []; place=Quad_none}
      )
      | Cond c -> return_null()
    end
  | h::t ->
    begin
      match h with
      | Expr e ->
      (
        let temp1 = newTemporary TYPE_int in
        let temp2 = newTemporary TYPE_int in
        match var_type with
          | TYPE_array (temp_var_type, temp_size) -> 
            let rest = handle_array temp_var_type t 
            in
            {
              code = Quad_calc("*", Quad_int(string_of_int temp_size), Quad_entry(temp2), Quad_entry(temp1))::Quad_calc("+", e.place, rest.place, Quad_entry(temp2))::rest.code;
              place = Quad_entry(temp1);
            }
          | _ -> return_null()
      )
      | Cond c -> return_null()
    end 
 
(* Handling [x] case *)
let dereference x = 
  match x with 
  | Expr ex ->
    begin
      match ex.code with
      |(Quad_array(a, _, ent)::_)->
        {ex with place = Quad_valof(ent, getArrayType (get_type a))}
      |_ -> ex
    end
  | Cond cond -> return_null () 

(* Get Type of a quad_elem_t *)

(* Get Size Description from a quad_elem_t *)
(* Extract the Entry from a quad_elem_t *)
let extract_entry = function
  |Quad_entry (ent) -> ent
  |Quad_valof (ent, _) -> ent
  |_ -> internal "Not an entry"; raise Terminate

(* Get a string description of a quad_elem_t *)
let get_id = function
  |Quad_none -> internal "proc func call"; raise Terminate
  |Quad_int (i) -> i
  |Quad_real (r) -> r
  |Quad_char (c) -> c
  |Quad_bool (b) -> b
  |Quad_string (s) -> s
  |Quad_valof (ent, _)
  |Quad_entry (ent) -> id_name ent.entry_id

(* Main function to convert a quad to a string *)
let string_of_quad_t = function
  |Quad_unit(ent) -> 
    Printf.sprintf "unit, %s, -, -\n"
    (id_name ent.entry_id)
  |Quad_endu(ent) -> 
    Printf.sprintf "endu, %s, -, -\n" 
    (id_name ent.entry_id)
  |Quad_calc (op, q1, q2, q) ->
    Printf.sprintf "%s, %s, %s, %s\n"
      (op)
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      (string_of_quad_elem_t q)
  |Quad_set(q,qr) ->
    Printf.sprintf ":=, %s, -, %s\n" 
      (string_of_quad_elem_t q)
      (string_of_quad_elem_t qr)
  |Quad_array(q1, q2, e) ->
    Printf.sprintf "array, %s, %s, %s\n"
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      (id_name e.entry_id)
  |Quad_cond(op, q1, q2, i) ->
    Printf.sprintf "%s, %s, %s, %d\n"
      (op)
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      !i
  |Quad_jump i  ->
    Printf.sprintf "jump, -, -, %d\n" !i
  |Quad_call (ent,_) ->
    Printf.sprintf "call, -, -, %s\n"
      (id_name ent.entry_id)
  |Quad_par(q,pm) ->
    let string_of_pass_mode = function
        |PASS_BY_VALUE -> "V"
        |PASS_BY_REFERENCE -> "R"
        |PASS_RET -> "RET" 
    in Printf.sprintf "par, %s, %s, -\n"
      (string_of_quad_elem_t q)
      (string_of_pass_mode pm)
  |Quad_ret -> "ret, -, -, -\n" 
  |Quad_dummy -> ""

(*Get a list of quads a print it to the given out_channel*)
let print_quads fd qlist =
    let help_fn q =
        let string_quad = string_of_quad_t q
        in Printf.fprintf fd "%s" string_quad
    in ignore(List.map help_fn qlist)

(* ----------------------------------------------------------------------------- *)

(* Functions to generate intermediate code in the parser *)

(* IMPORTANT: Intermediate code in the lists must be inverted *)

let handle_expr_to_stmt sexpr =
  match sexpr with 
  |Expr expr -> {s_code= expr.code; q_break=[]; q_cont=[]}
  |Cond cond -> return_null_stmt()

(*let handle_lval_to_expr l_val =
  match l_val.l_type with
    | TYPE_none -> Expr({code = l_val.l_code; place = l_val.l_place})
    | _ -> (
	let result_temp = newTemporary l_val.l_type in
        let array_quad = List.hd l_val.l_code in
        let array_temp, array_type = match array_quad with
          | Quad_array(q1, q2, e) -> e, get_type q1
          | _ -> internal "Not an array"; raise Terminate
        in
	Expr({
          code = Quad_set(Quad_valof(array_temp, array_type), Quad_entry(result_temp))::l_val.l_code;
          place = Quad_entry(result_temp)
        })
      )
 *)
(* Handle statement merge *) 

let handle_stmt_merge stmt1 stmt2 =
  let len = List.length stmt1.s_code in
  let len2 = List.length stmt2.s_code in
  List.iter (fun x -> x := !x - len) stmt2.q_cont;
  List.iter (fun x -> x := !x + len2) stmt1.q_break;
  { 
   s_code = stmt2.s_code @ stmt1.s_code;
   q_cont = stmt1.q_cont@stmt2.q_cont;
   q_break = stmt1.q_break @ stmt2.q_break;
  }


(* Handle break *)
let handle_break () =
  let break_ref = ref 1 in
  let code_break = (Quad_jump (break_ref)) in {
    s_code = [code_break];
    q_cont = [];
    q_break = [break_ref];
  }

(* Handle continue *)
let handle_continue ()=
  let cont_ref = ref 0 in
  let code_cont = (Quad_jump (cont_ref)) in {
    s_code = [code_cont];
    q_cont = [cont_ref];
    q_break = [];
  }


(* Handle Not *)
let handle_not expr =
  match expr with
    | Expr expr -> return_null_cond ()
    | Cond cond -> {
      c_code = cond.c_code;
      q_true = cond.q_false;
      q_false = cond.q_true;
      }


(* Handle an arithmetical expression 
 * Get the 2 types, semantically check them and create the intermediate code 
 * required *)
let handle_expression op expr1 expr2 expr_typ (sp,ep) =
  match expr1, expr2 with
  |  Expr e1, Expr e2 ->
    let t1 = expr_typ in
    let temp = newTemporary t1 in {
      code  = Quad_calc(op,e1.place, e2.place, Quad_entry(temp))
              ::(e2.code)@(e1.code);
      place = Quad_entry(temp);
    }
  | _ -> return_null ()

(* Handle Plus Plus *)
let handle_plus_plus sexpr =
    match sexpr with
    | Expr expr -> let temp = newTemporary TYPE_int in
                let plus_quad = Quad_calc("+", Quad_int("1"), expr.place, Quad_entry(temp)) in
                let assign_quad = 
                    match expr.place with
                    |Quad_valof (_)
                    |Quad_entry (_) -> Quad_set(Quad_entry(temp),expr.place)  
                    | _ -> internal "Assigning to something not an entry";
                     raise Terminate
      in {
        s_code = assign_quad::[plus_quad];
        q_cont = [];
        q_break = [];
      }
    | Cond cond -> return_null_stmt()

(* Handle Minus Minus *)
let handle_minus_minus sexpr =
    match sexpr with
    | Expr expr -> let temp = newTemporary TYPE_int in
                let plus_quad = Quad_calc("-", expr.place, Quad_int("1"), Quad_entry(temp)) in
                let assign_quad = 
                    match expr.place with
                    |Quad_valof (_)
                    |Quad_entry (_) -> Quad_set(Quad_entry(temp),expr.place)  
                    | _ -> internal "Assigning to something not an entry";
                     raise Terminate
      in {
        s_code = assign_quad::[plus_quad];
        q_cont = [];
        q_break = [];
      }
    | Cond cond -> return_null_stmt()

(* Handle signs in expression *)
let handle_unary_expression op expr pos =
  match expr with
  | Expr exp ->
    let t = get_type exp.place in
    if (t==TYPE_int) 
    then match op with
      |"+" -> 
        exp
      |"-" -> 
        let temp = newTemporary TYPE_int in
        let new_quad = Quad_calc("-",Quad_int("0"), exp.place, Quad_entry(temp)) in
          { code = (new_quad :: exp.code); place = Quad_entry(temp) }
      |_ -> internal "wrong unary expression"; raise Terminate
    else (
     (* print_unary_type_error op t pos;*)
      return_null ()
    )
  | _ -> return_null ()

let handle_func_call ent pos expr_list =

  (* Unzip expression list 
   * Takes expression list - reverse order 
   * Returns a triplet : code, place and types, correct order *)
  let rec unzip_expr_list code_acc place_acc type_acc = function
    | [] -> 
        (code_acc, place_acc, type_acc)
    | (h::t) ->
        unzip_expr_list (h.code :: code_acc) (h.place::place_acc)
          ((get_type h.place)::type_acc) t in

  (* Create Par quads 
   * Takes function information and parameter list 
   * Returns a list of Par Quads - normal *)
  let rec create_par_quads acc = function
    | (_,[]) ->
      List.rev acc
    | (hfi::tfi, hp::tp) -> 
      begin
        match hfi.entry_info with
        | ENTRY_parameter (par_info) ->
            let quad_type = get_type hp in
            let quad_code = 
              if ((quad_type != par_info.parameter_type) && 
                  (par_info.parameter_mode == PASS_BY_VALUE)) then
                    let tmp = newTemporary par_info.parameter_type in
                     [Quad_par (Quad_entry(tmp), par_info.parameter_mode); 
                      Quad_set(hp, Quad_entry(tmp))] 
              else [Quad_par (hp, par_info.parameter_mode)] in
          create_par_quads (quad_code::acc) (tfi, tp)
        | _ -> 
          internal "Function parameter not a parameter"; 
          raise Terminate
      end
    | _ -> 
      internal "Less args in create_par_quads"; 
      raise Terminate in

  (* Reverse the order of the code_list and add the par_quads *)
  let rec reverse_code_list acc = function
    | ([], []) -> acc
    | ((h::t), (hp::tp)) -> reverse_code_list (hp@h@acc) (t,tp) 
    | _ -> internal "Uneven args and code"; raise Terminate in

  
  (* Extract expr_list information *)        
  let (code_list, param_list, type_list) = 
    unzip_expr_list [] [] []  (List.rev expr_list) in

  
  match ent.entry_info with
  |ENTRY_function (info) ->(
      (* Generate par_quads *)
      let par_code = create_par_quads [] 
        (info.function_paramlist, param_list) in 
  
      let entire_code = reverse_code_list [] (code_list, par_code) in

      (* Create code based on function result *)
      match (info.function_result) with
      | TYPE_proc ->
        {
          code = Quad_call(ent,param_list)::entire_code;
          place = Quad_none
        }
      | TYPE_int
      | TYPE_char 
      | TYPE_bool -> 
        let temp = newTemporary info.function_result in 
        let ret_place = Quad_entry temp in
        let par_q = Quad_par ( ret_place , PASS_RET) in {
          code = Quad_call(ent,(param_list@[ret_place]))::par_q::entire_code;
          place = Quad_entry(temp)
        }
      | _ -> return_null ()
  )
  |_ ->   
    error "Invalid Function call. Identifier is not a function \
      at line %d, position %d."
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    return_null ()

let rec handle_format_list (*format_list quad_list int_quads*) = function
  | [], a, _ -> a
  | a::b, quads, int_quad -> 
    (let print_func =
        (match fst a, snd a with
            | Expr e1, Expr e2 ->
                (
                    match (get_type (e1.place)) with
                        |TYPE_int _ -> "WRITE_INT"
                        |TYPE_char _ -> "WRITE_CHAR"
                        |TYPE_array _ -> "WRITE_STRING"
                        |TYPE_bool _ -> "WRITE_BOOL"
                        |_ -> ""
                )
            | _, _ -> ""
        )
    in let e = lookupEntry  (id_make print_func) LOOKUP_ALL_SCOPES true
    in let get_expr e =
    (
        match e with
            | Expr e -> e
            | _ -> return_null()
    )
    in let expr_list = List.map get_expr (fst a::([snd a]))
    in let expr = (handle_func_call e (rhs_start_pos 1) expr_list)
    in let new_quads = (int_quad.code)@expr.code
    in handle_format_list (b, (new_quads@quads), int_quad)
  )

let handle_write write_type form_list =
  let (intermediate_quad, break_line_quad)= (
    match write_type with
        |"write" -> ({code = []; place = Quad_int("0")}, {code = []; place = Quad_int("0")})
        |"writeln" -> let e = lookupEntry (id_make "putchar") LOOKUP_ALL_SCOPES true in
            (
                {code = []; place = Quad_int("0")},
                handle_func_call e (rhs_start_pos 1)  [{ code=[]; place= Quad_char ("'\\n'")}]
            )
        |"writesp" -> let e = lookupEntry (id_make "putchar") LOOKUP_ALL_SCOPES true in
            (
                handle_func_call e (rhs_start_pos 1)  [{ code=[]; place= Quad_char ("' '")}],
                {code = []; place = Quad_int("0")}
            )
        |"writespln" ->(
            let e = lookupEntry (id_make "putchar") LOOKUP_ALL_SCOPES true in
            let expr1 = handle_func_call e  (rhs_start_pos 1) [({ code=[]; place= Quad_char ("'\\n'")})] in
            let expr2 = handle_func_call e  (rhs_start_pos 1) [({ code=[]; place= Quad_char ("' '")})] in
            (
                ({code=expr2.code; place=expr2.place}),
                ({code=expr1.code; place=expr1.place})
            )
        )
        |_ -> internal "Not a valid write function"; raise Terminate 
  )
  in match form_list with
  | [] ->
    {
        s_code = break_line_quad.code@intermediate_quad.code;
        q_cont = [];
        q_break = [];
    }
  | _ ->
    {
        s_code = break_line_quad.code@(handle_format_list (form_list, [], (intermediate_quad)));
        q_cont = [];
        q_break = [];
    }


let condition_to_expr expr =
    match expr with
    | Cond c ->
        let temp = newTemporary TYPE_bool                                           
        in let quad_false = Quad_set(Quad_bool("false"), Quad_entry(temp))          
        in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))            
        in let new_quad = Quad_jump (ref (2)) in                                       
        List.iter (fun x -> x := !x + 2) c.q_false;                                 
        Expr{                                                                           
            code = quad_false :: (new_quad::(quad_true :: c.c_code));               
            place = Quad_entry(temp)                                                
        }
    | Expr e -> expr

(* Handle Comparisons *)
let handle_comparison op sexp1 sexp2 (sp,ep) =
  (* First Check the types of the compared things *)
  let (exp1, exp2) = match op with
  | "=="
  | "!=" -> (
    match sexp1, sexp2 with
    | Cond _, Cond _ ->                   
        let e1 = condition_to_expr sexp1 in
        let e2 = condition_to_expr sexp2 in
        (e1, e2)
    | Cond _, Expr _ ->
        let e1 = condition_to_expr sexp1 in
        (e1, sexp2)
    | Expr _, Cond _ ->
        let e2 = condition_to_expr sexp2 in
        (sexp1, e2)
    | Expr _, Expr _-> 
        (sexp1, sexp2)
    )
  | _ -> (sexp1, sexp2)
  in
      match exp1, exp2 with
      | Expr e1, Expr e2 ->
            (* Invariant for Jumps :
             * Everything points to the beginning of the next block
             * with a relative offset. Backpatching is done additively *)
            let true_ref = ref 2 in 
            let false_ref = ref 1 in
            let code_true = (Quad_cond(op, e1.place, e2.place, true_ref))
            in let code_false = (Quad_jump (false_ref)) in {
              c_code = code_false::code_true::e2.code@e1.code;
              q_true = [true_ref];
              q_false = [false_ref];
            }
      | _ -> return_null_cond ()

(* Handle boolean values 
 * Constant values means no jump in the "opposite" direction 
 * Extraneous code can be eliminated with dead code elimination optimization *)
let handle_cond_const is_true = 
  let x = ref 1 in {
    c_code = [Quad_jump(x)];
    q_true = if (is_true) then [x] else [];
    q_false = if (is_true) then [] else [x];
  }

(* Handle an "and" cond *)
let handle_and sexpr1 sexpr2 =
  (* The "next" quad will be left unchanged for c2 but c1 will point |c2.code| 
   * later. For immediate evaluation when c1 is false we need to go the end 
   * of everything, when c1 is true we need to evaluate c2. *)
  match sexpr1, sexpr2 with
  | Cond c1, Cond c2 ->
    let len = List.length c2.c_code in
      List.iter (fun x -> x := !x + len) c1.q_false;
      { 
        c_code = c2.c_code @ c1.c_code;
        q_true = c2.q_true;
        q_false = c1.q_false @ c2.q_false;
      }
  | Cond c1, Expr e1 ->
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c2 = handle_comparison "==" (Expr(e1)) (true_expr) (1, 3) in
        let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_false;
          { 
            c_code = c2.c_code @ c1.c_code;
            q_true = c2.q_true;
            q_false = c1.q_false @ c2.q_false;
          }
  | Expr e1, Cond c2 ->
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c1 = handle_comparison "==" (Expr(e1)) true_expr (1, 3) in
         let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_false;
          { 
            c_code = c2.c_code @ c1.c_code;
            q_true = c2.q_true;
            q_false = c1.q_false @ c2.q_false;
          }
  | Expr e1, Expr e2 -> 
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c1 = handle_comparison "==" (Expr(e1)) true_expr (1, 3)
    in let c2 = handle_comparison "==" (Expr(e2)) true_expr (1, 3) in
        let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_false;
          { 
            c_code = c2.c_code @ c1.c_code;
            q_true = c2.q_true;
            q_false = c1.q_false @ c2.q_false;
          }

(* Handle an "or" cond *)
let handle_or cond1 cond2 = 
  (* Similarly, add |c2.code| to the relative jumps in c1 but now the "true" 
   * condition is the one that can "short-circuit" *)
  match cond1, cond2 with
  | Cond c1, Cond c2 ->
    let len = List.length c2.c_code in
      List.iter (fun x -> x := !x + len) c1.q_true;
      {
        c_code = c2.c_code @ c1.c_code;
        q_true = c1.q_true @ c2.q_true;
        q_false = c2.q_false;
      }
  | Cond c1, Expr e1 ->
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c2 = handle_comparison "==" (Expr(e1)) (true_expr) (1, 3) in
        let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_true;
          { 
            c_code = c2.c_code @ c1.c_code;
            q_true = c1.q_true @ c2.q_true;
            q_false = c2.q_false;
          }
  | Expr e1, Cond c2 ->
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c1 = handle_comparison "==" (Expr(e1)) true_expr (1, 3) in
         let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_true;
          { 
            c_code = c2.c_code @ c1.c_code;
            q_true = c1.q_true @ c2.q_true;
            q_false = c2.q_false;
          }
  | Expr e1, Expr e2 -> 
    let temp = newTemporary TYPE_bool
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
    in let c1 = handle_comparison "==" (Expr(e1)) true_expr (1, 3)
    in let c2 = handle_comparison "==" (Expr(e2)) true_expr (1, 3) in
        let len = List.length c2.c_code in
          List.iter (fun x -> x := !x + len) c1.q_true;
          {
            c_code = c2.c_code @ c1.c_code;
            q_true = c1.q_true @ c2.q_true;
            q_false = c2.q_false;
          }

(* Handle assignmenet *)
let handle_assignment assign lval exp (sp,ep) =
 (* let t1 = get_type lval.place in
  let t2 = get_type expr.place in
  if (check_types "=" t1 t2 sp ep) 
  then*) 
  let expr = match exp with
  | Expr ex -> ex
  | Cond c ->
    let temp = newTemporary TYPE_bool
    in let quad_false = Quad_set(Quad_bool("false"), Quad_entry(temp))
    in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
    in let new_quad = Quad_jump (ref (2)) in
    List.iter (fun x -> x := !x + 2) c.q_false;
    {
        code = quad_false :: (new_quad::(quad_true :: c.c_code));
        place = Quad_entry(temp)
    }
  in
    begin
    match assign with
    |"="->
    begin
    let new_quad = 
      match lval.place with
        |Quad_valof (_)
        |Quad_entry (_) -> Quad_set(expr.place,lval.place)  
        | _ -> internal "Assigning to something not an entry";
          raise Terminate
      in 
      {s_code=new_quad::lval.code@expr.code;
      q_break=[];
      q_cont=[]
      }
      end
    |"+="->
    begin
    let temp = newTemporary TYPE_int in                                     
        let plus_quad = Quad_calc("+",expr.place, lval.place, Quad_entry(temp)) in
        let assign_quad = Quad_set(Quad_entry(temp), lval.place) in
        {s_code=assign_quad::plus_quad::lval.code@expr.code;                                    
         q_break=[];                                                               
         q_cont=[]    
        }
    end
    |"-="->
    begin
    let temp = newTemporary TYPE_int in                                     
        let minus_quad = Quad_calc("-", lval.place, expr.place, Quad_entry(temp)) in
        let assign_quad = Quad_set(Quad_entry(temp), lval.place) in
        {s_code=assign_quad::minus_quad::lval.code@expr.code;                                    
         q_break=[];                                                               
         q_cont=[]    
        }
    end
    |"*="->
    begin
    let temp = newTemporary TYPE_int in                                     
        let times_quad = Quad_calc("*", lval.place, expr.place, Quad_entry(temp)) in
        let assign_quad = Quad_set(Quad_entry(temp), lval.place) in
        {s_code=assign_quad::times_quad::lval.code@expr.code;                                    
         q_break=[];                                                               
         q_cont=[]    
        }
    end
    |"%="->
    begin
    let temp = newTemporary TYPE_int in                                     
        let mod_quad = Quad_calc("%", lval.place, expr.place, Quad_entry(temp)) in
        let assign_quad = Quad_set(Quad_entry(temp), lval.place) in
        {s_code=assign_quad::mod_quad::lval.code@expr.code;                                    
         q_break=[];                                                               
         q_cont=[]    
        }
    end
    |"/="->
    begin
    let temp = newTemporary TYPE_int in                                     
        let div_quad = Quad_calc("/", lval.place, expr.place, Quad_entry(temp)) in
        let assign_quad = Quad_set(Quad_entry(temp), lval.place) in
        {s_code=assign_quad::div_quad::lval.code@expr.code;                                    
         q_break=[];                                                               
         q_cont=[]    
        }
    end
    |_ -> return_null_stmt()
  end
(*  else []*)

(* Handle if statement *)
let handle_if_stmt sexpr stmt =
  (* An if statement (without an else) is executed when true. Therefore only the
   * "false" relative jumps are increased by the length of the statement *)
  let cond = 
    match sexpr with
    | Cond condition -> condition
    | Expr expr ->
        (
            let temp = newTemporary TYPE_bool
            in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
            in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
            in handle_comparison "==" sexpr (true_expr) (1, 3)
        )
  in let len = List.length stmt.s_code in
  let len2 = List.length cond.c_code in
  List.iter (fun x -> x := !x + len) cond.q_false;
  List.iter (fun x -> x := !x - len2) stmt.q_cont;
  {
    s_code = stmt.s_code @ cond.c_code;
    q_cont = stmt.q_cont;
    q_break = stmt.q_break
  }

(* Handle if-else statement *)
(* The true condition is executed directly, and then a jump is added to the end
 * of the entire code (including the else-part). The false-refs are increased by 
 * the if-part + 1 (the new jump quad) *)
let handle_if_else_stmt sexpr s1 s2 =
  let cond =
    match sexpr with
    | Cond condition -> condition
    | Expr expr ->
        (
            let temp = newTemporary TYPE_bool
            in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
            in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
            in handle_comparison "==" sexpr (true_expr) (1, 3)
        )
  in
  let l1 = List.length s1.s_code in
  let l2 = List.length s2.s_code in
  let l3 = List.length cond.c_code in
  let new_quad = Quad_jump (ref (l2+1)) in
  List.iter (fun x -> x := !x + l1 + 1) cond.q_false;
  List.iter (fun x -> x := !x + l2 + 1) s1.q_break;
  List.iter (fun x -> x := !x - l3 -1) s1.q_cont;
  List.iter (fun x -> x := !x - l1 - l3 - 2) s2.q_cont;
  {
    s_code = s2.s_code @ (new_quad::(s1.s_code @ cond.c_code));
    q_cont = s1.q_cont @ s2.q_cont;
    q_break = s1.q_break @ s2.q_break
  }


let create_cond_quads expr switch =(
    let rec merge_lists = function
        | ([], [], l) -> l
        | ((h1::t1), (h2::t2), l) -> merge_lists (t1, t2, l@[(h1,h2)])
        | (_, _, _) -> []
    in let create_quads (a, b) = (
            let true_ref = ref 2 in
            let false_ref = ref 2 in
            let code_cond = (Quad_cond("==", expr.place, Quad_int(a), true_ref))
            in let code_false = (Quad_jump (false_ref))
	    in let code_true = (Quad_jump b)
            in (code_true::code_false::[code_cond])
	)
    in let create_and_append_quads acc tuple = 
	let quadlist = create_quads tuple in
	acc @ quadlist
    in let mylist = merge_lists (switch.cond_list, switch.true_list, [])
    in List.fold_left create_and_append_quads [] mylist;
    )

(* Handle switch statement *)
let handle_switch sexpr inner_switch =
     match sexpr with
     | Expr expr ->
	 List.iter (fun x -> x := !x + 1) inner_switch.true_list;
         let cond_quads = create_cond_quads expr inner_switch
	 in let l = List.length inner_switch.code_list
         in 
	 {
             s_code = (inner_switch.code_list)@((Quad_jump (ref (l + 1)))::cond_quads@expr.code);
             q_break = [];
             q_cont = [];
         }
     | Cond cond -> return_null_stmt()

let handle_switch_default sexpr inner_switch default_clause =
    match sexpr with
    | Expr expr ->
        let l= List.length default_clause.s_code in
        let l2 = List.length inner_switch.code_list in
        let cond_quads = create_cond_quads expr inner_switch in
        List.iter (fun x -> x := !x + 1) inner_switch.true_list;
        List.iter (fun x -> x := !x + l) inner_switch.false_list;

        {
         s_code = default_clause.s_code@(inner_switch.code_list@((Quad_jump(ref (l2 + 1)))::(cond_quads@expr.code)));
         q_break = [];
         q_cont = [];
        }
    | Cond cond -> return_null_stmt()

let handle_inner_switch switch_expr body next_switch =
   let l1 = (List.length next_switch.cond_list * 3) in
    let l2 = List.length next_switch.code_list in
    let l3 = List.length body.s_code in
    List.iter (fun x -> x := !x + l1) switch_expr.jump_list;
    List.iter (fun x -> x := !x + l3) next_switch.true_list;
    List.iter (fun x -> x := !x + l2) body.q_break;
    {
    cond_list = (next_switch.cond_list)@(switch_expr.case_list);
    true_list = next_switch.true_list@switch_expr.jump_list;
    code_list = next_switch.code_list@(body.s_code);
    false_list = next_switch.false_list@(body.q_break)
    }


let handle_switch_exp case switch_exp =
    let l = List.length switch_exp.case_list in
    let true_ref=ref ( 1 + 3*l) in 
    {
    case_list = switch_exp.case_list@[case];
    jump_list = switch_exp.jump_list@[true_ref];    
    }


(* Handle for statement *)
let handle_for_stmt indx info body pos=
  let (expr1, expr2, expr3, upordown) = match info with
    |(a,b,c,d) -> (a,b,c,d)
  in
  match expr1, expr2, expr3, indx with
   | Expr startfrom, Expr endto, Expr step, Expr index ->
        begin
            let temp_index = newTemporary TYPE_int in
            let temp_endto = newTemporary TYPE_int in
            let set_start_quad = Quad_set(startfrom.place, Quad_entry(temp_index)) in
            let set_end_quad = Quad_set(endto.place, Quad_entry(temp_endto)) in
            let assign_quad = 
                match index.place with
                |Quad_valof (_)
                |Quad_entry (_) -> Quad_set(Quad_entry(temp_index),index.place)  
                | _ -> internal "Assigning to something not an entry";
                raise Terminate
            in match upordown with
                |"+"-> let comp_quads = handle_comparison "<=" (Expr({code=[];place=Quad_entry(temp_index)})) (Expr({code=[];place=Quad_entry(temp_endto)})) pos in
                       let temp = newTemporary TYPE_int in                                     
                       let plus_quad = Quad_calc("+", step.place, Quad_entry(temp_index), Quad_entry(temp)) in
                       let assign_in_loop_quad = Quad_set(Quad_entry(temp), Quad_entry(temp_index)) in
                       let l1 = List.length body.s_code in
                       let l2 = List.length comp_quads.c_code in
                       let cont_jump = Quad_jump (ref (-l1 -l2 - 3)) in
                       List.iter (fun x -> x := !x + 3) body.q_break;
                       List.iter (fun x -> x := !x + l1) body.q_cont;
                       List.iter (fun x -> x := !x + l1 + 3) comp_quads.q_false;
                         {s_code= cont_jump::assign_in_loop_quad::plus_quad::body.s_code@(comp_quads.c_code@(assign_quad::set_end_quad::(set_start_quad::(step.code@(endto.code@startfrom.code)))));
                            q_break=[];
                            q_cont=[]    
                         }
                |"-" ->let comp_quads = handle_comparison ">=" (Expr({code=[];place=Quad_entry(temp_index)})) (Expr({code=[];place=Quad_entry(temp_endto)})) pos in
                       let temp = newTemporary TYPE_int in                                     
                       let minus_quad = Quad_calc("-", Quad_entry(temp_index), step.place, Quad_entry(temp)) in
                       let assign_in_loop_quad = Quad_set(Quad_entry(temp), Quad_entry(temp_index)) in
                       let l1 = List.length body.s_code in
                       let l2 = List.length comp_quads.c_code in
                       let cont_jump = Quad_jump (ref (-l1 -l2 - 3)) in
                       List.iter (fun x -> x := !x + l1 + 3) comp_quads.q_false;
                       List.iter (fun x -> x := !x + l1 + 3) body.q_break;
                       List.iter (fun x -> x := !x + l1) body.q_cont;

                         {s_code= cont_jump::assign_in_loop_quad::minus_quad::body.s_code@(comp_quads.c_code@(assign_quad::set_end_quad::(set_start_quad::(step.code@(endto.code@startfrom.code)))));
                            q_break=[];
                            q_cont=[]
                         }
                |_ -> return_null_stmt()
        end
   | _ -> return_null_stmt()




(* Handle while statement *)
(* The "false" jumps after all the statements plus the jump to the top. The jump to
 * the top must account for the re-evaluation of the condition *)
let handle_while_stmt sexpr stmt =
  let cond =
  match sexpr with
    | Cond condition -> condition
    | Expr expr ->
        (
            let temp = newTemporary TYPE_bool
            in let quad_true = Quad_set(Quad_bool("true"), Quad_entry(temp))
            in let true_expr = Expr{code = [quad_true]; place=Quad_entry(temp)}
            in handle_comparison "==" sexpr (true_expr) (1, 3)
        )
  in let l = List.length stmt.s_code in
  let lc = List.length cond.c_code in
  List.iter (fun x -> x := !x + l + 1) cond.q_false;
  List.iter (fun x -> x := !x - lc) stmt.q_cont;
  List.iter (fun x -> x := !x + 1) stmt.q_break;
  let new_quad = Quad_jump (ref (-l-lc)) in
  {
    s_code = new_quad :: (stmt.s_code @ cond.c_code);
    q_cont = [];
    q_break = []
  }


(* Handle do while statement *)
let handle_do_while_stmt stmt sexpr = 
  match sexpr with
  | Cond cond ->
  let l = List.length stmt.s_code in
  let lc = List.length cond.c_code in
  List.iter (fun x -> x := !x - l - lc) cond.q_true;
  List.iter (fun x -> x := !x + lc) stmt.q_break;
  List.iter (fun x -> x := !x + l) stmt.q_cont;
  {
  s_code =  (cond.c_code @ stmt.s_code);
  q_cont = [];
  q_break = []  
  }
  | Expr expr -> return_null_stmt()


(* Handle a return expression *)
(* After semantically checking the return types, and set to "$$" - the extra 
 * parameter by reference and then return (Quad_ret) *)
let handle_return_expr expr pos=
  let t = get_type expr.place in
  let fnType =get_entry_type !currentFun in  
  if (equalType t fnType) 
  then let ret_entry = lookupEntry (id_make "$$") LOOKUP_ALL_SCOPES true
    in Quad_ret ::(Quad_set(expr.place, Quad_entry(ret_entry))):: expr.code
  else (
    error "Wrong types in return, \
      in line %d, position %d" 
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )

(* Proc return *)
(* Make sure nothing should be returned and return *)
let handle_return_proc pos =
  if (equalType TYPE_proc (get_entry_type !currentFun))
  then
    [Quad_ret]
  else (
    error "Attemping to return something in a proc, \
      in line %d, position %d"
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )

(* Function definitions *)
(* Wrap the body around unit-endu and add the local definitions at the beginning *)
let handle_func_def ent local_def stmt =
  let s_quad = Quad_unit(ent) in
  let e_quad = Quad_endu(ent) in
  e_quad :: (stmt @ (s_quad :: local_def))

