open Types
open Error
open Symbol
open Identifier
open Semantic
open Lexing
open QuadTypes

(* Get Type of a quad_elem_t *)
let get_type = function
  |Quad_none -> TYPE_none
  |Quad_int (_) -> TYPE_int
  |Quad_real (_) -> TYPE_real
  |Quad_char(_) -> TYPE_char
  |Quad_string (str) -> TYPE_array(TYPE_char, String.length str)
  |Quad_valof (ent) 
  |Quad_entry (ent) -> 
    match ent.entry_info with
    |ENTRY_none -> TYPE_none
    |ENTRY_variable (info) -> info.variable_type
    |ENTRY_parameter (info) -> info.parameter_type
    |ENTRY_function (info) -> info.function_result
    |ENTRY_temporary (info) -> info.temporary_type
(* function to get entry's type)                                        
let get_type e =                                                        
  match e.entry_info with                                                 
  | ENTRY_variable inf -> inf.variable_type                               
  | ENTRY_parameter inf -> inf.parameter_type                             
  | ENTRY_function inf -> inf.function_result                             
   (*to be continued...*)                                                  
  | _ ->TYPE_int
*)
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
 

(* Handling [x] case *)
let dereference x = 
  match x with 
  | Expr ex ->
    begin
      match ex.code with
      |(Quad_array(_, _, ent)::_)->
        {ex with place = Quad_valof(ent)}
      |_ -> ex
    end
  | Cond cond -> return_null () 

(* Get Type of a quad_elem_t *)

(* Get Size Description from a quad_elem_t *)
(* Extract the Entry from a quad_elem_t *)
let extract_entry = function
  |Quad_entry (ent) -> ent
  |Quad_valof (ent) -> ent
  |_ -> internal "Not an entry"; raise Terminate

(* Get a string description of a quad_elem_t *)
let get_id = function
  |Quad_none -> internal "proc func call"; raise Terminate
  |Quad_int (i) -> i
  |Quad_real (r) -> r
  |Quad_char (c) -> c
  |Quad_string (s) -> s
  |Quad_valof (ent)
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
  |Quad_tailCall ent ->
    Printf.sprintf "tailRecursiveCall, -, -, %s\n"
      (id_name ent.entry_id)
  |Quad_call (ent,_) ->
    Printf.sprintf "call, -, -, %s\n"
      (id_name ent.entry_id)
(*  |Quad_par(q,pm) ->
    Printf.sprintf "par, %s, %s, -"
      (string_of_quad_elem_t q)
      (string_of_pass_mode pm)
*)  |Quad_ret -> "ret, -, -, -\n" 
  |Quad_dummy -> ""



(* ----------------------------------------------------------------------------- *)

(* Functions to generate intermediate code in the parser *)

(* IMPORTANT: Intermediate code in the lists must be inverted *)

let handle_expr_to_stmt sexpr =
  match sexpr with 
  |Expr expr -> {s_code= expr.code; q_break=[]; q_cont=[]}
  |Cond cond -> return_null_stmt()


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
let handle_expression op expr1 expr2 (sp,ep) =
  match expr1, expr2 with
  |  Expr e1, Expr e2 ->
    let t1 = get_type e1.place in
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

(* Handle L-Values *)

(* Non-array l-value needs no code 
let handle_simple_lvalue id pos =
  let (ent, _, correct) = check_lvalue id pos false in
  if (correct) 
    then {code = []; place = Quad_entry(ent)}
  else return_null ()
*)
(* Handle an array lvalue 
 * Array lvalue needs to be dereferenced 
let handle_array_lvalue id pos context q_t =
  let t = get_type q_t.place in
  if (t==TYPE_int) 
    then let (ent, l_typ, correct) = check_lvalue id pos true in
    if (correct) 
      (* The new temporary created is a Pointer to l_typ *)
      then let temp = newTemporary (TYPE_pointer l_typ)  in
      let new_quad =
         Quad_array(Quad_entry(ent), q_t.place, temp) in
      {code = new_quad::q_t.code ; place = Quad_entry(temp)}
    else return_null ()
  else let sp = fst context and ep = snd context in  
    error "Array index must be an integer in expression starting \
      at line %d, position %d and ending at line %d, position %d."
    (sp.pos_lnum) (sp.pos_cnum - sp.pos_bol)
    (ep.pos_lnum) (ep.pos_cnum - ep.pos_bol);
    return_null ()
*)


(* Ugliest function yet - Handle function calls *)
(*
let handle_func_call id pos expr_list =

  (* Get function entry from id *)
  let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
  
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
          let new_quad = Quad_par (hp, par_info.parameter_mode) in
          if par_info.parameter_mode = PASS_BY_REFERENCE 
          then check_param_by_reference hp id;
          create_par_quads (new_quad::acc) (tfi, tp)
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
    | ((h::t), (hp::tp)) -> reverse_code_list (hp::h@acc) (t,tp) 
    | _ -> internal "Uneven args and code"; raise Terminate in

  
  (* Extract expr_list information *)        
  let (code_list, param_list, type_list) = 
    unzip_expr_list [] [] []  expr_list in

  
  match ent.entry_info with
  |ENTRY_function (info) ->
    (* Check for semantic correctness *)
    (*if (check_func_call info id type_list pos)
    then (
  *)
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
      | TYPE_char -> 
        let temp = newTemporary info.function_result in 
        let ret_place = Quad_entry temp in
        let par_q = Quad_par ( ret_place , PASS_RET) in {
          code = Quad_call(ent,(param_list@[ret_place]))::par_q::entire_code;
          place = Quad_entry(temp)
        }
      | _ -> return_null ()         
    (*  )
    else 
      return_null () *)
  |_ ->   
    error "Invalid Function call. Identifier %s is not a function \
      at line %d, position %d."
      id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    return_null ()
*)  
(* Handle Comparisons *)
let handle_comparison op exp1 exp2 (sp,ep) =
  (* First Check the types of the compared things *)
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
let handle_and cond1 cond2 =
  (* The "next" quad will be left unchanged for c2 but c1 will point |c2.code| 
   * later. For immediate evaluation when c1 is false we need to go the end 
   * of everything, when c1 is true we need to evaluate c2. *)
  match cond1, cond2 with
  | Cond c1, Cond c2 ->
    let len = List.length c2.c_code in
      List.iter (fun x -> x := !x + len) c1.q_false;
      { 
        c_code = c2.c_code @ c1.c_code;
        q_true = c2.q_true;
        q_false = c1.q_false @ c2.q_false;
      }
  | _ -> return_null_cond()

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
  | _ -> return_null_cond()

(* Handle assignmenet *)
let handle_assignment lval exp (sp,ep) =
 (* let t1 = get_type lval.place in
  let t2 = get_type expr.place in
  if (check_types "=" t1 t2 sp ep) 
  then*) 
  match exp with
  | Expr expr ->
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
  | Cond cond -> return_null_stmt() 
(*  else []*)

(* Handle if statement *)
let handle_if_stmt sexpr stmt =
  (* An if statement (without an else) is executed when true. Therefore only the
   * "false" relative jumps are increased by the length of the statement *)
  match sexpr with
  | Cond cond ->
  let len = List.length stmt.s_code in
  let len2 = List.length cond.c_code in
  List.iter (fun x -> x := !x + len) cond.q_false;
  List.iter (fun x -> x := !x - len2) stmt.q_cont;
  {
  s_code = stmt.s_code @ cond.c_code;
  q_cont = stmt.q_cont;
  q_break = stmt.q_break
  }
  | Expr expr -> return_null_stmt()

(* Handle if-else statement *)
(* The true condition is executed directly, and then a jump is added to the end
 * of the entire code (including the else-part). The false-refs are increased by 
 * the if-part + 1 (the new jump quad) *)
let handle_if_else_stmt sexpr s1 s2 =
  match sexpr with
  | Cond cond ->
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
  | Expr expr -> return_null_stmt()


(* Handle while statement *)
(* The "false" jumps after all the statements plus the jump to the top. The jump to
 * the top must account for the re-evaluation of the condition *)
let handle_while_stmt sexpr stmt =
  match sexpr with
  | Cond cond ->
  let l = List.length stmt.s_code in
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
  | Expr exp -> return_null_stmt()


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
(*let handle_return_expr expr pos=
  let t = get_type expr.place in
  if (equalType t !currentScope.sco_ret_type) 
  then let ret_entry = lookupEntry (id_make "$$") LOOKUP_CURRENT_SCOPE true
    in Quad_ret ::(Quad_set(expr.place, Quad_entry(ret_entry))):: expr.code
  else (
    error "Attempting to return %s when %s was expected, \
      in line %d, position %d" 
      (string_of_typ t) (string_of_typ !currentScope.sco_ret_type)
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )

(* Proc return *)
(* Make sure nothing should be returned and return *)
let handle_return_proc pos =
  if (equalType TYPE_proc !currentScope.sco_ret_type)
  then
    [Quad_ret]
  else (
    error "Attemping to return proc when %s was expected, \
      in line %d, position %d"
      (string_of_typ !currentScope.sco_ret_type)
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )
*)
(* Function definitions *)
(* Wrap the body around unit-endu and add the local definitions at the beginning *)
let handle_func_def id local_def stmt =
  let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
  let s_quad = Quad_unit(ent) in
  let e_quad = Quad_endu(ent) in
  e_quad :: (stmt @ (s_quad :: local_def))

