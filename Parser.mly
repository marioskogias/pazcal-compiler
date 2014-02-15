%{

open Printf
open Types
open Identifier
open Symbol
open Semantic 
open Error
open Parsing
open Output
open QuadTypes
open Quads


let printTup (a,b) = print_string a; print_string " "; List.iter (printf "%d ") b

let rec printList = function 
  | [] -> ()
  | [(a,b)] -> printTup (a,b)
  | (a::b) -> printTup a ;  printList b;;

(*function to create table type*)

let rec table_type var_type = function 
  | [] -> var_type
  | (a::b) -> let x = table_type var_type b in TYPE_array(x,a)

(* Simple Function to get Expression Position *)
let get_binop_pos () = (rhs_start_pos 1, rhs_start_pos 3)

(*function to register a variable*)
let registerVar var_type place (a,b,c) = (*match c with
                                         | Expr(e) -> match e.place with
                                                        | Quad_none -> ignore(newVariable (id_make a) (table_type var_type b) true); return_null_stmt()
                                                        | _ -> let quad_e = Expr({code=[]; 
                                                                                  place=Quad_entry(newVariable (id_make a) (table_type var_type b) true)})
                                                                in handle_assignment (dereference quad_e) c place
                                         |_ -> return_null_stmt()*)
                        
                                         let quad_e = Expr({code=[]; place=Quad_entry(newVariable (id_make a) (table_type var_type b) true)})
                                            in handle_assignment "=" (dereference quad_e) c place
(*function to register a const*)
let registerConst var_type (a,v) =  ignore(newConst (id_make a) var_type v true)

(*function to register a param*)
let register_param anc (param_type, (name, mode, nlist)) = 
	let var_type = table_type param_type nlist
	in ignore(newParameter (id_make name)var_type mode anc true)

(*function to register a function/proc and its params*)
let registerFun (fun_type,fun_entry) a = ignore(List.map (register_param fun_entry) a); ignore(endFunctionHeader fun_entry fun_type); fun_entry

(*function to get variable's type*)
let rec get_var_type = function
    |(var_type, 0) -> var_type
    |(TYPE_array (t,s), a) -> get_var_type (t, a-1)
    |_ -> ignore(error "tables sizes"); TYPE_none

(*function to check entry is const*)
let is_const name = 
    let e = lookupEntry  (id_make name) LOOKUP_ALL_SCOPES true 
        in
        match e.entry_info with
        | ENTRY_variable inf ->  inf.is_const
        | _ -> false

(*function to get a cont val*)
let get_const_val name = 
    let e = lookupEntry  (id_make name) LOOKUP_ALL_SCOPES true 
        in
        match e.entry_info with
        | ENTRY_variable inf ->  inf.value
        | _ -> "0"

(*function to get entry's name*)
let get_name e = id_name e.entry_id 

(*handle tuples with 3 elements*)
let first_el (a,_,_) = a

let second_el (_,b,_) = b

let third_el (_,_,c) = c

let first (a,_,_,_) = a
let second (_,b,_,_) = b
let third (_,_,c,_) = c
let fourth (_,_,_,d) = d

(*get the parameter list of a function as it is in the symbol table*)
let get_param_list a = 
	match a.entry_info with 
	| ENTRY_function inf -> inf.function_paramlist 
	| _ -> []

(*function to evaulate expression*)

let eval_expr a b op = 
    try
        let a_val = int_of_string a in
        let b_val = int_of_string b in
            match op with
            | "+" -> string_of_int (a_val + b_val)
            | "-" ->  string_of_int (a_val - b_val)
            | "*" ->  string_of_int (a_val * b_val)
            | "/" ->  string_of_int (a_val / b_val)
            | "mod" ->  string_of_int (a_val mod b_val)
        with Failure "int_of_string" -> "this is not a number" (*change this*)

%}


%token T_and 
%token <Types.typ> T_bool 
%token T_break 
%token T_case 
%token <Types.typ> T_char 
%token T_const 
%token T_continue 
%token T_default 
%token T_do 
%token T_DOWNTO 
%token T_else 
%token T_false 
%token T_FOR 
%token T_FORM 
%token T_FUNC 
%token T_if 
%token <Types.typ> T_int 
%token T_MOD 
%token T_NEXT 
%token T_not 
%token T_or 
%token T_PROC 
%token T_PROGRAM 
%token <Types.typ> T_REAL 
%token T_return 
%token T_STEP 
%token T_switch 
%token T_TO 
%token T_true 
%token T_while 
%token T_WRITE 
%token T_WRITELN
%token T_WRITESP 
%token T_WRITESPLN 
%token <string> T_eq 
%token T_lparen 
%token T_rparen 
%token T_plus 
%token T_minus 
%token T_times 
%token T_equal 
%token T_greater 
%token T_less 
%token T_less_equal 
%token T_greater_equal 
%token T_not_equal 
%token T_mod 
%token <string> T_mod_equal 
%token <string> T_plus_equal 
%token <string> T_minus_equal 
%token <string> T_div_equal 
%token <string> T_times_equal
%token T_minus_minus 
%token T_plus_plus 
%token T_OR 
%token T_AND 
%token T_NOT 
%token T_div 
%token T_ampersand 
%token T_semicolon 
%token T_fullstop 
%token T_colon
%token T_comma
%token T_lbracket
%token T_rbracket 
%token T_lbrace
%token T_rbrace 
%token <string> T_name
%token <string> T_real_const
%token <string> T_const_char
%token <string> T_string_const
%token <string> T_int_const

%token T_eof

%left T_equal T_not_equal T_less T_greater T_less_equal T_greater_equal

%left T_plus T_minus T_OR T_or
%left T_times T_div T_mod T_MOD T_and T_AND

%right T_not T_NOT

%start pmodule
%type <unit> pmodule
%type <unit> declaration_list
%type <unit> declaration
%type <unit> const_def
%type <string * string > const_inner_def //(*name, val, thanasis*)
%type <(string * string) list> const_def_list
%type <QuadTypes.stmt_ret_type> var_def
%type <(string * int list * QuadTypes.superexpr) list> var_def_list
%type <string * int list * QuadTypes.superexpr> var_init
%type <int list> var_init_bra_list
%type <unit> routine
%type <entry> routine_header
%type <Types.typ * entry> routine_header_beg
%type <(Types.typ * (string * Symbol.pass_mode * int list)) list> routine_header_body
%type <(Types.typ * (string * Symbol.pass_mode * int list)) list> routine_header_list
%type <string * Symbol.pass_mode * int list> formal
%type <int list> formal_end
%type <unit> program
%type <Types.typ> ptype
%type <Types.typ * string> const_expr
%type <Types.typ * string * QuadTypes.superexpr> expr
%type <Types.typ * string * QuadTypes.superexpr> l_value
%type <int> expr_list
//%type <unit> unop
//%type <unit> binop
//%type <Types.typ * string * string> call 
%type <QuadTypes.superexpr> call 
%type <QuadTypes.superexpr  list> expressions
%type <QuadTypes.stmt_ret_type> block
%type <QuadTypes.stmt_ret_type> inner_block
%type <QuadTypes.stmt_ret_type> local_def
%type <QuadTypes.stmt_ret_type> stmt
%type <QuadTypes.inner_switch_ret_type> inner_switch
%type <QuadTypes.switch_exp_ret_type> switch_exp
%type <unit> pformat_list
%type <string> assign
%type <QuadTypes.superexpr*QuadTypes.superexpr*QuadTypes.superexpr*string>range
%type <QuadTypes.stmt_ret_type> clause
%type <QuadTypes.stmt_ret_type> stmt_list 
%type <unit> write
%type <unit> pformat
%%





pmodule : initialization declaration_list T_eof { () }

initialization : { ignore(initSymbolTable 256);  openScope()}

declaration_list : /*nothing */ { () }
|declaration declaration_list { () }

declaration : const_def { () }
	    | var_def { () }
	    | routine { () }
	    | program { () }

const_inner_def : T_name T_eq const_expr { ($1, (snd $3)) } 

const_def : T_const ptype const_inner_def const_def_list T_semicolon { ignore(registerConst $2 $3); ignore(List.map (registerConst $2) $4) }


const_def_list : /*nothing*/ { [] }
	       | T_comma const_inner_def const_def_list { $2::$3 }

var_def : ptype var_init var_def_list T_semicolon { let stmt = registerVar $1 (get_binop_pos()) $2 in 
                                                    let list_stmt = List.map (registerVar $1 (get_binop_pos())) $3 in
                                                    let rec help_merge stmt = function 
                                                            | [] -> stmt
                                                            |a::b -> help_merge (handle_stmt_merge a stmt) b
                                                    in help_merge stmt list_stmt
                                                 }

var_def_list : /*nothing*/ { [] }
	     | T_comma var_init var_def_list { ($2::$3) }

var_init : T_name { ($1,[], Expr(return_null())) } 
	    | T_name T_eq expr { ($1,[],third_el $3)  }
	    | T_name var_init_bra_list { ($1,$2, Expr(return_null())) }


var_init_bra_list : T_lbracket const_expr T_rbracket { [table_size (fst $2) (snd $2) (rhs_start_pos 1)] } //(*make sure to return only int*)
		  | T_lbracket const_expr T_rbracket var_init_bra_list { (table_size (fst $2) (snd $2) (rhs_start_pos 1)::$4) }

routine_header : routine_header_beg T_lparen routine_header_body T_rparen { registerFun $1 $3 }

routine_header_body :/*nothing*/ { [] } 
		    |ptype formal routine_header_list { (($1,$2)::$3) }

routine_header_list : /*nothing*/ { [] }
		    | T_comma ptype formal routine_header_list { (($2,$3) :: $4) }

routine_header_beg : T_PROC T_name { let a = (TYPE_proc,newFunction (id_make $2) true) in ignore(openScope());a }
		   | T_FUNC ptype T_name { let a = ($2,newFunction (id_make $3) true) in ignore(openScope());a }

formal : T_name { ($1,PASS_BY_VALUE,[]) }
       | T_ampersand T_name  { ($2,PASS_BY_REFERENCE,[]) }
       | T_name T_lbracket const_expr T_rbracket formal_end { ($1,PASS_BY_REFERENCE,(table_size (fst $3) (snd $3) (rhs_start_pos 1)::$5)) }
       | T_name T_lbracket T_rbracket formal_end {($1,PASS_BY_REFERENCE,(0::$4)) }

formal_end : /*nothing*/ { [] }
	   | T_lbracket const_expr T_rbracket formal_end { (table_size (fst $2) (snd $2) (rhs_start_pos 1)::$4) }

routine : routine_header T_semicolon closeScope { forwardFunction $1 }
	| routine_header block closeScope { () }

program_header : T_PROGRAM T_name T_lparen T_rparen { () }

program : openScope program_header block closeScope {  ignore(List.map print_string (List.map string_of_quad_t $3.s_code)) }

openScope : { openScope() }

closeScope : { closeScope() }

ptype : T_int  { $1 }
      | T_bool { $1 }
      | T_char { $1 }
      | T_REAL { $1 }

const_expr : expr { ((first_el $1), (second_el $1)) } 


expr:  T_int_const { (TYPE_int,$1, Expr( {code=[]; place= Quad_int ($1)})) }
    | T_real_const { (TYPE_real,$1, Expr(return_null())) }
    | T_const_char { (TYPE_char,$1 , Expr(return_null())) }
    | T_string_const { (TYPE_array (TYPE_char,0),$1, Expr(return_null())) }
    | T_true { (TYPE_bool,"true", Expr(return_null())) }
    | T_false { (TYPE_bool,"false", Expr(return_null())) }
    | T_lparen expr T_rparen { ((first_el $2),"test", third_el $2) }


     | l_value { if (is_const (second_el $1))
                    then ((first_el $1), get_const_val (second_el $1), Expr(return_null ()))
                 else $1 }
     | call { let c = match $1 with
                      | Expr e -> e
               in (get_type c.place,"test", $1) 
            }

     | T_plus expr { (check_is_number (first_el $2) (rhs_start_pos 1), "test", Expr(handle_unary_expression "+" (third_el $2) (rhs_start_pos 2)))}
     | T_minus expr { (check_is_number (first_el $2) (rhs_start_pos 1), "test", Expr(handle_unary_expression "-" (third_el $2) (rhs_start_pos 2)))}
     | T_NOT expr { (check_is_bool (first_el $2) (rhs_start_pos 1), "test", Cond(handle_not (third_el $2))) }
     | T_not expr { (check_is_bool (first_el $2) (rhs_start_pos 1), "test", Cond(handle_not (third_el $2)))  }
     | expr T_plus expr { (check_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                           eval_expr (second_el $1) (second_el $3) "+", 
                           Expr(handle_expression "+" (third_el $1) (third_el $3) (get_binop_pos())))}
     | expr T_minus expr { (check_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                            eval_expr (second_el $1) (second_el $3) "-", 
                            Expr(handle_expression "-" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_times expr { (check_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                            eval_expr (second_el $1) (second_el $3) "*", 
                            Expr(handle_expression "*" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_div expr { (check_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1), 
                          eval_expr (second_el $1) (second_el $3) "/", 
                          Expr(handle_expression "/" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_mod expr { (check_int_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                          eval_expr (second_el $1) (second_el $3) "mod", 
                          Expr(handle_expression "%" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_MOD expr { (check_int_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                          eval_expr (second_el $1) (second_el $3) "mod", 
                          Expr(handle_expression "%" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_equal expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                            "test", 
                            Cond(handle_comparison "==" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_not_equal expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                                "test", 
                                Cond(handle_comparison "!=" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_greater expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                              "test", 
                              Cond(handle_comparison ">" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_less expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                           "test", 
                           Cond(handle_comparison "<" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_less_equal expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                                 "test", 
                                 Cond(handle_comparison "<=" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_greater_equal expr { (check_equalities (first_el $1) (first_el $3) (rhs_start_pos 1),
                                    "test", 
                                    Cond(handle_comparison ">=" (third_el $1) (third_el $3) (get_binop_pos()))) }
     | expr T_and expr { (check_bool_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                          "test", 
                          Cond(handle_and (third_el $1) (third_el $3))) }
     | expr T_AND expr { (check_bool_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),
                          "test", 
                          Cond(handle_and (third_el $1) (third_el $3))) }
     | expr T_OR expr { (check_bool_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),"test", Cond(handle_or (third_el $1) (third_el $3))) }
     | expr T_or expr { (check_bool_binop_types (first_el $1) (first_el $3) (rhs_start_pos 1),"test", Cond(handle_or (third_el $1) (third_el $3))) }


l_value : T_name expr_list { let e = lookupEntry  (id_make $1) LOOKUP_ALL_SCOPES true 
                                in (get_var_type ((get_type (Quad_entry e)), $2),get_name e,Expr({code=[];place=(Quad_entry (e))}))}
                                

expr_list : /*nothing*/ { 0 }
	  | T_lbracket expr T_rbracket expr_list { $4 + 1 }

call : T_name T_lparen T_rparen {  (*lookupEntry  (id_make $1) LOOKUP_ALL_SCOPES true *) Expr(return_null()) }
     | T_name T_lparen expr expressions T_rparen {  let e = lookupEntry  (id_make $1) LOOKUP_ALL_SCOPES true 
                                                    in let get_expr e = 
                                                        match e with
                                                        | Expr e -> e
                                                        | _ -> return_null()
                                                    in let get_place e = e.place
                                                    in let expr_list = List.map get_expr (third_el $3::$4)
                                                    in let expr_types = List.map get_type (List.map get_place expr_list) 
                                                    in ignore(check_function_params (get_param_list e) expr_types (rhs_start_pos 1)) ;
                                                        Expr(handle_func_call e (rhs_start_pos 1) expr_list )
                                                 }

expressions : /*nothing*/ { [] }
	    | T_comma expr expressions { third_el $2::$3 }

block : T_lbrace  inner_block T_rbrace { $2 }

inner_block : /*nothing*/ { return_null_stmt () }

	    | local_def inner_block { handle_stmt_merge $1 $2 }
	    | stmt inner_block {  handle_stmt_merge $1 $2 }

local_def : const_def { return_null_stmt() }
	  | var_def { $1 }

stmt : T_semicolon { return_null_stmt () }
     | l_value assign expr T_semicolon {if (is_const (second_el $1)) 
                                            then print_error "Assign to a const variable" (rhs_start_pos 1)
                                        else
                                            ignore(check_assign $2 (first_el $1) (first_el $3)  (rhs_start_pos 1)); 
                                            handle_assignment $2 (dereference (third_el $1)) (third_el $3) (get_binop_pos())}
     | l_value T_plus_plus T_semicolon{if (is_const (second_el $1)) 
                                            then print_error "Assign to a const variable" (rhs_start_pos 1) 
                                       else
                                           ignore(check_assign "+=" (first_el $1) (first_el $1) (rhs_start_pos 1));
                                           return_null_stmt () } /*same as above same operant*/
     | l_value T_minus_minus T_semicolon {if (is_const (second_el $1)) 
                                            then print_error "Assign to a const variable" (rhs_start_pos 1)
                                          else
                                             ignore(check_assign "-=" (first_el $1) (first_el $1) (rhs_start_pos 1));
                                             return_null_stmt () }
     | call T_semicolon { handle_expr_to_stmt $1 }
     | T_if T_lparen expr T_rparen stmt T_else stmt { ignore(check_is_bool (first_el $3)  (rhs_start_pos 1));
                                                      handle_if_else_stmt (third_el $3) $5 $7 }
     | T_if T_lparen expr T_rparen stmt { ignore(check_is_bool (first_el $3)  (rhs_start_pos 1)); 
                                          handle_if_stmt (third_el $3) $5 } 
     | T_while stoppable T_lparen expr T_rparen stmt { (ignore(check_is_bool (first_el $4)  (rhs_start_pos 1)) ; in_loop := !in_loop -1);
                                                        handle_while_stmt (third_el $4) $6 }
     | T_FOR stoppable T_lparen T_name T_comma range T_rparen stmt { in_loop := !in_loop - 1 ; 
                                                                    let e = lookupEntry (id_make $4) LOOKUP_ALL_SCOPES true in (
                            handle_for_stmt (Expr({code=[];place=(Quad_entry (e))})) (first $6) (second $6) (third $6) (fourth $6) $8 (get_binop_pos())) 
                                                                    }
     | T_do stoppable stmt T_while T_lparen expr T_rparen T_semicolon { (ignore(check_is_bool (first_el $6)  (rhs_start_pos 1)) ; in_loop := !in_loop - 1);
                                                                         handle_do_while_stmt $3 (third_el $6) }
     | T_switch stoppable T_lparen expr T_rparen T_lbrace inner_switch T_default T_colon clause T_rbrace { in_loop := !in_loop - 1 ; handle_switch_default (third_el $4) $7 $10}
     | T_switch stoppable T_lparen expr T_rparen T_lbrace inner_switch T_rbrace { in_loop := !in_loop - 1; handle_switch (third_el $4) $7 }
     | T_break T_semicolon { ignore(if (!in_loop <= 0) then print_error "break not in loop" (rhs_start_pos 1)); handle_break() }
     | T_continue T_semicolon { ignore(if (!in_loop <= 0) then print_error "continue not in loop" (rhs_start_pos 1)); handle_continue() }
     | T_return T_semicolon { return_null_stmt() }
     | T_return expr T_semicolon { handle_expr_to_stmt (third_el $2) }
     | openScope block closeScope { $2 }
     | write T_lparen T_rparen T_semicolon { return_null_stmt() }
     | write T_lparen pformat pformat_list T_rparen T_semicolon { return_null_stmt() }

stoppable : {in_loop := !in_loop + 1}

assign : T_eq { $1 }
       | T_plus_equal { $1 }
       | T_minus_equal { $1 }
       | T_mod_equal { $1 }
       | T_div_equal { $1 }
       | T_times_equal { $1 }

range : expr T_TO expr { (third_el $1, third_el $3, Expr({ code=[]; place= Quad_int ("1")}), "+") }
      | expr T_TO expr T_STEP expr { (third_el $1, third_el $3, third_el $5, "+") }
      | expr T_DOWNTO expr { (third_el $1, third_el $3, Expr({ code=[]; place= Quad_int ("1")}), "-") }
      | expr T_DOWNTO expr T_STEP expr { (third_el $1, third_el $3, third_el $5, "-") }

stmt_list : /*nothing*/ { return_null_stmt() }
      | stmt_list T_break T_semicolon {let jump_ref = ref 1 in {s_code = (Quad_jump(jump_ref))::($1.s_code); q_cont = $1.q_cont; q_break =(jump_ref)::($1.q_break)  }}
      | stmt_list stmt { handle_stmt_merge $1 $2 }

clause : stmt_list { ($1) }
       | stmt_list T_NEXT T_semicolon { ($1) }
inner_switch : /*nothing*/ { {cond_list=[]; code_list=[]; true_list=[]; false_list=[]} }
       | switch_exp clause inner_switch { handle_inner_switch $1 $2 $3 }

switch_exp : T_case const_expr T_colon  { {case_list=[(snd $2)]; jump_list=[ref 1]} }
       | T_case const_expr T_colon switch_exp { handle_switch_exp (snd $2) $4 }

pformat_list : /*nothing*/ { () }
	     | T_comma pformat pformat_list { () }

write : T_WRITE { () }
      | T_WRITELN { () } 
      | T_WRITESP { () }
      | T_WRITESPLN { () }

pformat : expr { () }
	| T_FORM T_lparen expr T_comma expr T_rparen { () }
	| T_FORM T_lparen expr T_comma expr T_comma expr T_rparen { () }
