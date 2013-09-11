%{

open Printf 
open Types
open Identifier
open Symbol

let printTup (a,b) = print_string a; print_string " "; List.iter (printf "%d ") b

let rec printList = function 
  | [] -> ()
  | [(a,b)] -> printTup (a,b)
  | (a::b) -> printTup a ;  printList b;;

(*function to create table type*)

let rec table_type var_type = function 
  | [] -> var_type
  | (a::b) -> let x = table_type var_type b in TYPE_array(x,a)

(*function to register a variable*)
let registerVar var_type (a,b) = ignore(newVariable (id_make a) (table_type var_type b) true)

(*function to register a param*)
let register_param anc (param_type, (name, mode, nlist)) = 
	let var_type = table_type param_type nlist
	in ignore(newParameter (id_make name)var_type mode anc true)

(*function to register a function/proc and its params*)
let registerFun (fun_type,fun_entry) a = ignore(List.map (register_param fun_entry) a); ignore(endFunctionHeader fun_entry fun_type); fun_entry

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
%token T_eq 
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
%token T_mod_equal 
%token T_plus_equal 
%token T_minus_equal 
%token T_div_equal 
%token T_times_equal
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
%token T_real_const
%token T_const_char
%token T_string_const
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
%type <unit> const_inner_def
%type <unit> const_def_list
%type <unit> var_def
%type <(string * int list) list> var_def_list
%type <string * int list> var_init
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
%type <string> const_expr
%type <string> expr
%type <unit> l_value
%type <unit> expr_list
//%type <unit> unop
//%type <unit> binop
%type <unit> call 
%type <unit> expressions
%type <unit> block
%type <unit> inner_block
%type <unit> local_def
%type <unit> stmt
%type <unit> inner_switch 
%type <unit> switch_exp 
%type <unit> pformat_list
%type <unit> assign
%type <unit> range
%type <unit> clause
%type <unit> stmt_list 
%type <unit> write
%type <unit> pformat
%%





pmodule : initialization declaration_list T_eof { () }

initialization : { ignore(initSymbolTable 256) }

declaration_list : /*nothing */ { () }
		|declaration declaration_list { () }

declaration : const_def { () }
	    | var_def { () }
	    | routine { () }
	    | program { () }

const_inner_def : T_name T_eq const_expr { () }

const_def : T_const ptype const_inner_def const_def_list T_semicolon { () }


const_def_list : /*nothing*/ { () }
	       | T_comma const_inner_def const_def_list { () }

var_def : ptype var_init var_def_list T_semicolon { ignore(registerVar $1 $2); ignore(List.map (registerVar $1) $3)   }

var_def_list : /*nothing*/ { [] }
	     | T_comma var_init var_def_list { ($2::$3) }

var_init : T_name { ($1,[]) } 
	 | T_name T_eq expr { ($1,[]) }
	 | T_name var_init_bra_list { ($1,$2) }


var_init_bra_list : T_lbracket const_expr T_rbracket { [int_of_string $2] } //(*make sure to return only int*)
		  | T_lbracket const_expr T_rbracket var_init_bra_list { (int_of_string $2::$4) }

routine_header : routine_header_beg T_lparen routine_header_body T_rparen { registerFun $1 $3 }

routine_header_body :/*nothing*/ { [] } 
		    |ptype formal routine_header_list { (($1,$2)::$3) }

routine_header_list : /*nothing*/ { [] }
		    | T_comma ptype formal routine_header_list { (($2,$3) :: $4) }

routine_header_beg : T_PROC T_name { (TYPE_proc,newFunction (id_make $2) true) }
		   | T_FUNC ptype T_name { ($2,newFunction (id_make $3) true) }

formal : T_name { ($1,PASS_BY_VALUE,[]) }
       | T_ampersand T_name  { ($2,PASS_BY_REFERENCE,[]) }
       | T_name T_lbracket const_expr T_rbracket formal_end { ($1,PASS_BY_REFERENCE,(int_of_string $3::$5)) }
       | T_name T_lbracket T_rbracket formal_end {($1,PASS_BY_REFERENCE,(0::$4)) }

formal_end : /*nothing*/ { [] }
	   | T_lbracket const_expr T_rbracket formal_end { (int_of_string $2::$4) }

routine : routine_header T_semicolon { forwardFunction $1 }
	| routine_header block { () }

program_header : T_PROGRAM T_name T_lparen T_rparen { openScope() }

program : program_header block { closeScope() }

ptype : T_int  { $1 }
      | T_bool { $1 }
      | T_char { $1 }
      | T_REAL { $1 }

const_expr : expr { $1 }

expr : T_int_const { $1 }
     | T_real_const { "test" }
     | T_const_char { "test" }
     | T_string_const { "test" }
     | T_true { "true" }
     | T_false { "flase" }
     | T_lparen expr T_rparen { "test" }
     | l_value { "test" }
     | call { "test" }
     | T_plus expr { "test" }
     | T_minus expr { "test" }
     | T_NOT expr { "test" }
     | T_not expr { "test" } 
     | expr T_plus expr { "test" }
     | expr T_minus expr { "test" }
     | expr T_times expr { "test" }
     | expr T_div expr { "test" }
     | expr T_mod expr { "test" }
     | expr T_MOD expr { "test" }
     | expr T_equal expr { "test" }
     | expr T_not_equal expr { "test" }
     | expr T_greater expr { "test" }
     | expr T_less expr { "test" }
     | expr T_less_equal expr { "test" }
     | expr T_greater_equal expr { "test" }
     | expr T_and expr { "test" }
     | expr T_AND expr { "test" }
     | expr T_OR expr { "test" }
     | expr T_or expr { "test" }

l_value : T_name expr_list { () }

expr_list : /*nothing*/ { () }
	  | T_lbracket expr T_rbracket expr_list { () }
/*
unop : T_plus { () }
     | T_minus { () }
     | T_NOT { () }
     | T_not { () }

binop : T_plus { () }
      | T_minus { () } 
      | T_times { () }
      | T_div { () }
      | T_mod { () }
      | T_MOD { () }
      | T_equal { () }
      | T_not_equal { () }
      | T_greater { () }
      | T_less { () }
      | T_less_equal { () }
      | T_greater_equal { () }
      | T_and { () }
      | T_AND { () } 
      | T_OR { () }
      | T_or { () }
*/

call : T_name T_lparen T_rparen { () }
     | T_name T_lparen expr expressions T_rparen { () }

expressions : /*nothing*/ { () }
	    | T_comma expr expressions { () }

block : T_lbrace inner_block T_rbrace { () }

inner_block : /*nothing*/ { () }
	    | local_def inner_block { () }
	    | stmt inner_block { () }

local_def : const_def { () }
	  | var_def { () }

stmt : T_semicolon { () }
     | l_value assign expr T_semicolon { () }
     | l_value T_plus_plus T_semicolon{ () }
     | l_value T_minus_minus T_semicolon { () }
     | call T_semicolon { () }
     | T_if T_lparen expr T_rparen stmt T_else stmt { () }
     | T_if T_lparen expr T_rparen stmt { () } 
     | T_while T_lparen expr T_rparen stmt { () }
     | T_FOR T_lparen T_name T_comma range T_rparen stmt { () }
     | T_do stmt T_while T_lparen expr T_rparen T_semicolon { () }
     | T_switch T_lparen expr T_rparen T_lbrace inner_switch T_default T_colon clause T_rbrace { () }
     | T_switch T_lparen expr T_rparen T_lbrace inner_switch T_rbrace { () }
     | T_break T_semicolon { () }
     | T_continue T_semicolon { () }
     | T_return T_semicolon { () }
     | T_return expr T_semicolon { () }
     | block { () }
     | write T_lparen T_rparen T_semicolon { () }
     | write T_lparen pformat pformat_list T_rparen T_semicolon { () }


assign : T_eq { () }
       | T_plus_equal { () }
       | T_minus_equal { ()}
       | T_mod_equal { () }
       | T_div_equal { () }
       | T_times_equal { () }

range : expr T_TO expr { () }
      | expr T_TO expr T_STEP expr { () }
      | expr T_DOWNTO expr { () }
      | expr T_DOWNTO expr T_STEP expr { () }

stmt_list : /*nothing*/ { () }
	  | stmt stmt_list { () }

clause : stmt_list { () }
       | stmt_list T_NEXT T_semicolon { () }

inner_switch : /*nothing*/ { () }
	     | switch_exp clause inner_switch { () }

switch_exp : T_case const_expr T_colon  { () }
	   | T_case const_expr %prec T_colon switch_exp { () } 

pformat_list : /*nothing*/ { () }
	     | T_comma pformat pformat_list { () }



write : T_WRITE { () }
      | T_WRITELN { () } 
      | T_WRITESP { () }
      | T_WRITESPLN { () }

pformat : expr { () }
	| T_FORM T_lparen expr T_comma expr T_rparen { () }
	| T_FORM T_lparen expr T_comma expr T_comma expr T_rparen { () }
