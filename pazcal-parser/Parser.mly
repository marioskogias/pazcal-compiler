%token T_and 
%token T_bool 
%token T_break 
%token T_case 
%token T_char 
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
%token T_int 
%token T_MOD 
%token T_NEXT 
%token T_not 
%token T_or 
%token T_PROC 
%token T_PROGRAM 
%token T_REAL 
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
%token  T_eq 
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




%token T_eof

%left T_equal T_not_equal T_less T_greater T_less_equal T_greater_equal

%left T_plus T_minus T_OR T_or
%left T_times T_div T_mod T_MOD T_and T_AND
%start pmodule
%type <unit> pmodule
%type <unit> declaration_list
%type <unit> declaration
%type <unit> const_def
%type <unit> const_inner_def
%type <unit> const_def_list
%type <unit> var_def
%type <unit> var_def_list
%type <unit> var_init
%type <unit> var_init_bra
%type <unit> var_init_bra_list
%type <unit> routine
%type <unit> routine_header
%type <unit> routine_header_beg
%type <unit> routine_header_body
%type <unit> routine_header_list
%type <unit> formal
%type <unit> formal_end
%type <unit> program
%type <unit> ptype
%type <unit> const_expr
%type <unit> expr
%type <unit> l_value
%type <unit> expr_list
%type <unit> unop
%type <unit> binop
%type <unit> call 
%type <unit> expressions
%type <unit> block
%type <unit> inner_block
%type <unit> local_def
%type <unit> stmt
%type <unit> switch_inner 
%type <unit> switch_first_part 
%type <unit> pformat_list
%type <unit> assign
%type <unit> range
%type <unit> clause
%type <unit> stmt_list 
%type <unit> write
%type <unit> pformat
%%





pmodule : declaration_list T_eof { () }

declaration_list : /*nothing */ { () }
		|declaration declaration_list { () }

declaration : const_def { () }
	    | var_def { () }
	    | routine { () }
	    | program { () }

const_inner_def : id T_equal const_expr { () }

const_def : T_const ptype const_inner_def const_def_list T_semicolon { () }


const_def_list : /*nothing*/ { () }
	       | T_comma const_inner_def const_def_list

var_def : ptype var_init var_def_list T_semicolon { () }

var_def_list : /*nothing*/ { () }
	     | T_comma var_init var_def_list { () }

var_init : id { () } 
	 |id T_equal expr { () }
	 | var_init_bra_list { () }

var_init_bra : id T_lbracket const_expr T_rbracket { () }

var_init_bra_list : var_init_bra { () }
		  | var_init_bra_list var_init_bra { () }

routine_header : routine_header_beg id T_lparen routine_header_body T_rparen

routine_header_body : ptype formal | routine_header_list

routine_header_list : /*nothing*/ { () }
		    | T_comma ptype formal routine_header_list

routine_header_beg : T_proc { () }
		   | T_func ptype { () }

formal : id { () }
       | T_ambersand id  { () }
       | id T_lbracket const_expr Trbracket formal_end { () }
       | id T_lbracket Trbracket formal_end { () }

formal_end : /*nothing*/ { () }
	   | T_lbracket const_expr T_rbracket formal_end { () }

routine : routine_header T_semicolon { () }
	| routine_header block { () }

program_header : T_program id T_lparen T_rparen

program : program_header block

ptype : T_init { () }
      | T_bool { () }
      | T_char { () }
      | T_REAL { () }

const_expr : expr { () }

expr : T_int_const { () }
     | T_real_const { () }
     | T_const_char { () }
     | T_string_const { () }
     | T_true { () }
     | T_false { () }
     | T_lparen expr T_rparen { () }
     | l_value { () }
     | call { () }
     | unop expr { () }
     | expr binop expr { () }

l_value : id expr_list { () }

expr_list : /*nothing*/ { () }
	  | T_lbracket expr_list T_rbracket { () }

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
      | T_greate { () }
      | T_less { () }
      | T_less_equal { () }
      | T_greater_equal { () }
      | T_and { () }
      | T_AND { () } /*need to change the lexer */
      | T_OR { () }
      | T_or { () }

call : id T_lparen T_rparen { () }
     | id T_lparen expr expressions T_rparen

expressions : /*nothing*/ { () }
	    | T_comma expr expressions { () }

block : T_lbrace inner_block T_rbrace { () }

inner_block : /*nothing*/ { () }
	    | local_def inner_block { () }
	    | stmt inner_block { () }

local_def : const_def { () }
	  | var_def { () }

stmt : T_semicolon { () }
     | l_value assign T_semicolon { () }
     | l_value T_plus_plue T_semicolon{ () }
     | l_value T_minus_minus T_semicolon { () }
     | call T_semicolon { () }
     | T_if T_lparen expr T_rparen stmt T_else stmt { () }
     | T_if T_lparen expr T_rparen stmt { () }
     | T_while T_lparen expr T_rparen stmt { () }
     | T_for T_lparen id T_comma range T_rparen stmt { () }
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


pformat_list : /**nothing/ { () }
	     | T_comma pformat pformat_list { () }



inner_switch : /*nothing*/ { () }
	     | switch_first_part clause inner_switch { () }

switch_first_part : T_case const_expr T_colon { () }
		  | T_case const_expr T_colon switch_first_part { () }

assign : T_eq { () }
       | T_plus_equal { () }
       | T_minus_equal { ()}
       | T_mod_equal { () }
       | T_div_equal { () }
       | T_times_equal { () } /*add this to the lexer*/

range : expr T_TO expr { () }
      | expr T_TO expr T_STEP expr
      | expr T_DOWNTO expr { () }
      | expr T_DOWNTO expr T_STEP expr

clause : stmt_list T_break T_semicolon { () }
       | stmt_list T_NEXT T_semicolon { () }

stmt_list : /*nothing*/ { () }
	  | stmt stmt_list { () }

write : T_WRITE { () }
      | T_WRITELN { () } 
      | T_WRITESP { () }
      | T_WRITESPLN { () }

pformat : expr { () }
	| T_FORM T_lparen expr T_comma expr T_rparen { () }
	| T_FORM T_lparen expr T_comma expr T_comma expr T_rparen { () }
