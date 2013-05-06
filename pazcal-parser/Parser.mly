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

(* old stuff
%start program
%type <unit> program
%type <unit> stmt_list
%type <unit> stmt
%type <unit> expr

%%

program   : stmt_list T_eof { () }

stmt_list : /* nothing */ { () }
          | stmt stmt_list { () }

stmt      : T_print expr { () }
          | T_let T_var T_eq expr { () }
  	  | T_for expr T_do stmt { () }
	  | T_begin stmt_list T_end { () }
	  | T_if expr T_then stmt { () }

expr      : T_const { () }
          | T_var { () }
	  | T_lparen expr T_rparen { () }
	  | expr T_plus expr { () }
	  | expr T_minus expr { () }
	  | expr T_times expr { () }
*)

(*declarations*)

%start pmodule
%type <unit> pmodule
%type <unit> declaration_list
%type <unit> declaration
%type <unit> const_def
%type <unit> var_def
%type <unit> routine
%type <unit> program
%%

(*rules*)




pmodule : declaration_list T_eof { () }

declaration_list : /*nothing */ { () }
		|declaration declaration_list { () }

declaration : const_def { () }
	    | var_def { () }
	    | routine { () }
	    | program { () }

const_def : T_const ptype id T_equal const_expr (T_comma id T_equal const_expr)* T_semicolon { () }

