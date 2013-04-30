%token T_print
%token T_let
%token T_for
%token T_do
%token T_begin
%token T_end
%token T_if
%token T_then

%token T_const
%token T_var

%token T_eq
%token T_rparen
%token T_lparen
%token T_plus
%token T_minus
%token T_times

%token T_eof

%left T_plus T_minus
%left T_times

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
