(* Symbol table *)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE | PASS_RET

type param_status =
  | PARDEF_COMPLETE                             (* ĞëŞñçò ïñéóìüò     *)
  | PARDEF_DEFINE                               (* Åí ìİóù ïñéóìïı    *)
  | PARDEF_CHECK                                (* Åí ìİóù åëİã÷ïõ    *)

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int
}

and variable_info = {                         (******* ÌåôáâëçôŞ *******)
  variable_type   : Types.typ;                (* Ôığïò                 *)
  variable_offset : int;                       (* Offset óôï Å.Ä.       *)
  is_const : bool;
  mutable value : string
}

and function_info = {                         (******* ÓõíÜñôçóç *******)
  mutable function_isForward : bool;          (* ÄŞëùóç forward        *)
  mutable function_paramlist : entry list;    (* Ëßóôá ğáñáìİôñùí      *)
  mutable function_redeflist : entry list;    (* Ëßóôá ğáñáìİôñùí (2ç) *)
  mutable function_result    : Types.typ;     (* Ôığïò áğïôåëİóìáôïò   *)
  mutable function_pstatus   : param_status;  (* ÊáôÜóôáóç ğáñáìİôñùí  *)
  mutable function_initquad  : int;            (* Áñ÷éêŞ ôåôñÜäá        *)
  mutable function_scope     : scope            (* Áñ÷éêŞ ôåôñÜäá        *)
}

and parameter_info = {                        (****** ĞáñÜìåôñïò *******)
  parameter_type           : Types.typ;       (* Ôığïò                 *)
  mutable parameter_offset : int;             (* Offset óôï Å.Ä.       *)
  parameter_mode           : pass_mode        (* Ôñüğïò ğåñÜóìáôïò     *)
}

and temporary_info = {                        (** ĞñïóùñéíŞ ìåôáâëçôŞ **)
  temporary_type   : Types.typ;               (* Ôığïò                 *)
  temporary_offset : int;                     (* Offset óôï Å.Ä.       *)
  mutable temp_value    : string
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

val currentScope : scope ref              (* Ôñİ÷ïõóá åìâİëåéá         *)
val quadNext : int ref                    (* Áñéèìüò åğüìåíçò ôåôñÜäáò *)
val tempNumber : int ref                  (* Áñßèìçóç ôùí temporaries  *)
val currentFun : entry ref

val initSymbolTable    : int -> unit
val openScope          : unit -> unit
val closeScope         : unit -> unit
val closeFunctionScope : entry_info -> unit
val newVariable      : Identifier.id -> Types.typ -> bool -> entry
val newConst         : Identifier.id -> Types.typ -> string -> bool -> entry
val newFunction      : Identifier.id -> bool -> entry
val newParameter     : Identifier.id -> Types.typ -> pass_mode ->
                                        entry -> bool -> entry
val newTemporary     : Types.typ -> entry

val forwardFunction   : entry -> unit
val endFunctionHeader : entry -> Types.typ -> unit
val lookupEntry       : Identifier.id -> lookup_type -> bool -> entry
val set_var_val         : entry -> string -> unit
val get_var_val         : entry -> string

val start_positive_offset : int   (* Áñ÷éêü èåôéêü offset óôï Å.Ä.   *)
val start_negative_offset : int   (* Áñ÷éêü áñíçôéêü offset óôï Å.Ä. *)
val get_entry_type        : entry -> Types.typ
val get_function_param_size : entry -> int
