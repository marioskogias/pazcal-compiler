open Types
open QuadTypes

(* This file includes the supportive functions and types for final code generation*)

(* Registers *)

type register = Ax | Bx | Cx | Dx | Di | Si | Bp | Sp
              | Al | Ah | Bl | Bh | Cl | Ch | Dl | Dh

(* Convert register to string *)
let string_of_register = function
  | Ax -> "ax"
  | Bx -> "bx"
  | Cx -> "cx"
  | Dx -> "dx"
  | Al -> "al"
  | Ah -> "ah"
  | Di -> "di"
  | Si -> "si"
  | Bp -> "bp"
  | Sp -> "sp"
  | Bl -> "bl"
  | Bh -> "bh"
  | Cl -> "cl"
  | Ch -> "ch"
  | Dl -> "dl"
  | Dh -> "dh"

let get_register = function
  |(Ax, TYPE_int) -> Ax
  |(Ax, _) -> Al
  |(Bx, TYPE_int) -> Bx
  |(Bx, _) -> Bl
  |(Cx, TYPE_int) -> Cx
  |(Cx, _) -> Cl
  |(Dx, TYPE_int) -> Dx
  |(Dx, _) -> Dl
  |(reg, _) -> reg

let get_16_bit_reg = function
  | Ax
  | Al -> Ax
  | Cx
  | Cl -> Cx
  | Bx
  | Bl -> Bx
  | Dx
  | Dl -> Dx
  | reg -> reg

(* In calculations we can have either registers or constants *)
type action_arg = 
  |Action_reg of register
  |Constant of int

let string_of_action_arg = function
  | Action_reg reg -> string_of_register reg
  | Constant num -> (string_of_int num)

(* For Mov, Lea, etc we have a memory_location *)
type mem_loc = 
  | Register of register                  (* Register               *)
  | Mem_loc of (string * register * int)  (* string ptr [reg + int] *)
  | String_addr of string                 (* @strx                  *)
  | Num of string                         (* Constant               *)

let mem_size t =
  let type_size = sizeOfArrayType t in
    match type_size with
      |2 -> "word"
      |_ -> "byte"

let string_of_mem_loc = function
  | Register reg -> (string_of_register reg)
  | Mem_loc (size, reg, offset) ->
      if (offset = 0)
      then 
        Printf.sprintf "%s ptr [%s]" 
        size (string_of_register reg)
      else
        Printf.sprintf "%s ptr [%s%+d]" 
        size (string_of_register reg) offset
  | String_addr str -> 
      Printf.sprintf "byte ptr %s" str
  | Num str -> str

(* Every assembly instruction is represented by a new type final_t *)
type final_t =
  | Start of string
  | End of string
  | Mov of (mem_loc * mem_loc)
  | Call of string
  | Lea of (mem_loc * mem_loc)
  | Jump of string
  | Cond_jump of (string * string)
  | Add of (action_arg * action_arg)
  | Sub of (action_arg * action_arg)
  | IMul of register
  | IDiv of register
  | Push of mem_loc
  | Pop of register
  | Cwd
  | Ret 
  | Label of string
  | Misc of string
  | Cmp of (register * register)
  | Proc of string

let string_of_final_t = function
  | Start str -> str
  | End str -> str
  | Mov (m1, m2) -> 
      Printf.sprintf "\tmov\t%s, %s\n" 
      (string_of_mem_loc m1) 
      (string_of_mem_loc m2)
  | Call str -> 
      Printf.sprintf "\tcall\tnear ptr %s\n" str
  | Lea (m1,m2) ->
      Printf.sprintf "\tlea\t%s, %s\n" 
      (string_of_mem_loc m1) 
      (string_of_mem_loc m2)
  | Jump str -> 
      Printf.sprintf "\tjmp\t%s\n" str
  | Cond_jump (c,str) ->
      Printf.sprintf "\t%s\t%s\n" c str
  | Add (a1, a2) ->
      Printf.sprintf "\tadd\t%s, %s\n"
      (string_of_action_arg a1)
      (string_of_action_arg a2)
  | Sub (a1, a2) ->
      Printf.sprintf "\tsub\t%s, %s\n"
      (string_of_action_arg a1)
      (string_of_action_arg a2)
  | IMul reg ->
      Printf.sprintf "\timul\t%s\n" 
      (string_of_register reg)
  | IDiv reg ->
      Printf.sprintf "\tidiv\t%s\n" 
      (string_of_register reg)
  | Push mem ->
      Printf.sprintf "\tpush\t%s\n"
      (string_of_mem_loc mem)
  | Pop reg ->
      Printf.sprintf "\tpop\t%s\n"
      (string_of_register reg)
  | Cwd -> "\tcwd\n"
  | Ret -> "\tret\n"
  | Label lab ->
      Printf.sprintf "%s:\n" lab
  | Misc str -> str
  | Cmp (r1,r2) -> 
      Printf.sprintf "\tcmp\t%s,%s\n"
      (string_of_register r1)
      (string_of_register r2)
  | Proc str ->
      Printf.sprintf "%s\tproc\tnear\n" str
