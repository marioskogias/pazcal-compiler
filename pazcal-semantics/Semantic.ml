open Types
open Identifier
open Symbol
open Error
open Lexing

(* Semantic checking of values in binary expressions *)
let check_arithmetic_types op_name type_1 type_2 =
  match (type_1, type_2) with

  (* Same kind of types in expression are correct *)
  |(TYPE_int, TYPE_int)
  |(TYPE_real, TYPE_real)
  |(TYPE_int, TYPE_real)
  |(TYPE_real, TYPE_int)
    -> true

  (* Default is to expect Int in expressions *)
  |_ ->
    false

