type typ = TYPE_none
  | TYPE_int
  | TYPE_real
  | TYPE_char
  | TYPE_bool
  | TYPE_array of
      typ *
      int
  | TYPE_proc

let rec sizeOfType t =
  match t with
    | TYPE_int            -> 2
    | TYPE_char           -> 1
    | TYPE_bool           -> 1
    | TYPE_real           -> 4
    | TYPE_array (et, sz) -> sz * sizeOfType et
    | _                   -> 0

let rec sizeOfArrayType t =  
  match t with
    |TYPE_array(et, sz) -> sizeOfArrayType et
    |_ -> sizeOfType t

let rec equalType t1 t2 =
  match t1, t2 with
    | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> if sz1*sz2 = 0 then equalType et1 et2
      else
        if sz1 = sz2 then equalType et1 et2
        else false

    | _ -> t1 = t2

let typeToString = function
  |TYPE_none -> "type none"
  | TYPE_int -> "type int"
  | TYPE_real -> "type real"
  | TYPE_char -> "type char"
  | TYPE_bool -> "type bool"
  | TYPE_array _ -> "type array"
  | TYPE_proc -> "type proc"
