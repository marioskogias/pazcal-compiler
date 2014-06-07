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

let rec equalType t1 t2 =
  let printType = function
    |TYPE_array(_,_) ->print_string "array\n"
    |_ -> print_string "not array\n" 
  in ignore(printType t1; printType t2;); 
  match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> if sz1*sz2 = 0 then equalType et1 et2
                                                     else
                                                         if sz1 = sz2 then equalType et1 et2
                                                         else false

   | _                                            -> t1 = t2
