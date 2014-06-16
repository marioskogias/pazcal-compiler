let special_char_asci c = 
  let code =  match c with
    |'n' -> 10
    |'t' -> 9
    |'r' -> 13
    |'0' -> 0
    |'\\' -> 92
    |'\'' -> 39
    |'"'  -> 3
  in string_of_int code
