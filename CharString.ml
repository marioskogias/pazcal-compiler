let special_char_asci c = 
  let code =  match c with
    |'n' -> 10
    |'t' -> 9
    |'r' -> 13
    |'0' -> 0
    |'\\' -> 92
    |'\'' -> 39
    |'"'  -> 34
    |_ -> 0
  in string_of_int code

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;


let escape_chars s =
  let char_arr = explode s in
  let rec help_fn = function
    |([], [], res) -> List.rev res
    |([], current, res) -> let new_s = Printf.sprintf "'%s'" (implode (List.rev current)) in help_fn([], [], (new_s::res))
    |((h::t), current, res) ->
        let no_special = 
          if (String.length (Char.escaped h)) == 1 then true
          else false in
        if no_special then help_fn(t,(h::current),res)
        else 
          let escaped = special_char_asci (List.hd t)  in
          let old = Printf.sprintf "'%s'" (implode (List.rev current)) in
            help_fn((List.tl t), [], (escaped::old::res))
  in help_fn(char_arr, [], []) 

let declare_string label str = 
  let escaped = escape_chars str in
  let add_db s = Printf.sprintf "db %s\n" s in
  let db_list = List.map add_db escaped in
  let rec final = function
    |([], s) -> s^"\tdb 0\n"
    |((h::t), "") -> final(t, label^" "^h)
    |((h::t), s) -> final(t, s^"\t"^h)
  in final (db_list, "")
