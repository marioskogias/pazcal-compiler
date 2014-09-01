open QuadTypes
open Symbol
open Error
open Types

let constant_optimize q_list =
  (*constants can only be integers, booleans, or chars*)
  let get_constant_val = function
    |Quad_entry e -> (
       match e.entry_info with
         |ENTRY_variable vinfo -> if vinfo.is_const then (
            match vinfo.variable_type with
              |TYPE_int -> Some(Quad_int(vinfo.value))
              |TYPE_char -> Some(Quad_char(vinfo.value))
              |TYPE_bool -> Some(Quad_bool(vinfo.value))
              |_ -> error "Unacceptable const type";raise Terminate
          )
          else None
         |_ -> None
     )
    |_ -> None

  (*x+0=x, x*1=x*)
  in let handle_trans quad =  
    match quad with 
      | Quad_calc (op, q1, q2, q) -> (
          match op with
            |"+" -> (
               match (q1, q2) with
                 |(Quad_int v, _) -> if v=="0" then Quad_set(q2, q) else quad 
                 |(_, Quad_int v) -> if v=="0" then Quad_set(q1, q) else quad
                 |(_,_) -> quad
             ) 
            |"*" -> (
               match (q1, q2) with
                 |(Quad_int v, _) -> 
                     if v=="1" then Quad_set(q2, q)
                     else if v=="0" then Quad_set(Quad_int "0", q) 
                     else quad 
                 |(_, Quad_int v) ->
                     if v=="1" then Quad_set(q1, q)
                     else if v=="0" then Quad_set(Quad_int "0", q) 
                     else quad 
                 |(_,_) -> quad
             ) 
            |_ ->quad
        )
      |_ -> internal "Not a quad_calc"; raise Terminate

  (*do the math only with integers*)
  in let do_the_math quad = 
    match quad with 
      | Quad_calc (op, q1, q2, q) -> (
          match q1, q2 with
            |Quad_int v1, Quad_int v2 -> (
               let calc = 
                 match op with
                   |"+" -> ( + )
                   |"-" -> ( - )
                   |"*" -> ( * )
                   |"/" -> ( / )
                   |"%" -> (mod)
                   | _  -> internal "Not an operator"; raise Terminate in
               let op_res = calc (int_of_string v1) (int_of_string v2) in
                 Quad_set(Quad_int(string_of_int op_res), q)
             )
            |_ -> quad
        )
      |Quad_set _ -> quad (*if handle trans has been applied*)
      |_ -> internal "Do the math only with Quad_calc"; raise Terminate

  in let do_the_logic quad =
    match quad with
      | Quad_cond (op, q1, q2, q) -> (
          let char_cond = 
            match op with
              | "==" -> ( = )
              | "<"  -> ( < )
              | ">"  -> ( > )
              | "!=" -> ( != )
              | "<=" -> ( <= )
              | ">=" -> ( >= )
              | _ -> internal "Not a cond operand"; raise Terminate 
          in let int_cond = 
            match op with
              | "==" -> ( = )
              | "<"  -> ( < )
              | ">"  -> ( > )
              | "!=" -> ( != )
              | "<=" -> ( <= )
              | ">=" -> ( >= )
              | _ -> internal "Not a cond operand"; raise Terminate 
          in let bool_cond = 
            match op with
              | "==" -> ( = )
              | "<"  -> ( < )
              | ">"  -> ( > )
              | "!=" -> ( != )
              | "<=" -> ( <= )
              | ">=" -> ( >= )
              | _ -> internal "Not a cond operand"; raise Terminate 
          in let res =  match q1, q2 with
            |Quad_int v1, Quad_int v2 -> Some(int_cond (int_of_string v1) (int_of_string v2))
            |Quad_char c1, Quad_char c2 -> Some(char_cond c1 c2)
            |Quad_bool b1, Quad_bool b2 -> Some(bool_cond (bool_of_string b1) (bool_of_string b2))
            |_ -> None
           in match res with
           |Some v -> if (v) then Quad_jump(q)
           else Quad_dummy  
           |None -> quad
        )
      |_ -> internal "Do the logic only with Quad_cond"; raise Terminate
  
  in let change_single_quad quad =
    match quad with 
      | Quad_calc (op, q1, q2, q) -> (
          let new_calc_quad = match (get_constant_val q1, get_constant_val q2) with 
              |(None, None) -> quad
              |(None, Some v) -> handle_trans (Quad_calc(op, q1, v, q))
              |(Some v, None) -> handle_trans (Quad_calc(op, v, q2,  q))
              |(Some v1, Some v2) ->  handle_trans (Quad_calc(op, v1, v2, q))
          in do_the_math new_calc_quad
        )
      |Quad_set(q1, q2) -> (
         let v = get_constant_val q1 in
           match v with
             |Some value -> Quad_set(value, q2)
             |_ -> quad
       )                      
      |Quad_array (q1, q2, s) -> ( 
         let v = get_constant_val q2 in
           match v with
             |Some value -> Quad_array(q1, value,s)
             |_ -> quad
       )                      
      |Quad_par (q, pm) -> ( 
         let v = get_constant_val q in
           match v with
             |Some value -> Quad_par(value, pm)
             |_ -> quad
       )                      
      |Quad_cond (op, q1, q2, e)  -> 
          let new_cond_quad = match (get_constant_val q1, get_constant_val q2) with 
              |(None, None) -> quad
              |(None, Some v) -> Quad_cond(op, q1, v, e)
              |(Some v, None) -> Quad_cond(op, v, q2,  e)
              |(Some v1, Some v2) ->  Quad_cond(op, v1, v2, e)
          in do_the_logic new_cond_quad
      |_ -> quad

  in List.map change_single_quad q_list
