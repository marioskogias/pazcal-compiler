open QuadTypes
open Symbol
open Error

let constant_optimize q_list =
    let get_constant_val = function
    |Quad_entry e -> (
            match e.entry_info with
            |ENTRY_variable vinfo -> if vinfo.is_const then ( 
                try Some(int_of_string vinfo.value)
                with Failure "int_of_string" -> None)
            else None
            |_ -> None
            )
    |Quad_int value -> Some (int_of_string value)
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
    
    in let change_single_quad quad =
    match quad with 
    | Quad_calc (op, q1, q2, q) -> (
            let calc = 
            match op with
            |"+" -> ( + )
            |"-" -> ( - )
            |"*" -> ( * )
            |"/" -> ( / )
            |"%" -> (mod)
            | _  -> internal "Not an operator"; raise Terminate in
            match (get_constant_val q1, get_constant_val q2) with 
            |(None, None) -> quad
            |(None, Some v) -> handle_trans (Quad_calc(op, q1, Quad_int(string_of_int v), q))
            |(Some v, None) -> handle_trans (Quad_calc(op, Quad_int(string_of_int v), q2,  q))
            |(Some v1, Some v2) -> let op_res = calc v1 v2 in
            Quad_set(Quad_int(string_of_int op_res), q)
            )
    |Quad_set(q1, q2) -> (
            let v = get_constant_val q1 in
            match v with
            |Some value -> Quad_set(Quad_int (string_of_int value),q2)
            |_ -> quad
            )                      
    |Quad_array (q1, q2, s) -> ( 
            let v = get_constant_val q2 in
            match v with
            |Some value -> Quad_array(q1, Quad_int (string_of_int value),s)
            |_ -> quad
            )                      
    |Quad_par (q, pm) -> ( 
            let v = get_constant_val q in
            match v with
            |Some value -> Quad_par(Quad_int (string_of_int value),pm)
            |_ -> quad
            )                      
    |Quad_cond (op, q1, q2, e)  -> quad
    |_ -> quad

    in List.map change_single_quad q_list
