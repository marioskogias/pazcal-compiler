open FinalSupport 

let optimize asm_list = 
  let rec help acc alist =
    match alist with
      |(a::b::t) -> (
         match a,b with
           |Mov(m1, _), Mov(m2, _) when m1 = m2 -> help (b::acc) t
           |_ -> (help (a::acc) (b::t))
       )
      |[a] ->  (help (a::acc) [])
      |[] -> acc 
  in let rev_list = help [] asm_list
  in List.rev rev_list
