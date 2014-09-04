open QuadTypes
open Quads


let calc_sizes func_array =
  let n = Array.length func_array in
  let sizes = Array.make n 0 in
  sizes.(0) <- Array.length (func_array.(0)); 
  for i = 1 to n - 1 do 
    sizes.(i) <- sizes.(i-1) + (Array.length func_array.(i)) 
  done;
  sizes

let alter_jump sizes offset quad =
    match quad with
    | Quad_jump (x)
    | Quad_cond (_,_,_,x) -> x := offset + sizes.(!x - 1); 
    | _ -> ()



let merge_quad_block quads_array sizes offset =
    Array.iter (alter_jump sizes offset) quads_array;
    Array.to_list quads_array

let my_append sizes offset acc array_to_list = 
    let new_quad_list = merge_quad_block array_to_list sizes offset in
    acc @ new_quad_list

let merge_func_block acc func_array =
    let sizes = calc_sizes func_array in
    let offset = List.length acc in
    let func_list = Array.fold_left (my_append sizes offset) [] func_array
    in acc @ func_list

let make_list block_code =
    Array.fold_left merge_func_block [] block_code;
