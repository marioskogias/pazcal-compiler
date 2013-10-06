open Error
open Lexing

let print_error msg pos = 
     error  "Line: %d %s"
        pos.pos_lnum
        msg

