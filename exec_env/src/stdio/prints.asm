; print_string : array of char -> unit
; ------------------------------------
; This function prints a null terminated string to the standard output.


xseg           segment public 'code'
               assume  cs:xseg, ds:xseg, ss:xseg

               public _print_string

_print_string  proc  near
               push  bp
               mov   bp, sp
               mov   si, word ptr [bp+8]       ; 1st parameter
;               inc   si
;               inc   si                       ; text of the string
               push  si
               sub   sp, 2
               push  bp
               call  _printString
               add   sp, 6
               pop   bp
               ret
_print_string  endp

               extrn _printString : proc

xseg           ends
               end
