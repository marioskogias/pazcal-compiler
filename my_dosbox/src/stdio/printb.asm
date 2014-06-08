; print_bool : bool -> unit
; -------------------------
; This function prints a boolean to the standard output.
; One of the strings 'true' and 'false' is printed.


xseg          segment public 'code'
              assume  cs:xseg, ds:xseg, ss:xseg

              public _print_bool

_print_bool   proc  near
              push  bp
              mov   bp, sp
              mov   cx, word ptr [bp+8]      ;1st parameter width (change afterwards 1st 2nd)
              mov   al, byte ptr [bp+10]     ; 2nd parameter
              or    al, al                   ; True if non zero
              jnz   par_true
              sub   cx, 5
              jbe   print_false
              call  _print_empty       
print_false:  lea   dx, byte ptr str_false   ; Print 'false'
              mov   ah, 09h
              int   21h
              jmp   short ok
par_true:
              sub   cx, 4
              jbe   print_true
              call  _print_empty       
print_true:   lea   dx, byte ptr str_true    ; Print 'true'
              mov   ah, 09h
              int   21h
ok:
              pop   bp
              ret
_print_bool   endp

_print_empty  proc  near
again:        mov   dl,20h
              mov   ah,02h
              int   21h
              loop  again
              ret
_print_empty  endp

; These are the strings that will be printed
; DOS int 21h, function 09h requires a '$' at the end.

str_false     db    'false'
              db    '$'
str_true      db    'true'
              db    '$'

xseg          ends
              end
