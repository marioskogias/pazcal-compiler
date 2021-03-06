; print_s_char : char -> unit
; -------------------------
; This function prints a character to the standard output.


xseg        segment public 'code'
            assume  cs:xseg, ds:xseg, ss:xseg

            public _print_s_char

_print_s_char proc  near
              push  bp
              mov   bp, sp
no_padd:      mov   dl, byte ptr [bp+8]     ; 1st parameter
              or    dl, dl                   ; ignore high order byte
              jz    ok                       ; if 0, then ok
              cmp   dl, 0Ah
              jnz   normal                   ; if not '\n', no problem
              push  dx
              mov   dl, 0Dh
              mov   ah, 02h
              int   21h                      ; else, print also '\r'
              pop   dx
normal:
              mov   ah, 02h
              int   21h
ok:
              pop   bp
              ret
_print_s_char endp

xseg          ends
              end
