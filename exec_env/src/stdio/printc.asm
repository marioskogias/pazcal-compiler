; print_char : char -> unit
; -------------------------
; This function prints a character to the standard output.


xseg        segment public 'code'
            assume  cs:xseg, ds:xseg, ss:xseg

            public _print_char

_print_char proc  near
            push  bp
            mov   bp, sp
            mov   cx, word ptr [bp+8]      ;2nd parameter width
            cmp   cl,00h
            je    no_padd
            cmp   cl,01h
            je    no_padd
            dec   cx
again:      mov   dl,20h
            mov   ah,02h
            int   21h
            loop  again
no_padd:    mov   dl, byte ptr [bp+10]     ; 1st parameter
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
_print_char endp

xseg        ends
            end
