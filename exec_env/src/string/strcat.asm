; strcat : array of char -> array of char -> unit
; -----------------------------------------------
; This function concatenates the null terminated strings
; trg and src. The result is stored in trg. Pointers to
; both strings are passed. String src is left untouched.
; It is assumed that trg has enough space to hold the
; result of the concatenation.


xseg        segment public 'code'
            assume  cs:xseg, ds:xseg, ss:xseg

            public _strcat

_strcat     proc  near
            push  bp
            mov   bp, sp
            mov   di, word ptr [bp+10]     ; 1st parameter
            mov   si, word ptr [bp+8]      ; 2nd parameter
loop1:
            mov   dl, byte ptr [di]        ; Find the end of trg
            or    dl, dl
            jz    loop2
            inc   di
            jmp   short loop1
loop2:
            mov   dl, byte ptr [si]        ; Until the end of src
            or    dl, dl
            jz    ok
            mov   byte ptr [di], dl        ; Append characters
            inc   si
            inc   di
            jmp   short loop2
ok:
            xor   dl, dl                   ; Append final 0
            mov   byte ptr [di], dl
            pop   bp
            ret
_strcat     endp

xseg        ends
            end
