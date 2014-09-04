; int_of_char : char -> int
; -------------------------
; This function returns the ASCII code of a character.


xseg          segment public 'code'
              assume  cs:xseg, ds:xseg, ss:xseg

              public _int_of_char

_int_of_char  proc   near
              push   bp
              mov    bp, sp
              mov    al, byte ptr [bp+8]      ; 1st parameter
              xor    ah, ah
              mov    si, word ptr [bp+6]      ; store result
              mov    word ptr [si], ax
              pop    bp
              ret
_int_of_char  endp

xseg          ends
              end
