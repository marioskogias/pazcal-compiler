; double tan (double d);
; ----------------------
; This function returns the tangent of a real number.

; KNOWN BUGS !!!
; 1. Does not handle exceptions.
; 2. Ignores the abnormal case that 1.0 was not pushed.



xseg          segment public 'code'
              assume  cs:xseg, ds:xseg, ss:xseg

              .8087

              public _tan

_tan          proc   near
              push   bp
              mov    bp, sp
              fld    tbyte ptr [bp+8]
              fptan
              ffree  ST(0)                    ; pop the 1.0 that is
              fincstp                         ; pushed by fptan
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_tan          endp

xseg          ends
              end
