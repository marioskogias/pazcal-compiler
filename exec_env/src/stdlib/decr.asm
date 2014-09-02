; decr : int ref -> unit
; ----------------------
; This function decrements by one the integer pointed to by its parameter.


xseg         segment   public 'code'
             assume    cs:xseg, ds:xseg, ss:xseg

             public    _decr

_decr        proc      near
             push      bp
             mov       bp, sp
             mov       si, word ptr [bp+8]
             mov       ax, word ptr [si]
             dec       ax
             mov       word ptr [si], ax
             mov       sp, bp
             pop       bp
             ret
_decr        endp

xseg         ends
             end
