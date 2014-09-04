; read_string : array of char -> unit
; -----------------------------------
; This function reads a line from the standard input
; and stores it into string 's'.  The newline character
; is not stored.  It stops reading when the size of the
; array is reached.  Finally, a '\0' is always appended.


xseg          segment   public 'code'
              assume    cs:xseg, ds:xseg, ss:xseg

              public    _read_string

_read_string  proc      near
              push      bp
              mov       bp, sp
              mov       si, word ptr [bp+8]      ; 1st parameter (s)
              mov       ax, word ptr [si]        ; size of the string
              push      ax
              inc       si
              inc       si                       ; text of the string
              push      si
              sub       sp, 2
              push      bp
              call      _readString
              add       sp, 8
              pop   bp
              ret
_read_string  endp

              extrn _readString : proc

xseg          ends
              end
