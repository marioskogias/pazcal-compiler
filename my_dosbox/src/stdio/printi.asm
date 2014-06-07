; print_int : int -> unit
; -----------------------
; This function prints an integer to the standard output.


xseg          segment public 'code'
              assume  cs:xseg, ds:xseg, ss:xseg

              public _print_int

_print_int    proc  near
              push  bp
              mov   bp, sp
              sub   sp, 1                ; dummy space for the result
              
              mov   ax, OFFSET buffer    ; where to store it
              push  ax
              push  word ptr [bp+10]     ; 1st parameter (the number)
              mov   ah, byte ptr [bp+8]  ; 2nd parameter (width) to ah
              mov   al, 0000h            ; flags = 0
              push  ax
              sub   sp, 1
              mov   si, sp
              mov   byte ptr [si], 10    ; base = 10
              lea   ax, byte ptr [bp-1]
              push  ax                   ; dummy: result
              push  bp                   ; dummy: access link
              call  near ptr _formatInteger
              add   sp, 11
              
              mov   ax, OFFSET buffer    ; what to print
              push  ax
              sub   sp, 2                ; dummy: result
              push  bp                   ; dummy: access link
              call  near ptr _printString
              add   sp, 6

              mov   sp, bp
              pop   bp
              ret
_print_int    endp

buffer        db    6 dup(?)

              extrn _formatInteger : proc
              extrn _printString : proc

xseg          ends
              end
