; procedure write_string (var s : array of char, w : int)
; -------------------------------------------------------
; This function prints a null terminated string to the standard output
; with a predefined width.


xseg           segment public 'code'
               assume  cs:xseg, ds:xseg, ss:xseg

               public _write_string

_write_string  proc  near
               push  bp
               mov   bp, sp
               mov   si, word ptr [bp+10]      ; 1st parameter
               mov   bx, word ptr [bp+8]       ; 2nd parameter
mloop:
               dec   bx
               jz    next_print                ; if 0, then print_next
               mov   dl, byte ptr [si]         ; Load next character
               or    dl, dl
               jz    print_spaces              ; if 0, then print_spaces
               inc   si
               jmp   mloop                      ; else loop again 
print_spaces:  
               mov   dl, 32
               mov   ah, 2
               int   21h
               dec   bx
               jnz   print_spaces 
next_print:    
               mov   si, word ptr [bp+10]
               push  si
               sub   sp, 2
               push  bp
               call  _printString
               add   sp, 6
               pop   bp
               ret
_write_string  endp

               extrn _printString : proc

xseg           ends
               end
 
