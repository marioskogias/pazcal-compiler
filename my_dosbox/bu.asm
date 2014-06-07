xseg    segment public 'code'
    assume  cs : xseg, ds : xseg, ss : xseg
    org 100h
    extrn _read_char : proc
    extrn _read_bool : proc
    extrn _read_int : proc
    extrn _print_char : proc
    extrn _print_int : proc
    extrn _print_string : proc
    extrn _print_char : proc

main    proc    near
    mov BP, SP
    sub BP, 4
    push BP
    sub SP, 2
    mov BP, SP
    sub SP, 0
    call    near ptr _PROGRAM_1
    mov ax, 4C00h
    int 21h
main endp
_PROGRAM_1  proc    near
    push    bp
    mov bp, sp
    sub sp, 0
    mov al, 50
    sub sp, 1
    mov si, sp
    mov byte ptr [si], al
    mov ax, 100
    push    ax
    sub sp, 2
    mov ax, word ptr [bp+4]
    push    ax
    call    near ptr _print_char
    add sp, 4
@_PROGRAM_1:
    mov sp, bp
    pop bp
    ret
_PROGRAM_1  endp
xseg ends
    end  main
