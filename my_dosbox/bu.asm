xseg	segment	public 'code'
	assume	cs : xseg, ds : xseg, ss : xseg
	org	100h
	extrn _strcat : proc
	extrn _strcpy : proc
	extrn _strcmp : proc
	extrn _strlen : proc
	extrn _readString : proc
	extrn _read_char : proc
	extrn _read_bool : proc
	extrn _read_int : proc
	extrn _write_string : proc
	extrn _print_bool : proc
	extrn _print_char : proc
	extrn _print_int : proc
	extrn _print_string : proc
	extrn _print_char : proc

main	proc	near
	mov BP, SP
	sub BP, 4
	push BP
	sub SP, 2
	mov BP, SP
	sub SP, 0
	call	near ptr _PROGRAM_1
	mov	ax, 4C00h
	int	21h
main endp
@0:
_test_2	proc	near
	push	bp
	mov	bp, sp
	sub	sp, 0
@1:
	mov	al, 52
	sub	sp, 1
	mov	si, sp
	mov	byte ptr [si], al
@2:
	mov	ax, 5
	push	ax
@3:
	sub	sp, 2
	mov	ax, word ptr [bp+4]
	push	ax
	call	near ptr _print_char
	add	sp, 4
@4:
@_test_2:
	mov	sp, bp
	pop	bp
	ret
_test_2	endp
@5:
_PROGRAM_1	proc	near
	push	bp
	mov	bp, sp
	sub	sp, 0
@6:
	sub	sp, 2
	mov	ax, word ptr [bp+4]
	push	ax
	call	near ptr _test_2
	add	sp, 4
@7:
@_PROGRAM_1:
	mov	sp, bp
	pop	bp
	ret
_PROGRAM_1	endp
xseg ends
	end  main
