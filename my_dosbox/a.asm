DATA SEGMENT
    MSG DB "Hello, world!","$"
DATA ENDS

CODE SEGMENT
    ASSUME CS:CODE, DS:DATA
    START:
        MOV AX, DATA
        MOV DS, AX

        MOV AH, 09H
        LEA DX, MSG
        INT 21H

    STOP:
        MOV AX, 4C00H
        INT 21H

CODE ENDS
    END START
