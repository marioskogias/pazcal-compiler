@echo off
if exist llama.lib del llama.lib

support\masm -mx src\stdio\printi.asm;
support\masm -mx src\stdio\readi.asm;
support\masm -mx src\stdio\printc.asm;
support\masm -mx src\stdio\readc.asm;
support\masm -mx src\stdio\printb.asm;
support\masm -mx src\stdio\readb.asm;
support\masm -mx src\stdio\printr.asm;
support\masm -mx src\stdio\readr.asm;
support\masm -mx src\stdio\prints.asm;
support\masm -mx src\stdio\reads.asm;

support\lib llama.lib /NOIGNORECASE +printi.obj +readi.obj;
support\lib llama.lib /NOIGNORECASE +printc.obj +readc.obj;
support\lib llama.lib /NOIGNORECASE +printb.obj +readb.obj;
support\lib llama.lib /NOIGNORECASE +printr.obj +readr.obj;
support\lib llama.lib /NOIGNORECASE +prints.obj +reads.obj;

support\masm -mx src\math\abs.asm;
support\masm -mx src\math\fabs.asm;
support\masm -mx src\math\sqrt.asm;
support\masm -mx src\math\sin.asm;
support\masm -mx src\math\cos.asm;
support\masm -mx src\math\tan.asm;
support\masm -mx src\math\atan.asm;
support\masm -mx src\math\exp.asm;
support\masm -mx src\math\ln.asm;
support\masm -mx src\math\pi.asm;

support\lib llama.lib /NOIGNORECASE +abs.obj fabs.obj +sqrt.obj;
support\lib llama.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +atan.obj;
support\lib llama.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj;

support\masm -mx src\stdlib\incr.asm;
support\masm -mx src\stdlib\decr.asm;
support\masm -mx src\stdlib\float.asm;
support\masm -mx src\stdlib\trunc.asm;
support\masm -mx src\stdlib\round.asm;
support\masm -mx src\stdlib\ord.asm;
support\masm -mx src\stdlib\chr.asm;
support\masm -mx src\stdlib\exit.asm;

support\lib llama.lib /NOIGNORECASE +incr.obj +decr.obj;
support\lib llama.lib /NOIGNORECASE +float.obj +trunc.obj +round.obj;
support\lib llama.lib /NOIGNORECASE +ord.obj +chr.obj;
support\lib llama.lib /NOIGNORECASE +exit.obj;

support\masm -mx src\string\strlen.asm;
support\masm -mx src\string\strcmp.asm;
support\masm -mx src\string\strcpy.asm;
support\masm -mx src\string\strcat.asm;

support\lib llama.lib /NOIGNORECASE +strlen.obj +strcmp.obj;
support\lib llama.lib /NOIGNORECASE +strcpy.obj +strcat.obj;

support\masm -mx src\auxil\new.asm;
support\masm -mx src\auxil\dispose.asm;
support\masm -mx src\auxil\makearr.asm;
support\masm -mx src\auxil\formati.asm;
support\masm -mx src\auxil\formatr.asm;
support\masm -mx src\auxil\parsei.asm;
support\masm -mx src\auxil\parser.asm;
support\masm -mx src\auxil\read.asm;
support\masm -mx src\auxil\print.asm;

support\lib llama.lib /NOIGNORECASE +new.obj +dispose.obj;
support\lib llama.lib /NOIGNORECASE +makearr.obj;
support\lib llama.lib /NOIGNORECASE +formati.obj +formatr.obj;
support\lib llama.lib /NOIGNORECASE +parsei.obj +parser.obj;
support\lib llama.lib /NOIGNORECASE +print.obj +read.obj;

support\lib llama.lib /NOIGNORECASE, llama.lst;
ren llama.lib llama.lib
ren llama.lst llama.lst

del *.obj
del *.bak
