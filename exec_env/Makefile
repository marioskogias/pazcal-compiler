.PHONY: clean distclean

LIBFILE=PAZCAL.LIB

default: $(LIBFILE)


$(LIBFILE): makelib.bat
	dosbox makelib.bat -exit 2> /dev/null > /dev/null

run: a.asm
	dosbox run.bat -exit 2> /dev/null > /dev/null

clean:
	rm -f PAZCAL.LST a.asm A.COM A.OBJ

distclean:
	rm -f PAZCAL.*

