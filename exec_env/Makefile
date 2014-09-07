.PHONY: clean distclean

LIBFILE=PAZCAL.LIB

default: $(LIBFILE)


$(LIBFILE): makelib.bat
	dosbox makelib.bat -exit > /dev/null

run: a.asm
	dosbox run.bat -exit > /dev/null   

clean:
	rm -f PAZCAL.LST

distclean:
	rm -f PAZCAL.*

