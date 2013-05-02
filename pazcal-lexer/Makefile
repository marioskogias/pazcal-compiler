# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g 
OCAMLC=ocamlc

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

pazcal$(EXE): Lexer.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ str.cma $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

.PHONY: clean distclean

clean:
	$(RM) Lexer.ml *.cmo *.cmi *~

distclean: clean
	$(RM) pazcal$(EXE)
