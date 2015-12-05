CMO=debug.cmo lexer.cmo parser_error.cmo parser.cmo type_ast.cmo typing.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
FLAGS=-annot -g
MENHIR_FLAGS=-v --infer
all: pscala

.PHONY: tests
tests: pscala
	bash run-tests

pscala: $(CMO)
	ocamlc $(FLAGS) -o $@ nums.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir $(MENHIR_FLAGS) $<

.mly.mli:
	menhir $(MENHIR_FLAGS) $<

parser.mly: ast.cmi parser_error.cmo

clean:
	rm -f *.cm[io] *.o *.annot *~ pscala$(GENERATED)
	rm -f parser.output parser.automaton
	rm -f .depend

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
