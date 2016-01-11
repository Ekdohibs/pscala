CMO=common.cmo debug.cmo menhir_parse.cmo parser.cmo lexer.cmo type_ast.cmo typing.cmo x86_64.cmo register.cmo label.cmo is_ast.cmo is.cmo rtl_ast.cmo rtl.cmo ertl_ast.cmo ertl.cmo liveliness.cmo interference.cmo coloring.cmo lsl_ast.cmo lsl.cmo code_production.cmo main.cmo
GENERATED = lexer.ml menhir_parse.ml menhir_parse.mli
FLAGS=-annot -g
MENHIR_FLAGS=-v --infer
all: pscala

.PHONY: tests
tests: pscala
	bash run_tests_parser.sh
	bash run_tests_typer.sh

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

menhir_parse.mly: ast.cmi

clean:
	rm -f *.cm[io] *.o *.annot *~ pscala $(GENERATED)
	rm -f menhir_parse.output menhir_parse.automaton menhir_parse.conflicts
	rm -f .depend
	rm -f *.log *.aux *.synctex.gz

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
