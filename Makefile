S=cmx
OBJS=common.$S debug.$S menhir_parse.$S parser.$S lexer.$S type_ast.$S typing.$S x86_64.$S register.$S label.$S is_ast.$S is.$S rtl_ast.$S rtl.$S ertl_ast.$S ertl.$S liveliness.$S interference.$S coloring.$S ltl_ast.$S ltl.$S lin.$S code_production.$S main.$S
GENERATED = lexer.ml menhir_parse.ml menhir_parse.mli
FLAGS=-annot -g
MENHIR_FLAGS=-v --infer
OCAML=ocamlopt
all: pscala

.PHONY: tests
tests: pscala
	bash run_tests_parser.sh
	bash run_tests_typer.sh
	bash run_tests_exec.sh

pscala: $(OBJS)
	$(OCAML) $(FLAGS) -o $@ nums.cmxa $(OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly .cmx

.mli.cmi:
	$(OCAML) $(FLAGS) -c  $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

.ml.cmx:
	$(OCAML) $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir $(MENHIR_FLAGS) $<

.mly.mli:
	menhir $(MENHIR_FLAGS) $<

menhir_parse.mly: ast.cmi

clean:
	rm -f *.cm[iox] *.o *.annot *~ pscala $(GENERATED)
	rm -f menhir_parse.output menhir_parse.automaton menhir_parse.conflicts
	rm -f .depend
	rm -f *.log *.aux *.synctex.gz

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
