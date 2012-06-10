CAMLP4=-pp "camlp4o pa_extend.cmo"

PACKAGE=-package core_extended,threads

.PHONY: all clean test

all: ort_prelude.cmxa ort_prelude.cma

ort_prelude.cmxa: ort_prelude.cmx
	ocamlfind ocamlopt -a $(PACKAGE) -thread -I +camlp4 -o ort_prelude.cmxa ort_prelude.cmx

ort_prelude.cma: ort_prelude.cmo
	ocamlfind ocamlc -a $(PACKAGE) -thread -custom -I +camlp4 -o ort_prelude.cma ort_prelude.cmo

ort_prelude.cmo: ort_prelude.ml
	ocamlfind ocamlc -c $(PACKAGE) -thread -I +camlp4 $(CAMLP4) ort_prelude.ml

ort_prelude.cmx: ort_prelude.ml ort_prelude.cmi
	ocamlfind ocamlopt $(PACKAGE) -thread -c -I +camlp4 $(CAMLP4) ort_prelude.ml

ort_prelude.cmi: ort_prelude.mli
	ocamlfind ocamlc $(PACKAGE) -thread -c -I +camlp4 ort_prelude.mli

test: all

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
