CAMLP4=-pp "camlp4o pa_extend.cmo"

.PHONY: all clean test

all: ort_prelude.cmxa ort_prelude.cma

ort_prelude.cmxa: ort_prelude.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o ort_prelude.cmxa ort_prelude.cmx

ort_prelude.cma: ort_prelude.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o ort_prelude.cma ort_prelude.cmo

ort_prelude.cmo: ort_prelude.ml
	ocamlfind ocamlc -c -I +camlp4 $(CAMLP4) ort_prelude.ml

ort_prelude.cmx: ort_prelude.ml ort_prelude.cmi
	ocamlfind ocamlopt -c -I +camlp4 $(CAMLP4) ort_prelude.ml

ort_prelude.cmi: ort_prelude.mli
	ocamlfind ocamlc -c -I +camlp4 ort_prelude.mli

test: all

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
