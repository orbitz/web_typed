CAMLP4=-pp "camlp4o pa_extend.cmo" -pp "camlp4orf -I ../pa_monad ../pa_monad/pa_monad.cmo"

PACKAGE=-package core_extended,threads,seq,ort_prelude

.PHONY: all clean test

all:  stomp.cma stomp.cmxa

stomp.cma: stomp.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o stomp.cma stomp.cmo

stomp.cmo: frame.cmo
	ocamlfind ocamlc -pack -o stomp.cmo frame.cmo

frame.cmo: frame.ml frame.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c -I +camlp4 $(CAMLP4)  frame.ml

frame.cmi: frame.mli
	ocamlfind ocamlc $(PACKAGE) -c -thread -I +camlp4 frame.mli

stomp.cmxa: stomp.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o stomp.cmxa stomp.cmx

stomp.cmx: frame.cmx
	ocamlfind ocamlopt -pack -o stomp.cmx frame.cmx

frame.cmx: frame.ml frame.cmi
	ocamlfind ocamlopt $(PACKAGE) -thread -c -for-pack Stomp -I +camlp4 $(CAMLP4) frame.ml

test: frame_test
	./frame_test

frame_test: frame_test.cmo
	ocamlfind ocamlc $(PACKAGE),oUnit,stomp -custom -linkpkg -thread -o frame_test frame_test.cmo

frame_test.cmo: frame_test.ml stomp.cma
	ocamlfind ocamlc $(PACKAGE),oUnit,stomp -c -thread frame_test.ml

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o frame_test
