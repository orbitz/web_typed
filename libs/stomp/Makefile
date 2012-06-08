CAMLP4=-pp "camlp4o pa_extend.cmo"

.PHONY: all clean

all:  stomp.cma stomp.cmxa

stomp.cma: stomp.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o stomp.cma stomp.cmo

stomp.cmo: frame.cmo
	ocamlfind ocamlc -pack -o stomp.cmo frame.cmo

frame.cmo: frame.ml frame.cmi
	ocamlfind ocamlc -package core_extended,threads,seq,ort_prelude -c -thread -I +camlp4 $(CAMLP4)  frame.ml

frame.cmi: frame.mli
	ocamlfind ocamlc -package core_extended,threads,ort_prelude -c -thread -I +camlp4 frame.mli

stomp.cmxa: stomp.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o stomp.cmxa stomp.cmx

stomp.cmx: frame.cmx
	ocamlfind ocamlopt -pack -o stomp.cmx frame.cmx

frame.cmx: frame.ml frame.cmi
	ocamlfind ocamlopt -package core_extended,threads,seq,ort_prelude -c -thread -for-pack Stomp -I +camlp4 $(CAMLP4) frame.ml

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
