CAMLP4=-pp "camlp4o pa_extend.cmo"

.PHONY: all clean

all:  stomp.cma

stomp.cma: stomp.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o stomp.cma stomp.cmo

stomp.cmo: client.cmo
	ocamlfind ocamlc -pack -o stomp.cmo client.cmo

client.cmo: client.ml client.cmi
	ocamlfind ocamlc -package core_extended,threads,seq,ort_prelude -c -thread -I +camlp4 $(CAMLP4)  client.ml

client.cmi: client.mli
	ocamlfind ocamlc -package core_extended,threads,ort_prelude -c -thread -I +camlp4 client.mli

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
