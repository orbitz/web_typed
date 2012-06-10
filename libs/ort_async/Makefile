PACKAGE=-package async,ort_prelude,threads

.PHONY: all clean test

all: ort_async.cmxa ort_async.cma

async_result.cmi: async_result.mli
	ocamlfind ocamlc $(PACKAGE) -thread -c async_result.mli

ort_async.cmxa: ort_async.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o ort_async.cmxa ort_async.cmx

ort_async.cmx: async_result.cmx
	ocamlfind ocamlopt -pack -o ort_async.cmx async_result.cmx

async_result.cmx: async_result.ml async_result.cmi
	ocamlfind ocamlopt -package async,ort_prelude,threads -thread -c -for-pack Ort_async async_result.ml

ort_async.cma: ort_async.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o ort_async.cma ort_async.cmo

ort_async.cmo: async_result.cmo
	ocamlfind ocamlc -pack -o ort_async.cmo async_result.cmo

async_result.cmo: async_result.ml async_result.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c async_result.ml


clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
