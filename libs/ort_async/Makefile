PACKAGE=-package async,ort_prelude,threads

.PHONY: all clean test

all: ort_async.cmxa ort_async.cma

async_return.cmi: async_return.mli
	ocamlfind ocamlc $(PACKAGE) -thread -c async_return.mli

ort_async.cmxa: ort_async.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o ort_async.cmxa ort_async.cmx

ort_async.cmx: async_return.cmx
	ocamlfind ocamlopt -pack -o ort_async.cmx async_return.cmx

async_return.cmx: async_return.ml async_return.cmi
	ocamlfind ocamlopt -package async,ort_prelude,threads -thread -c -for-pack Ort_async async_return.ml

ort_async.cma: ort_async.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o ort_async.cma ort_async.cmo

ort_async.cmo: async_return.cmo
	ocamlfind ocamlc -pack -o ort_async.cmo async_return.cmo

async_return.cmo: async_return.ml async_return.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c async_return.ml


clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
