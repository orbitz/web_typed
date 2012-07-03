PACKAGE=-package async,ort_prelude,threads

.PHONY: all clean test

all: ort_async.cmxa ort_async.cma

async_result.cmi: async_result.mli
	ocamlfind ocamlc $(PACKAGE) -thread -c async_result.mli

ort_async.cmxa: ort_async.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o ort_async.cmxa ort_async.cmx

ort_async.cmx: async_result.cmx gen_server.cmx
	ocamlfind ocamlopt -pack -o ort_async.cmx async_result.cmx gen_server.cmx

async_result.cmx: async_result.ml async_result.cmi
	ocamlfind ocamlopt $(PACKAGE) -thread -c -for-pack Ort_async async_result.ml

ort_async.cma: ort_async.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o ort_async.cma ort_async.cmo

ort_async.cmo: async_result.cmo gen_server.cmo
	ocamlfind ocamlc -pack -o ort_async.cmo async_result.cmo gen_server.cmo

async_result.cmo: async_result.ml async_result.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c async_result.ml

gen_server.cmx: gen_server.ml gen_server.cmi
	ocamlfind ocamlopt $(PACKAGE) -thread -c -for-pack Ort_async gen_server.ml

gen_server.cmo: gen_server.ml gen_server.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c gen_server.ml

gen_server.cmi: gen_server.mli
	ocamlfind ocamlc $(PACKAGE) -thread -c gen_server.mli

test: gen_server_test
	./gen_server_test

gen_server_test: gen_server_test.cmo
	ocamlfind ocamlc $(PACKAGE),ort_async -custom -linkpkg -thread -o gen_server_test gen_server_test.cmo

gen_server_test.cmo: gen_server_test.ml ort_async.cma
	ocamlfind ocamlc $(PACKAGE),ort_async -thread -c gen_server_test.ml

clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o gen_server_test
