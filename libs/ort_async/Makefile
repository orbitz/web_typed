PACKAGE=-package async,ort_prelude,threads

.PHONY: all clean test

all: ort_async.cmxa ort_async.cma

return_monad.cmi: return_monad.mli
	ocamlfind ocamlc $(PACAKGE) -thread -c return_monad.mli

ort_async.cmxa: ort_async.cmx
	ocamlfind ocamlopt -a -thread -I +camlp4 -o ort_async.cmxa ort_async.cmx

ort_async.cmx: return_monad.cmx
	ocamlfind ocamlopt -pack -o ort_async.cmx return_monad.cmx

return_monad.cmx: return_monad.ml return_monad.cmi
	ocamlfind ocamlopt -package async,ort_prelude,threads -thread -c -for-pack Ort_async return_monad.ml

ort_async.cma: ort_async.cmo
	ocamlfind ocamlc -a -custom -I +camlp4 -o ort_async.cma ort_async.cmo

ort_async.cmo: return_monad.cmo
	ocamlfind ocamlc -pack -o ort_async.cmo return_monad.cmo

return_monad.cmo: return_monad.ml return_monad.cmi
	ocamlfind ocamlc $(PACKAGE) -thread -c return_monad.ml


clean:
	rm *.cmo *.cmi *.cmx *.cma *.cmxa *.a *.o
