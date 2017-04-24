all: io_helpers networking crypto signature

io_helpers: iOHelpers.ml
	ocamlbuild -pkg yojson -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -pkg yojson -ocamlc 'ocamlc -thread str.cma threads.cma' -use-ocamlfind networking.byte

crypto: crypto.ml
	ocamlbuild -pkgs nocrypto.unix crypto.byte

signature: signature.ml
	ocamlbuild -use-ocamlfind -pkgs nocrypto.unix signature.byte

clean:
	rm -rf _build *.byte
