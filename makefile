all: io_helpers networking crypto signature merkle

tests: payments_tests

io_helpers: iOHelpers.ml
	ocamlbuild -pkg yojson -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -pkg yojson -ocamlc 'ocamlc -thread str.cma threads.cma' -use-ocamlfind networking.byte

crypto: crypto.ml
	ocamlbuild -pkgs nocrypto.unix crypto.byte

signature: signature.ml
	ocamlbuild -use-ocamlfind -pkgs nocrypto.unix signature.byte

merkle: merkletree.ml
	ocamlbuild -use-ocamlfind -pkgs nocrypto.unix merkletree.byte

mining: mining.ml
	ocamlbuild -use-ocamlfind -ocamlc 'ocamlc -thread str.cma threads.cma' -pkgs nocrypto.unix mining.byte

payments: payments.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments.byte

payments_tests: payments_tests.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments_tests.byte

clean:
	rm -rf _build *.byte
