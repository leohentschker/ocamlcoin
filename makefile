all: io_helpers networking

io_helpers: iOHelpers.ml
	ocamlbuild -pkg yojson -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -pkg yojson -ocamlc 'ocamlc -thread str.cma threads.cma' -use-ocamlfind networking.byte

clean:
	rm -rf _build *.byte
