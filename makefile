all: io_helpers networking

io_helpers: iOHelpers.ml
	ocamlbuild -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -ocamlc 'ocamlc str.cma' -use-ocamlfind networking.byte

clean:
	rm -rf _build *.byte
