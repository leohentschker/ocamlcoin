all: networking

networking: networking.ml
	ocamlbuild -lib unix -ocamlc 'ocamlc str.cma' -use-ocamlfind networking.byte

clean:
	rm -rf _build *.byte
