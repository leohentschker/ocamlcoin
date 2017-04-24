all: signature

signature: signature.ml
	ocamlbuild -use-ocamlfind -pkgs nocrypto.unix signature.byte

clean:
	rm -rf _build *.byte
