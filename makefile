all: io_helpers networking crypto signature merkle

tests: payments_tests events_tests

ocamlcoin: ocamlcoin.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix -ocamlc 'ocamlc -thread str.cma threads.cma' ocamlcoin.byte

events: events.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix -ocamlc 'ocamlc -thread str.cma threads.cma' events.byte

payments_tests: payments_tests.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments_tests.byte

events_tests: events_tests.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix -ocamlc 'ocamlc -thread str.cma threads.cma' events_tests.byte

networking_tests: networking_tests.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix -ocamlc 'ocamlc -thread str.cma threads.cma' networking_tests.byte

io_helpers: iOHelpers.ml
	ocamlbuild -pkg yojson -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -pkg yojson -ocamlc 'ocamlc -thread str.cma threads.cma' -use-ocamlfind networking.byte

crypto: crypto.ml
	ocamlbuild -pkgs nocrypto.unix crypto.byte

merkle: merkletree.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs sexplib -pkgs nocrypto.unix merkletree.byte

mining: mining.ml
	ocamlbuild -use-ocamlfind -ocamlc 'ocamlc -thread str.cma threads.cma' -pkgs nocrypto.unix mining.byte

payments: payments.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments.byte

gui: gui.ml
	ocamlbuild -use-ocamlfind -pkgs lablgtk2 -pkgs nocrypto.unix -ocamlc 'ocamlc -thread threads.cma' gui.byte

clean:
	rm -rf _build
