all: io_helpers networking crypto merkle profile

tests: payments_tests events_tests

profile: profile.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix -ocamlc 'ocamlc -thread str.cma threads.cma' profile.byte

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
	ocamlbuild -lib unix -pkg yojson -use-ocamlfind iOHelpers.byte

networking: networking.ml
	ocamlbuild -lib unix -pkg sexplib -pkgs nocrypto.unix -pkgs yojson -ocamlc 'ocamlc -thread str.cma threads.cma' -use-ocamlfind networking.byte

crypto: crypto.ml
	ocamlbuild -pkgs sexplib -pkgs nocrypto.unix crypto.byte

merkle: merkletree.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs sexplib -pkgs nocrypto.unix merkletree.byte

mining: mining.ml
	ocamlbuild -use-ocamlfind -ocamlc 'ocamlc -thread str.cma threads.cma' -pkgs nocrypto.unix mining.byte

payments: payments.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments.byte

payments_tests: payments_tests.ml
	ocamlbuild -use-ocamlfind -pkg yojson -pkgs nocrypto.unix payments_tests.byte

gui: gui.ml
	ocamlbuild -use-ocamlfind -pkgs lablgtk2 -pkgs nocrypto.unix -ocamlc 'ocamlc -thread threads.cma' gui.byte

wallet : wallet.ml
	ocamlbuild -use-ocamlfind -ocamlc 'ocamlc -thread str.cma threads.cma' -pkgs nocrypto.unix -pkg yojson wallet.byte

ledger : ledger.ml
	ocamlbuild -use-ocamlfind -ocamlc 'ocamlc -thread str.cma threads.cma' -pkgs nocrypto.unix -pkg yojson ledger.byte

clean:
	rm -rf _build
