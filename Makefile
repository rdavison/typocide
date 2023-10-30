.PHONY: build
build:
	opam exec -- dune build @all

.PHONY: start
start:
	opam exec -- dune build @all -w

.PHONY: install-deps
install-deps:
	opam exec -- opam install . --deps-only -y

.PHONY: run
run:
	opam exec -- dune exec bin/main.exe