SHELL := /bin/bash -e -o pipefail

opam:
	opam update
	if [[ ! -d "_opam" ]]; then opam switch create . 4.10.0 --no-install -y; fi
	opam pin add understanding_computation_book . -n -y
	opam install -y . --deps-only --locked --working-dir

build:
	dune build

bin:
	@rm -rf bin && ln -sf _build/install/default/bin bin

