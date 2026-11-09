DUNE = opam exec -- dune

.PHONY: build test size

build:
	$(DUNE) build @all

test:
	$(DUNE) build @runtest

size: build
	node size/size.mts
