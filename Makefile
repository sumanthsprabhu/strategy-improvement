SETUP = ocaml setup.ml

all: build

.PHONY: ark simsat

build: setup.ml setup.data
	$(SETUP) -build

ark: setup.ml setup.data
	ocamlbuild ark/test_ark.native ark/arkTop.native -tag debug

patools: setup.ml setup.data
	ocamlbuild simsat/simsat.native -tag debug

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	$(SETUP) -configure --enable-tests

install:
	$(SETUP) -install

clean:
	$(SETUP) -clean

test:
	$(SETUP) -test

uninstall:
	$(SETUP) -uninstall

reinstall:
	$(SETUP) -reinstall

doc: setup.ml setup.data
	$(SETUP) -doc
