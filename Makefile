.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

dkpsuler:
	@ln -s _build/install/default/bin/dkpsuler dkpsuler || true

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
