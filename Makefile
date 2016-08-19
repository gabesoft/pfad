
TIX := $(shell find . -name "*.tix")

repl:
	stack ghci --ghc-options "-package ghci-pretty"

clean-tix:
	$(RM) $(TIX)

build: clean-tix
	stack build

test: clean-tix
	stack build --test

# sample call: make run exe=hit-counter arg="http://"
run: build
	@stack exec $$exe -- $$arg

# sample call: make test-only test=funct-tests
test-only: clean-tix
	stack build --test hapro:$$test
