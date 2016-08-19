
TEMP := $(shell find . -name ".\#*")
TIX := $(shell find . -name "*.tix")

repl:
	stack ghci --ghc-options "-package ghci-pretty"

clean-temp:
	$(RM) $(TEMP)

clean-tix:
	$(RM) $(TIX)

build: clean-temp clean-tix
	stack build

test: clean-temp clean-tix
	stack build --test

# sample call: make run exe=hit-counter arg="http://"
run: build
	@stack exec $$exe $$arg

# sample call: curl localhost:3000/google.com
run-hit-counter: build
	stack exec hit-counter "http://"

# sample call: make test-only test=funct-tests
test-only: clean-temp clean-tix
	stack build --test hapro:$$test

profile:
	stack ghc -- -prof -fprof-auto -rtsopts -O2 app/ProfileEx.hs
	app/ProfileEx +RTS -P
	cat ProfileEx.prof

profile-mem:
	stack ghc -- -prof -fprof-auto -rtsopts -O2 app/ProfileHeap.hs
	app/ProfileHeap +RTS -hc -p
	open ProfileHeap.hp

setup:
	stack setup
