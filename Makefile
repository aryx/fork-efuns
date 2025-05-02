###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

.PHONY: all clean install test dump

###############################################################################
# Developer targets
###############################################################################

# -filter semgrep
visual:
	codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .

sync:
	@echo go to docs/literate/
