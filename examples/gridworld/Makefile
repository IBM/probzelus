PROBZELUC=probzeluc

ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte:
	dune build gridworld.cma

opt:
	dune build gridworld.cmxa

%.zci: %.zli
	$(PROBZELUC) $<

clean:
	dune clean
	-rm -f *.zci
cleanall: clean
	rm -f *~
