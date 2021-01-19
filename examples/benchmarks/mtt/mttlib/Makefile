PROBZELUC=probzeluc


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml
	dune build mttlib.cma

opt: metrics.ml
	dune build mttlib.cmxa

test:
	dune build test.exe

%.zci: %.zli
	$(PROBZELUC) $<

test.ml : test.zls util.zci
	$(PROBZELUC) test.zls -s main

metrics.ml : metrics.zls
	$(PROBZELUC) -noreduce metrics.zls

clean:
	dune clean
	-rm -f *.zci metrics.ml
cleanall: clean
	rm -f *~
