PROBZELUC=probzeluc


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml utils.ml
	dune build trackerlib.cma

opt: metrics.ml utils.ml
	dune build trackerlib.cmxa

metrics.ml : metrics.zls
	$(PROBZELUC) -noreduce $<

utils.ml : utils.zls lib.zci
	$(PROBZELUC) -noreduce $<

%.zci: %.zli
	$(PROBZELUC) $<

clean:
	dune clean
	-rm -f *.zci metrics.ml utils.ml
cleanall: clean
	rm -f *~
