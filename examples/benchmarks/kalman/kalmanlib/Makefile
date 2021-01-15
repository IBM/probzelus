PROBZELUC=probzeluc


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml
	dune build kalmanlib.cma

opt: metrics.ml
	dune build kalmanlib.cmxa

%.zci: %.zli
	$(PROBZELUC) $<

metrics.ml : metrics.zls
	$(PROBZELUC) -noreduce metrics.zls

clean:
	dune clean
	-rm -f *.zci metrics.ml
cleanall: clean
	rm -f *~
