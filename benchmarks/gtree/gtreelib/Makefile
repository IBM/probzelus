PROBZELUC=probzeluc


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml config.ml
	dune build gtreelib.cma

opt: metrics.ml config.ml
	dune build gtreelib.cmxa

%.zci: %.zli
	$(PROBZELUC) $<

metrics.ml : metrics.zls
	$(PROBZELUC) -noreduce metrics.zls

config.ml : config.zls
	$(PROBZELUC) -noreduce $<

clean:
	dune clean
	-rm -f *.zci metrics.ml config.ml
cleanall: clean
	rm -f *~
