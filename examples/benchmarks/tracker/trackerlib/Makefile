INFERLIB=../../../../inference
OWL=../../../../owl
ZELUC=zeluc -copy -I $(INFERLIB) -I $(OWL)


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml utils.ml
	dune build trackerlib.cma

opt: metrics.ml utils.ml
	dune build trackerlib.cmxa

metrics.ml : metrics.zls
	$(ZELUC) -noreduce $<

utils.ml : utils.zls lib.zci
	$(ZELUC) -noreduce $<

%.zci: %.zli
	$(ZELUC) $<

clean:
	dune clean
	-rm -f *.zci metrics.ml utils.ml
cleanall: clean
	rm -f *~
