INFERLIB=../../../../inference
OWL=../../../../owl
ZELUC=zeluc -copy -I $(INFERLIB) -I $(OWL)


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml
	dune build trackerlib.cma

opt:
	dune build trackerlib.cmxa

metrics.ml : metrics.zls
	$(ZELUC) -noreduce $<

%.zci: %.zli
	$(ZELUC) $<

clean:
	dune clean
	-rm -f *.zci
cleanall: clean
	rm -f *~
