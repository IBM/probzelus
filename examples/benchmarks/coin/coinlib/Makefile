INFERLIB=../../../../inference
OWLLIB=../../../../owl
ZELUC=zeluc -copy -I $(INFERLIB) -I $(OWLLIB)


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: metrics.ml
	dune build coinlib.cma

opt: metrics.ml
	dune build coinlib.cmxa

%.zci: %.zli
	$(ZELUC) $<

metrics.ml : metrics.zls
	$(ZELUC) -noreduce metrics.zls

clean:
	dune clean
	-rm -f *.zci metrics.ml
cleanall: clean
	rm -f *~
