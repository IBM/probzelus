INFERLIB=../../../../inference
ZELUC=zeluc -noreduce -copy -I $(INFERLIB)


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: $(ZCI) byte opt

.phony: byte opt

byte: lib.ml metrics.ml
	dune build slamlib.cma

opt: lib.ml metrics.ml
	dune build slamlib.cmxa

lib.zci lib.ml: lib.zls
	$(ZELUC) $<

metrics.zci metrics.ml : metrics.zls
	$(ZELUC) $<

%.zci: %.zli
	$(ZELUC) $<

clean:
	dune clean
	-rm -f *.zci lib.ml metrics.ml
cleanall: clean
	rm -f *~
