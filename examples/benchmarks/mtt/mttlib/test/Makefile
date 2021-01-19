PROBZELUC=probzeluc -I ..


ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

all: main.ml test.ml
	dune build main.exe

metrics :
	dune build test_metrics.exe

%.zci: %.zli
	$(PROBZELUC) $<

main.ml test.ml : test.zls 
	$(PROBZELUC) test.zls -s main

clean:
	dune clean
	-rm -f *.zci
cleanall: clean
	rm -f *~
