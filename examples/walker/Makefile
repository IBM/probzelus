PROBZELUC=probzeluc

ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

NAME=walker


all: build

$(NAME).ml main.ml: $(ZCI) $(NAME).zls
	$(PROBZELUC) -s main $(NAME).zls

clean:
	rm -f $(NAME).ml main.ml
	rm -f *.zci

## Generic rules

.phony: build exec

exec: main.ml
	dune exec ./main.exe

build: main.ml
	dune build main.exe

%.zci: %.zli
	$(PROBZELUC) $<

cleanall: clean
	dune clean
	rm -f *~


hybrid:
	probzeluc -s main walker_cont.zls
	ocamlfind ocamlc -g -linkpkg -package probzelus walker_cont.ml main.ml -o main