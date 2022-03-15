PROBZELUC=probzeluc

ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

NAME=gaussian_tree

all: build

$(NAME).ml main.ml: $(ZCI) $(NAME).zls metrics.ml gaussian_tree_ds.ml gaussian_tree_semi_symb.ml
	$(PROBZELUC) -s main $(NAME).zls

metrics.ml : metrics.zls
	$(PROBZELUC) metrics.zls

gaussian_tree_ds.ml : gaussian_tree_ds.zls
	$(PROBZELUC) $<

gaussian_tree_semi_symb.ml : gaussian_tree_semi_symb.zls
	$(PROBZELUC) $<

clean:
	rm -f $(NAME).ml main.ml metrics.ml gaussian_tree_ds.ml gaussian_tree_semi_symb.ml
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

