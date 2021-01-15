PROBZELUC=probzeluc

NAME=line

all: build

$(NAME).ml main.ml: $(NAME).zls
	$(PROBZELUC) -s main $+

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
