PROBZELUC=probzeluc

ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

NAME=kalman_delay

all: build

$(NAME).ml main.ml: $(ZCI) $(NAME).zls metrics.ml kalman_delay_ds.ml kalman_delay_semi_symb.ml
	$(PROBZELUC) -s main $(NAME).zls

metrics.ml : metrics.zls
	$(PROBZELUC) metrics.zls

kalman_delay_ds.ml : kalman_delay_ds.zls
	$(PROBZELUC) $<

kalman_delay_semi_symb.ml : kalman_delay_semi_symb.zls
	$(PROBZELUC) $<

clean:
	rm -f $(NAME).ml main.ml metrics.ml kalman_delay_ds.ml kalman_delay_semi_symb.ml
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

