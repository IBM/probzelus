EX=kalman
ALGO=copy
NAME=kalman_copy_instrumented
INFERLIB=../../../inference
ZELUC=zeluc -copy -I $(INFERLIB)
ZELUC += -I ../kalmangslib
PROBZELUC = probzeluc
PROBZELUC += -I ../kalmangslib

$(NAME).ml : $(NAME.zls)
	$(PROBZELUC) -noreduce -nopt -nosimplify -inline 0 $(NAME).zls

%.zci : %.zli
	$(PROBZELUC) $<

build: $(NAME).ml run.ml
	dune build run.exe

exec: build
	dune exec ./run.exe < ../data

clean:
	-rm -f *.zc*
	-rm -f $(NAME).ml

cleanall: clean
	dune clean
	-rm -f *~

all: build
