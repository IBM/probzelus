EX=kalman
ALGO=copy
NAME=kalman_copy_instrumented
INFERLIB=../../../inference
ZELUC=zeluc -copy -I $(INFERLIB)
ZELUC += -I ../kalmangslib

$(NAME).ml : $(NAME.zls)
	$(ZELUC) -noreduce $(NAME).zls

%.zci : %.zli
	$(ZELUC) $<

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
