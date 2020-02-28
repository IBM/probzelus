INFERLIB=../../../inference
OWLLIB=../../../owl
ZELUC=zeluc -copy -I $(INFERLIB) -I $(OWLLIB) -I mttlib

ZLI=$(wildcard mttlib/*.zli)
ZCI=$(ZLI:zli=zci)

NAME=gen_data

all: build
	$(MAKE) -C particles
	$(MAKE) -C ds
	$(MAKE) -C ds_bounded
	$(MAKE) -C ds_nogc

publish:
	$(MAKE) -C particles publish
	$(MAKE) -C ds publish
	$(MAKE) -C ds_bounded publish
	$(MAKE) -C ds_nogc publish

bench: bench_particles bench_ds bench_ds_bounded bench_ds_nogc

bench_particles:
	$(MAKE) -C particles bench

bench_ds:
	$(MAKE) -C ds bench

bench_ds_bounded:
	$(MAKE) -C ds_bounded bench

bench_ds_nogc:
	$(MAKE) -C ds_nogc bench

bench_per_particles:
	$(MAKE) -C particles bench_per_particles
	$(MAKE) -C ds bench_per_particles
	$(MAKE) -C ds_bounded bench_per_particles
	echo "Skip:" $(MAKE) -C ds_nogc bench_per_particles

bench_per_steps:
	$(MAKE) -C particles bench_per_steps
	$(MAKE) -C ds bench_per_steps
	$(MAKE) -C ds_bounded bench_per_steps
	$(MAKE) -C ds_nogc bench_per_steps

bench_mem_ideal:
	$(MAKE) -C particles bench_mem_ideal
	$(MAKE) -C ds bench_mem_ideal
	$(MAKE) -C ds_bounded bench_mem_ideal
	$(MAKE) -C ds_nogc bench_mem_ideal

analyze:
	$(MAKE) -C particles analyze
	$(MAKE) -C ds analyze
	$(MAKE) -C ds_bounded analyze
	$(MAKE) -C ds_nogc analyze

.PHONY: publish bench build \
	bench_particles bench_ds bench_ds_bounded bench_ds_nogc

build: $(ZCI) main.ml
	$(MAKE) -C mttlib
	dune build main.exe


%.zci: %.zli
	$(ZELUC) $<


$(NAME).ml main.ml: $(NAME).zls
	$(ZELUC) -s main $(NAME).zls

clean:
	-rm -f *.zc*
	-rm -f $(NAME).ml main.ml
	$(MAKE) -C mttlib clean
	$(MAKE) -C particles clean
	$(MAKE) -C ds clean
	$(MAKE) -C ds_bounded clean
	$(MAKE) -C ds_nogc clean

cleanall: clean
	-rm -f *~
	dune clean
	$(MAKE) -C particles cleanall
	$(MAKE) -C ds cleanall
	$(MAKE) -C ds_bounded cleanall
	$(MAKE) -C ds_nogc cleanall
