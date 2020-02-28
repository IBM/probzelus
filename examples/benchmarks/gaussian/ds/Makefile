include ../../Makefile.common-config

NAME=gaussian_ds
RUN=$(BROOT)/gaussian/ds/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../gaussianlib

all: build

publish:
	mkdir -p $(PPL-ZELUS)/data/gaussian/ds
	cp $(CSV_FILES) $(PPL-ZELUS)/data/gaussian/ds

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
