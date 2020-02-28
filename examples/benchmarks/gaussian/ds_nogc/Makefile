include ../../Makefile.common-config

NAME=gaussian_ds_nogc
RUN=$(BROOT)/gaussian/ds_nogc/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../gaussianlib

all: build

publish:
	mkdir -p $(PPL-ZELUS)/data/gaussian/ds_nogc
	cp perf-step.csv mem.csv mem-ideal.csv $(PPL-ZELUS)/data/gaussian/ds_nogc

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common
