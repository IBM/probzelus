include ../../Makefile.common-config

NAME=kalman_ds_nogc
RUN=$(BROOT)/kalman/ds_nogc/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../kalmanlib

all: build

publish:
	cp perf-step.csv mem.csv mem-ideal.csv $(PPL-ZELUS)/data/kalman/ds_nogc

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common
