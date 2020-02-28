include ../../Makefile.common-config

NAME=kalman_ds
RUN=$(BROOT)/kalman/ds/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../kalmanlib

all: build

publish:
	cp $(CSV_FILES) $(PPL-ZELUS)/data/kalman/ds

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
