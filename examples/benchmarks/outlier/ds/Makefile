include ../../Makefile.common-config

NAME=outlier_ds
RUN=$(BROOT)/outlier/ds/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../outlierlib

all: build

publish:
	cp $(CSV_FILES) $(PPL-ZELUS)/data/outlier/ds

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
