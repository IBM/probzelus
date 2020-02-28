include ../../Makefile.common-config

NAME=outlier_ds_bounded
RUN=$(BROOT)/outlier/ds_bounded/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../outlierlib

all: build

publish:
	cp $(CSV_FILES) $(PPL-ZELUS)/data/outlier/ds_bounded

bench: bench_per_particles bench_mem_ideal bench_per_steps

include ../../Makefile.common
