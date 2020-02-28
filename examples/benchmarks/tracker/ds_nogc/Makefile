include ../../Makefile.common-config
ZELUC+=-I ../trackerlib -I ../../../../owl

NAME=tracker_ds_nogc
RUN=$(BROOT)/tracker/ds_nogc/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

all: build

publish:
	cp perf-step.csv mem.csv mem-ideal.csv $(PPL-ZELUS)/data/tracker/ds_nogc

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common
