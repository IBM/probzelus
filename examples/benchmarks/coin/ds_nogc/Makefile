include ../../Makefile.common-config

NAME=coin_ds_nogc
RUN=$(BROOT)/coin/ds_nogc/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../coinlib

all: build

publish:
	cp perf-step.csv mem.csv mem-ideal.csv $(PPL-ZELUS)/data/coin/ds_nogc

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common
