include ../../Makefile.common-config
ZELUC+=-I ../slamlib

NAME=slam_ds_nogc
RUN=$(BROOT)/slam/ds_nogc/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

all: build

publish:
	cp perf-step.csv mem.csv mem-ideal.csv $(PPL-ZELUS)/data/slam/ds_nogc

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common
