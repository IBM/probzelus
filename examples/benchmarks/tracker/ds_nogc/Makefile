EX=tracker
ALGO=ds
NAME=tracker_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv \
	per_step.csv per_step_mem.csv

ZELUC += -I ../trackerlib -I ../../../../owl

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
