EX=gaussian
ALGO=ds
NAME=gaussian_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv \
	per_step.csv per_step_mem.csv

ZELUC += -I ../gaussianlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
