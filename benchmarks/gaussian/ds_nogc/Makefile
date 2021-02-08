EX=gaussian
ALGO=ds_nogc
NAME=gaussian_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

PROBZELUC += -I ../gaussianlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
