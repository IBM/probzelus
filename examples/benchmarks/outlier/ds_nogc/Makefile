EX=outlier
ALGO=ds_nogc
NAME=outlier_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

ZELUC += -I ../outlierlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
