EX=wheels
ALGO=ds_nogc
NAME=wheels_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

PROBZELUC += -I ../wheelslib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
