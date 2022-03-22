EX=trackerdelay
ALGO=ds_nogc
NAME=trackerdelay_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

PROBZELUC += -I ../trackerdelaylib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
