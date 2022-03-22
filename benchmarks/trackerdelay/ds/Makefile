EX=trackerdelay
ALGO=ds
NAME=trackerdelay_ds

include ../../Makefile.common-config

CSV_FILES=perf.csv perf-step.csv mem-ideal.csv accuracy.csv

PROBZELUC+=-I ../trackerdelaylib

all: build

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
