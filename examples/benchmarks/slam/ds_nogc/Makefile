EX=slam
ALGO=ds_nogc
NAME=slam_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

PROBZELUC += -I ../slamlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
