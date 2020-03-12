EX=mtt
ALGO=ds_nogc
NAME=mtt_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

OWLLIB=../../../../owl
ZELUC += -I $(OWLLIB) -I ../mttlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
