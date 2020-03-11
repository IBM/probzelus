EX=slam
ALGO=ds
NAME=slam_ds

include ../../Makefile.common-config

CSV_FILES=perf.csv perf-step.csv mem-ideal.csv accuracy.csv \
	per_particles.csv per_step.csv per_step_mem.csv

ZELUC += -I ../slamlib

all: build

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
