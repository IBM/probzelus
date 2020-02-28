include ../../Makefile.common-config

NAME=kalman_particles
RUN=$(BROOT)/kalman/particles/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../kalmanlib

all: build

publish:
	cp $(CSV_FILES) $(PPL-ZELUS)/data/kalman/particle

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
