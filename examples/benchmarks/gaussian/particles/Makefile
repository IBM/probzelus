include ../../Makefile.common-config

NAME=gaussian_particles
RUN=$(BROOT)/gaussian/particles/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../gaussianlib

all: build

publish:
	mkdir -p $(PPL-ZELUS)/data/gaussian/particle
	cp $(CSV_FILES) $(PPL-ZELUS)/data/gaussian/particle

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
