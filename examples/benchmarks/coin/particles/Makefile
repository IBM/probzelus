include ../../Makefile.common-config

NAME=coin_particles
RUN=$(BROOT)/coin/particles/run.exe
CSV_FILES=perf.csv perf-step.csv mem.csv mem-ideal.csv accuracy.csv

ZELUC += -I ../coinlib

all: build

publish:
	cp $(CSV_FILES) $(PPL-ZELUS)/data/coin/particle

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common
