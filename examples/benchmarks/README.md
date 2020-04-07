# Benchmarks

This directory contains the benchmarks used in the PLDI 2020 paper entitled _Reactive Probabilistic Programming_ by Guillaume Baudart, Louis Mandel, Eric Atkinson, Benjamin Sherman, Marc Pouzet, and Michael Carbin.

# Step by Step Execution

The different benchmarks are in the following directories:
- `coin` for Beta-Bernoulli
- `gaussian` for Gaussian-Gaussian
- `kalman` for Kalman-1D
- `outlier` for Outlier
- `tracker` for Robot
- `slam` for the Simultaneous Location And Mapping
- `mtt` for the Multi-Target Tracker.

We will use the name `ex` when we want to refer to any of the `coin`, `gaussian`, `kalman`, `outlier`, `tracker`, `slam` or `mtt` directories.

For each of the benchmarks, we provide implementations with four different inference algorithms.
In each `ex` directory, you will find:
- `particles` for the particle filter (PF)
- `ds` for streaming delayed sampling (SDS)
- `ds_bounded` for bounded delayed sampling (BDS)
- `ds_nogc` for naive delayed sampling (DS).

We will use the name `algo` when we want to refer to any of the `particles`, `ds`, `ds_bounded`, or `ds_nogc` directories.

The `~/probzelus/examples/benchmarks` directory also contains:
- `harness` and `tools` that contain utility code to execute the benchmarks and analyze them
- `plot` for generating graphs after generating the benchmarks results
- `Makefile`, `Makefile.common-config`, `Makefile.common-ex`, `Makefile.common-algo` for building and execution.

Each `ex` directory has a similar structure.
In addition to the `algo`s directories, you will find:
- `exlib` that contains auxiliary code that is shared between all the implementations of the model. In this directory, there is in particular always a file `metrics.zls` that defines the error metrics used by the benchmark.
- `data` is the file containing the input data that has been used as input to the benchmark.
- `gen_data.zls` contains the code to generate data for the benchmark.
- `Makefile` and `dune` are files for the build system.

Each `algo` directory contains:
- `ex_algo.zls`: the code of the model.
- `run.ml`: the entry point of execute the program (which is an instantiation of the functor defined in `harness/harness.ml`).

## Building the benchmarks

The first thing to do is to build all the benchmarks:
```
make build
```

## Execute one of the benchmarks

Each implementation of the benchmark can be executed individually.
To run the benchmark `ex` (`coin`, `gaussian`, `kalman`, `outlier`, `tracker`, `slam` or `mtt`) with the algorithm `algo` (`particles`, `ds`, , `ds_bounded`, or `ds_nogc`) on the data `ex/data`, use the command:

```
dune exec ./ex/algo/run.exe -- -result -particles 100 < ./ex/data
```

It outputs a time stamp, the name of the benchmark, the name of the algorithm, the number of particles used, the accumulated loss at the last step, the total execution time in ms, the number of live words in the heap (if the option `-mem-ideal false` is provided), the computed result at the last step.

If the option `-step` is provided, you can see the results after each execution step of the program.

The complete list of options can be obtained with:

```
dune exec ./ex/algo/run.exe -- --help
```

## Execute all the benchmarks

Under `~/probzelus/examples/benchmarks` all `ex` directories and `algo` subdirectories contain a `Makefile` to automate the execution of the experiments.
The following commands will work in any of these directories

`make bench_per_particles` executes all the benchmarks with a range of different particles numbers.
It generates in each `ex/algo` directory a result file `per_particles.csv`.
It produces, in particular, the data to generate Figures 10 and 11 and the Figures 21 to 24 of Appendix F.

This command can be configured with the following makefile variables:
 - `NUMRUNS`: the number of times to run each experiment
 - `MIN` and `MAX`: define the range for the number of particles used in the experiments
 - `WARMUP`: the number of execution before recording the statistics.

For example the following command launches 50 runs for a number of particles ranging from 1 to 500:

```
make bench_per_particles MIN=1 MAX=500 NUMRUNS=50
```


The `make bench_per_steps` command executes all the benchmarks with a given number of particles and record the statistics of each execution step.
It generates in each `ex/algo` directory a result file `per_step.csv`.
It produces in particular the data to generate Figures 25 to 27 of Appendix F.

This command can be configured with the following makefile variables:
 - `NUMRUNS`: the number of runs for each experiment
 - `P`: the number of particles used to execute the experiments
 - `WARMUP`: the number of runs to execute before recording the statistics.


The `make bench_mem_ideal` command executes all the benchmarks with a given number of particles and record the statistics of ideal memory consumption (after GC compaction) at each execution step.
It generates in each `ex/algo` directory a result file `per_step_mem.csv`.
It produces in particular the data to generate Figures 26 to 28 of Appendix F.

This command can be configured with the following makefile variables:
 - `P`: the ranges of number of particles hon which to execute the experiments.


The `make bench` command executes the three targets `bench_per_particles`, `bench_per_steps`, and `bench_mem_ideal`.
The execution of the benchmarks can be parallelized the `-j` option of `make`:

```
make -j 8 MIN=1 MAX=1000 P=100 NUMRUNS=50 bench
```

Be careful, each time the benchmarks are executed, the data are appended to the files `per_particles.csv`, `per_step.csv`, and `per_step_mem.csv`.


## Analyzing the results

Once the files `per_particles.csv`, `per_step.csv`, and `per_step_mem.csv` are generated.
The summary of the results used to build the figures of Appendix F can be generate:

```
make analyze
```

It generates in each `ex/algo` directory the files:
 - `accuracy.csv`: corresponding to Figures 21 and 23
 - `perf.csv`: corresponding to Figures 22 and 24
 - `perf-step.csv`: corresponding to Figures 25 and 27
 - `mem-ideal.csv `: corresponding to Figures 26 and 28

Summary of individual `csv` file can also be produced with the command:

```
dune exec ./tools/analyze_csv.exe -- --help
```

Finally, the `plot` directory contains some gnuplot scripts that allows to generate figures similar to the ones of Appendix F.
The command `make -C plot` generates all the figures in `png` format.
A particular figure can be build with the `EX` variable and `ex` target:

```
make -C plot EX=coin ex
```

Note that the files `per_particles.csv`, `per_step.csv`, and `per_step_mem.csv` are incrementally created during the execution of the benchmarks.
It is thus possible of analyze partial results during the execution of the benchmark.

Figure 10 and 11 of the paper are automatically generated using Python.
The script first lookup the expected loss for each benchmark the `ex/ds/accuracy.csv` files that define the baseline.
Then it finds the numbers of particles to reach the baseline for each algorithms in the corresponding `ex/algo/accuracy.csv` files.
Finally, it gets the timing information for the given number of particles in `ex/algo/perf.csv`.
The Python script is given in the Jupyter notebook `plot/summary.ipynb` that can be launched as follows:

```
jupyter notebook
```

## Regenerate data

In order to be able to compare different inference algorithms, we execute the programs on a fixed set of data.
These data are available in `ex/data`.
New data can be generated for each benchmark using the `ex/gen_data.zls` program:

```
dune exec ./ex/main.exe | head -n 500 > ./ex/data
```
