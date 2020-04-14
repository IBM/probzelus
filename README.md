# ProbZelus

## Install

### Docker

The easiest way to use ProZelus is via the docker image.
```
make docker_build
make docker_run
```

On MacOS you need XQuartz for the examples with a graphical visualization.

### From source for MacOS

First install dependencies:
```
brew install openblas plplot
opam install dune menhir owl owl-plplot
```

Then install zelus-2.1 (binary distribution availabe in the `zelus-2.1` directory)
```
cd zelus-2.1/zelus-2.1-macos
./configure
make && make install
```

Finally install probzelus
```
make
```

To compile and execute the benchmark, you need the following additional dependencies:
```
opam install csv mtime
```


## Examples

A set of examples is available in the `examples` directory.
Most of them can be built and executed with:

```
make exec
```

The probzelus code is in the `*.zls` files


## Contributions

ProbZelus is still under development and we welcome contributions.
Contributors are expected to submit a 'Developer's Certificate of Origin' which can be found in [DCO1.1.txt](DCO1.1.txt).
