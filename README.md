# ProbZelus

ProbZelus is synchronous probabilistic programming language. It is a conservative extension of [Zelus](http://zelus.di.ens.fr/) with probabilistic constructs to model uncertainties and perform inference-in-the-loop.

More information about ProbZelus are available in the paper [Reactive Probabilistic Programming](https://arxiv.org/abs/1908.07563).

```
@inproceedings{rppl-plid20,
  author = {Baudart, Guillaume and Mandel, Louis and Atkinson, Eric and Sherman, Benjamin and Pouzet, Marc and Carbin, Michael},
  title = {Reactive Probabilistic Programming},
  year = {2020},
  booktitle = {Proceedings of the 41st ACM SIGPLAN Conference on Programming Language Design and Implementation},
  series = {PLDI 2020}
}
```

## Install

The easiest way to install probzelus is via [Opam](https://opam.ocaml.org/), the OCaml package manager.
You first need to install [openblas](https://www.openblas.net/).

Then pin the packages defined in this repo to add them to opam.
```
opam pin -k path -n zelus-owl
opam pin -k path -n probzelus
```

You can now install ProbZelus with
```
opam install probzelus
```


An optional plplot library based on owl-plplot that can be built with:
```
opam install zelus-owl-plplot
```

### Docker

We also provide a docker image.
```
make docker_build
make docker_run
```

## Examples

A set of examples is available in the `examples` directory.
Most of them can be built and executed with:

```
make exec
```

The probzelus code is in the `*.zls` files

Most of the examples requires a X11 server. It can be install on MacOS using [XQuartz](https://www.xquartz.org/).

To compile and execute the benchmark, you need the following additional dependencies:
```
opam install csv mtime
```

## Contributions

ProbZelus is still under development and we welcome contributions.
Contributors are expected to submit a 'Developer's Certificate of Origin' which can be found in [DCO1.1.txt](DCO1.1.txt).
