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

This will install a `probzeluc` executable and the `probzelus` library in you Opam/OCaml echosystem.

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

## A Simple Example

Consider the example of a Hidden Markov Model. 
The probzelus code is the following

```ocaml
open Probzelus
open Distribution
open Infer_pf

let proba hmm obs = p where
  rec p = sample (gaussian(0.0 fby p, 1.0))
  and () = observe (gaussian(p, 0.5), obs)

let node main () = () where
  rec obs = 0.0 fby (obs +. 1.1)
  and pos_dist = infer 1000 hmm obs
  and mean, std = stats_float pos_dist
  and () = 
    print_string " mean: "; print_float mean; 
    print_string " std: ";  print_float std; 
    print_newline ()
```

We assume that at each step the position `p` is not too far from the previous position `0.0 fby p`, and that this position is also close to the observation `obs`.

Node `main` launches the inference with `1000` particles and print the mean and standard deviation of the inferred distribution at each step.
Here the observations are defined with a simple equation: starting from 0.0, at each step we add 1.1 to the value of previous observation. 

```
obs = 0.0, 1.1, 2.2, 3.3, 4.4, 5.5, ...
```

### Compilation

The `probzeluc` executable is a wrapper around the Zelus compiler. 
It takes a zelus file (e.g., `hmm.zls`) and compiles it to OCaml code (e.g., `hmm.ml`)

```
probzeluc hmm.zls
```

You can also specify a simulation node.
The compiler then generates an additional files containing the simulation code (e.g., `main.ml`).

```
probzeluc -s main hmm.zls
```

To build an executable, you can then compile the simulation code using the `probzelus` library.

```
ocamlfind ocamlc -linkpkg -package probzelus hmm.ml main.ml -o hmm
```

## Other Examples

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
