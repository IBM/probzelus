For most of the programs in this directory, you can compile and execute them by typing:

```
make exec
```

More generally, ProbZelus programs are compiled with `probzeluc`, the ProbZelus compiler. A file `a.zls` whose main node is `main` can be compiled as follows:

```
probzeluc -s main a.zls
```

It generates an OCaml files `a.ml` and `main.ml` that has to be linked with the `probzelus` library.


The `naive_bayes_german` and `tracker` examples depend on the [owl-plplot](https://github.com/owlbarn/owl) library. This is an option that is not enable by default in ProbZelus. To compile the Zelus binding to the library, do:

```
opam install zelus-owl-plplot
```
