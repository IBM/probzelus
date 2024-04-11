To compile and execute this example, you need [pyml](https://github.com/thierry-martinez/pyml) OCaml bindings for Pythom and [gymnasium](https://gymnasium.farama.org/index.html)
The (Python) Gymnasium package is called from OCaml via pyml.

The gymnasium package can be installed as follows.
``` shell
pip install gymnasium opencv-python pygame
```

The Ocaml binding can be installed as follows:

``` shell
opam install pyml
```

Then the example can be executed from `~/probzelus/examples/gym-cartpole` with

```
make [exec | exec_smart | exec_pid | exec_smart_pid | exec_simple_pid]
```
