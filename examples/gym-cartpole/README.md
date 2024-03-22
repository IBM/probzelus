To compile and execute this example, you need [pyml](https://github.com/thierry-martinez/pyml) OCaml bindings for Pythom and [gymnasium](https://gymnasium.farama.org/index.html)
The (Python) Gymnasium package is called from OCaml via pyml.

The gymnasium package can be installed as follows. First, create a conda environment called gymnasium. Then, install gymnasium.

``` shell
conda create --name gymnasium python=3.11
conda activate gymnasium
pip install gymnasium
```

The first time I had tried this, I had an error message ("failed to open iris") which can be fixed with

``` shell
conda install -c conda-forge libstdcxx-ng
```


The Ocaml binding can be installed as follows:

``` shell
opam install pyml
```

To execute this example, you first have to activate the (conda) gymnasium environment:

``` shell
conda activate gymnasium
```

Then the example can be executed from `~/probzelus/examples/gym-cartpole` with

```
make [exec | exec_smart | exec_pid | exec_smart_pid | exec_simple_pid]
```
