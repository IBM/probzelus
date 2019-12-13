# ProbZelus

## Install

### Docker

The easiest way to use ProZelus is via the docker image.
```
docker build -t probzelus -f probzelus.docker .
docker run -ti --rm -e DISPLAY=host.docker.internal:0 probzelus bash
```

On MacOS you need XQuartz for the examples with a graphical visualization.
Remember to allow connections in XQuartz settings, and add localhost:
```
xhost + 127.0.0.1
```

### From source for MacOS

First install dependencies:
```
brew install openblas plplot
opam install dune menhir owl owl-plplot
```

Then install zelus-2.0 (binary distribution availabe in the `zelus-2.0` directory)
```
cd zelus-2.0/zelus-2.0-macos
./configure
make && make install
```

Finally install probzelus
```
make
```

## Examples

A set of examples is available in the `examples` directory.
Most of them can be built and executed with:

```
make exec
```

The probzelus code is in the `*.zls` files
