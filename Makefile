all:
	@echo "To build the probzelus, first install the zelus-owl package using opam:"
	@echo "    opam pin -n -k path zelus-owl"
	@echo "    opam install zelus-owl"
	@echo "Then:"
	@echo "    make -C probzelus"


docker_build:
	docker build -t probzelus -f probzelus.docker .
	
docker_run:
	xhost + 127.0.0.1 && \
	docker run -ti --rm -e DISPLAY=host.docker.internal:0 probzelus bash