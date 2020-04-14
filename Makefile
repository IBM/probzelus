all:
	$(MAKE) -C owl
	$(MAKE) -C inference
	
examples: all
	$(MAKE) -C examples
	
with-plplot: all
	$(MAKE) -C owl-plplot
	
docker_build:
	docker build -t probzelus -f probzelus.docker .
	
docker_run:
	xhost + 127.0.0.1 && \
	docker run -ti --rm -e DISPLAY=host.docker.internal:0 probzelus bash

clean:
	$(MAKE) -C owl clean
	$(MAKE) -C owl-plplot clean
	$(MAKE) -C inference clean
	$(MAKE) -C examples clean

cleanall:
	-rm -f *~
	$(MAKE) -C owl cleanall
	$(MAKE) -C owl-plplot cleanall
	$(MAKE) -C inference cleanall
	$(MAKE) -C examples cleanall

.phony: examples