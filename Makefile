docker_build:
	docker build -t probzelus -f probzelus.docker .
	
docker_run:
	xhost + 127.0.0.1 && \
	docker run -ti --rm -e DISPLAY=host.docker.internal:0 probzelus bash