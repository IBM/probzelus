CC=gcc 
.PHONY: clean 

all : test_time
	./test_time

test_time : test_time.c
	$(CC) -o $@ $<

clean:
	rm -f test_time
