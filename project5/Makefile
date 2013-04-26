.PHONY: all test clean

TESTS = $(wildcard tests/assignment4*.c)

PROGS = $(patsubst %.c,%,$(TESTS))

#PASSED = $(grep -c ./tests/results.txt)
#TESTS = $(wc -l ./tests/results.txt)

all: test

clean:
	rm -f ./tests/results.txt

test: $(PROGS)
	@./tests/test-results.sh

%: %.c
	@./tests/test.sh $<
