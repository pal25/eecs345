.PHONY: all test

TESTS = $(wildcard tests/*.c)

PROGS = $(patsubst %.c,%,$(TESTS))

all: test

test: $(PROGS)

%: %.c
	@./tests/test.sh $<
