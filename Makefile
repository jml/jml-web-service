.PHONY: all build test clean lint
.DEFAULT_GOAL := all

# Flags to pass to stack.
STACK_FLAGS :=

# Flags to pass to stack build & test. Typically `--fast`
STACK_BUILD_FLAGS :=

all: test build lint

lint:
	hlint .

# stack does its own dependency management and it's a fool's errand to try to
# second-guess it. Instead, just always run stack when we think we need a build.
build:
	stack $(STACK_FLAGS) build $(STACK_BUILD_FLAGS)

test: build
	stack $(STACK_FLAGS) test $(STACK_BUILD_FLAGS)

clean:
	stack $(STACK_FLAGS) clean
