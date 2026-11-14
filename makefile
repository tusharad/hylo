.PHONY: all build test lint format format-check docs clean gate

all: build test

## Build project with Stack
build:
	stack build

## Run test suite
test:
	stack test --pedantic

## Run HLint linter
lint:
	hlint src/ app/ test/

## Format source code with Fourmolu
format:
	fourmolu -i src/ app/ test/

## Check code formatting with Fourmolu
format-check:
	fourmolu -m check src/ app/ test/

## Run formal verification gate
gate:
	./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell

## Build Haddock documentation
docs:
	stack haddock --no-haddock-deps

## Clean build artifacts
clean:
	stack clean
