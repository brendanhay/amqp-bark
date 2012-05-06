.PHONY: all clean

all: build

clean:
	cabal clean

configure: clean
	cabal configure

build:
	cabal build
