.PHONY: all clean conf build

all: conf build

clean:
	cabal clean

conf: clean
	cabal configure

build:
	cabal build
