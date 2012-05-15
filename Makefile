.PHONY: all clean conf build

build:
	cabal build

clean:
	cabal clean

conf: clean
	cabal configure

all: conf build
