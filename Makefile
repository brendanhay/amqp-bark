.PHONY: all clean conf build

build:
	cabal build

clean:
	cabal clean

conf: clean
	cabal configure

prof: clean
	cabal configure --enable-executable-profiling

all: conf build
