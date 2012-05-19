.PHONY: build all clean conf prof

build:
	cabal build

all: conf build

clean:
	cabal clean

conf: clean
	cabal configure

prof: clean
	cabal configure --enable-executable-profiling
	$(MAKE) build
	cat ./script/prefix.log | ./dist/build/bark/bark --service=prof


