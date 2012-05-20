.PHONY: build all clean conf prof

build:
	cabal-dev build

all: conf build

clean:
	cabal-dev clean

conf:
	cabal-dev configure

prof: clean
	cabal-dev configure --enable-executable-profiling
	$(MAKE) build


