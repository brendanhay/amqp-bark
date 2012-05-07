.PHONY: all clean build

all: build

clean:
	cabal clean

conf: clean
	cabal configure

build:
	cabal build

run: build
	./bark
