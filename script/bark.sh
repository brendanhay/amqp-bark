#!/bin/sh

./script/sample.sh | ./cabal-dev/bin/bark --service=prof --tee
