#!/bin/sh

input=./script/prefix.log

exec<$input

while read line
do
    echo $line;
    sleep 0.1;
done
