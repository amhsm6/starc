#!/usr/bin/env bash

set -e

if (( $# != 1 )); then
    echo "Usage: $0 input"
    exit 1
fi

input=$1
filename=$(basename $input)
basename=${filename%.star}

asm=/tmp/$basename.s
obj=/tmp/$basename.o
elf=$basename

std=std/std.o

gleam run $input -o $asm
as -o $obj $asm
ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o $elf $obj $std -lc

rm $asm $obj
