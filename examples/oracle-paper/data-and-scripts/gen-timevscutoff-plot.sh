#!/bin/bash

prog=$1
procs=$2
input_size=$3

cat<<FOO
set terminal png nocrop enhanced
set output 'timevscutoff-$prog.png'
set title "Time vs. cutoff (procs=$procs, input size=$3)"
set ylabel "time"
set xlabel "cutoff"
plot 'timevscutoff-$prog.txt'
FOO