#!/bin/bash

prog=$1
procs=$2
cutoff=$3

cat<<FOO
set terminal png nocrop enhanced
set output 'spread-$prog.png'
set title "Spread of execution times (procs=$procs, cutoff=$cutoff)"
set ylabel "time"
set xlabel "input size"
plot 'spread-$prog.txt'
FOO