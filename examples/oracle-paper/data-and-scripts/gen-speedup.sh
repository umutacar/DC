#!/bin/bash

prog=$1
cutoff=$2
input_size=$3

cat<<FOO
set terminal png nocrop enhanced
set output 'speedup-$prog.png'
set title "Speedup curves (procs=$procs, cutoff=$2, input size=$3)"
set ylabel "time"
set xlabel "speedup"
plot 'speedup-$prog.txt'
FOO