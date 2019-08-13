#!/bin/bash

rm -f $1.mlb
echo "../lib/parallel.mlb" >> $1.mlb
echo $1.pml >> $1.mlb

rm -f $1_sq.mlb
echo "../lib/parallel.mlb" >> $1_sq.mlb
echo $1_sq.pml >> $1_sq.mlb

rm -f $1_ws.mlb
echo "../lib/parallel.mlb" >> $1_ws.mlb
echo $1_ws.pml >> $1_ws.mlb

