#!/bin/bash
echo "Changing to /dc."
cd /dc
echo "Cleaning up."
make clean 
echo "Making now."
make
echo "Removing absolute symlinks."
rm -f ./*.native
rm -f ./*.dbg
rm -f ./*.profile


