#!/bin/bash
echo "Changing to /mtl."
cd /mtl
echo "Cleaning up."
make clean 
echo "Making now."
make
echo "Removing absolute symlinks."
rm -f ./*.native
rm -f ./*.dbg
rm -f ./*.profile


