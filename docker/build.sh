#!/bin/bash
echo "Changing to /mtl."
cd /mtl
echo "Cleaning up."
make clean 
echo "Making now."
make
echo "Removing absolute symlinks."
rm ./*.native
rm ./*.dbg
rm ./*.profile


