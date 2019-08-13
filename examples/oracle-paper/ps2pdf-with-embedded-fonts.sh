#!/bin/sh

gs -sDEVICE=pdfwrite -q -dBATCH -dNOPAUSE -dSAFER \
 -dPDFX \
 -dPDFSETTINGS=/prepress \
 -dAutoFilterColorImages=false -dColorImageFilter=/FlateEncode \
 -dAutoFilterGrayImages=false -dGrayImageFilter=/FlateEncode \
 -sOutputFile=$1.out.pdf $1.pdf
mv $1.pdf $1.in.pdf
mv $1.out.pdf $1.pdf
pdftops $1.pdf