#!/usr/bin/python
import re
import sys


import pervasives.os as os 
import pervasives.syntax as syntax 


def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: dex2latex inputfile'
    sys.exit()

  infile_name = sys.argv[1]
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  latexfile_name = infile_name_first + os.LATEX_EXTENSION
  
  infile = open(infile_name, 'r')
  latexfile = open(latexfile_name, 'w')

  in_data = infile.read ()

#  print 'Input\n', data


  preamble = r'''\documentclass{book}
\usepackage{diderot}
\begin{document}
''' 

  postamble = r'\end{document}'

  latexfile.write(preamble)
  latexfile.write(in_data + '\n')
  latexfile.write(postamble)
  latexfile.write('\n')
  latexfile.close()

  print 'LaTex code is in file:', latexfile_name

if __name__ == "__main__":
    main(sys.argv)
