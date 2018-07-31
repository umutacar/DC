#!/usr/bin/env python

# 
# Runs the parser with the specified filename
# and translates the code into the core language
# filename_core.dex 

import re
import sys
import os

import pervasives.syntax as syntax 
import parser

def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: parse inputfile'
    sys.exit()

  # get current working directory
  root_dir = os.getcwd()

  # get the file and its path
  infile_name = sys.argv[1]
  (path, infile_name) = os.path.split(infile_name) 

  # cd to path
  if path is not '':
    os.chdir(path)

  # dex: parser
  parents_optional = True
  titles_optional = True
  parser.main(infile_name, parents_optional, titles_optional)


if __name__ == "__main__":
    main(sys.argv)
