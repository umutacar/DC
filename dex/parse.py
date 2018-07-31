#!/usr/bin/env python
import re
import sys
import os

import pervasives.syntax as syntax 
import parser

def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: parser inputfile'
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
  command = PYTHON + syntax.SPACE + PARSER + syntax.SPACE + infile_name
  print 'Executing command:', command
  if os.system(command):
    print "Fatal Error: command failed! Command = ", command

if __name__ == "__main__":
    main(sys.argv)
