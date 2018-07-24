#!/usr/bin/python
import re
import sys
import os

import pervasives.syntax as syntax 

PYTHON = 'python'


######################################################################
## BEGIN: Fix various files
## REQUIRES DIDEROT_HOME environment variable
## REQUIRES LATEX_PREAMBLE file, see below

MTL_HOME = os.environ['MTL_HOME']
DEX_DIR =  MTL_HOME + '/dex_mlx/'

## END: Fix your username / path here.
######################################################################

# scripts
PARSER = DEX_DIR + 'parser.py'

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
  os.chdir(path)

  # dex: parser
  command = PYTHON + syntax.SPACE + PARSER + syntax.SPACE + infile_name
  print 'Executing command:', command
  if os.system(command):
    print "Fatal Error: command failed! Command = ", command

if __name__ == "__main__":
    main(sys.argv)
