#!/usr/bin/python
import re
import sys
import os

import pervasives.os as pos
import pervasives.syntax as syntax 

## Some constants

PYTHON = 'python'
SPACE = ' '
PERIOD = '.'
DEX_EXTENSION = '.dex'
DIL_EXTENSION = '.dil'
ELABORATED = '_ed'

######################################################################
## BEGIN: Fix various files
## REQUIRES DIDEROT_HOME environment variable
## REQUIRES LATEX_PREAMBLE file, see below

MTL_HOME = os.environ['MTL_HOME']
DEX_DIR =  MTL_HOME + '/dex/'
LATEX_PREAMBLE_FILE =  '/Users/umut/teach/adps-diderot/latex_preamble/preamble.tex'

## END: Fix your username / path here.
######################################################################

# scripts
ELABORATE = DEX_DIR + 'elaborate.py'
DEX_2_DIL = DEX_DIR + 'dex2dil.py'


def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: publish.py inputfile'
    sys.exit()


  # get current working directory
  root_dir = os.getcwd()

  # get the file and its path
  infile_name = sys.argv[1]
  (path, infile_name) = os.path.split(infile_name) 

  # cd to path
  os.chdir(path)

  # create the various file names
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  elaborated_file_dex = infile_name_first + pos.ELABORATED + pos.DEX_EXTENSION
  elaborated_file_dil = infile_name_first + pos.DIL_EXTENSION

  # dex: elobarate
  command = PYTHON + syntax.SPACE + ELABORATE + syntax.SPACE + infile_name
  print 'Executing command:', command
  os.system(command)

  # convert dex (elaborated) to dil
  command = PYTHON + syntax.SPACE + DEX_2_DIL + syntax.SPACE + elaborated_file_dex + syntax.SPACE + LATEX_PREAMBLE_FILE
  print 'Executing command:', command
  os.system(command)

  # cp to Desktop
  command = 'cp' + syntax.SPACE + elaborated_file_dil + syntax.SPACE + '~/Desktop/'
  print 'Executing command:', command
  os.system(command)

  # cd to starting directory
  os.chdir(root_dir)

if __name__ == "__main__":
    main(sys.argv)
