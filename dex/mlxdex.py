#!/usr/bin/env python

# Translates dex to mlx.  
# A slightly different implementation of mlxdex_direct.py
#
# # It requires acces to MTL and MLX directories.
#   and the scripts within.
#   These are defined by environment variables
#   MTL_HOME and DIDEROT_HOME.
#
# # It uses a fixed latex preamble as defined by the 
#   global variable LATEX_PREAMBLE_FILE, which can also 
#   be set by passing an additional argument


import re
import sys
import os

import pervasives.syntax as syntax
import pervasives.os_utils as os_utils

## Some constants

PYTHON = 'python'
SPACE = ' '
PERIOD = '.'
DEX_EXTENSION = '.dex'
MLX_EXTENSION = '.mlx'

######################################################################
## BEGIN: Fix various files
## REQUIRES MTL_HOME environment variable
## REQUIRES DIDEROT_HOME environment variable
## REQUIRES LATEX_PREAMBLE file, see below

MTL_HOME = os.environ['MTL_HOME']
DIDEROT_HOME = os.environ['DIDEROT_HOME']
DEX_DIR =  MTL_HOME + '/dex/'
MLX_DIR =  DIDEROT_HOME + '/mlx/'
LATEX_PREAMBLE_FILE =  '/Users/umut/diderot/algobook/latex_preamble/preamble.tex'

## END: Fix your username / path here.
######################################################################

# scripts
DEX_PARSER = DEX_DIR + 'parser.py'
DEX_2_MLX = DEX_DIR + 'dex2mlx.py'
MLX_ELABORATE = MLX_DIR + 'elaborate.py'

def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) == 2:
    infile_name = sys.argv[1]
    latex_preamble_file = LATEX_PREAMBLE_FILE 
  elif len(sys.argv) == 3: 
    infile_name = sys.argv[1]
    latex_preamble_file = sys.argv[2]
  else:
    print 'Usage: mlxdex.py inputfile [latex_preamble_file =', LATEX_PREAMBLE_FILE, ']'
    sys.exit()


  # get current working directory 
  root_dir = os.getcwd()
  latex_preamble_file = root_dir + syntax.SLASH + root_dir


  print 'Executing:', sys.argv[0], infile_name, latex_preamble_file

  
  # get the file and its path
  (path, infile_name) = os.path.split(infile_name) 

  # cd to path
  if path is not '': 
    os.chdir(path)

  # create the various file names
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  core_infile = os_utils.mk_file_name_derivative(infile_name, os_utils.CORE)
  core_infile_mlx = os_utils.mk_file_name_ext(core_infile, os_utils.MLX_EXTENSION)
  outfile_mlx = os_utils.mk_file_name_ext(infile_name, os_utils.MLX_EXTENSION)
  outfile_mlx_elaborated = os_utils.mk_file_name_derivative(core_infile_mlx, os_utils.ELABORATED)

   # convert dex to core dex by using the parser
  command = PYTHON + syntax.SPACE + DEX_PARSER + syntax.SPACE + infile_name
  print 'Executing command:', command
  os.system(command)

  # convert core dex to mlx
  command = PYTHON + syntax.SPACE + DEX_2_MLX + syntax.SPACE + core_infile + syntax.SPACE + LATEX_PREAMBLE_FILE
  print 'Executing command:', command
  os.system(command)


  # elaborate mlx
  command = PYTHON + syntax.SPACE + MLX_ELABORATE + syntax.SPACE + core_infile_mlx + syntax.SPACE + outfile_mlx_elaborated
  print 'Executing command:', command
  os.system(command)

  # rename and copy to Desktop
  os_utils.mv_file_to(outfile_mlx_elaborated, outfile_mlx)  
  os_utils.cp_file_to(outfile_mlx, '~/Desktop/')  

  # cd to starting directory
  os.chdir(root_dir)

if __name__ == "__main__":
    main(sys.argv)
