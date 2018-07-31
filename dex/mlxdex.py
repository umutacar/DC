#!/usr/bin/env python

# Translates dex to mlx.  
# A slightly different implementation of mlxdex_direct.py
#
# # It requires acces to MTL and MLX directories.
#   and the scripts within.
#   These are defined by environment variables
#   MTL_HOME and DIDEROT_HOME.
#
# # It assumes a fixed latex preamble as defined by the 
#   global variable LATEX_PREAMBLE_FILE


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
LATEX_PREAMBLE_FILE =  MTL_HOME + '/adps/latex_preamble/preamble.tex'

## END: Fix your username / path here.
######################################################################

# scripts
DEX_PARSER = DEX_DIR + 'parser.py'
DEX_2_MLX = DEX_DIR + 'dex2mlx.py'
MLX_ELABORATE = MLX_DIR + 'elaborate.py'

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
