#!/usr/bin/env python

# Translates dex to mlx, taking in a latex_preamble_file
# as an argument.
#
# Requires dex and mlx packages to be accessible (via PYTHONPATH)

import re
import sys
import os

import pervasives.syntax as syntax
import pervasives.os_utils as os_utils
import parser
import dex2mlx

## Some constants

SPACE = ' '
PERIOD = '.'
DEX_EXTENSION = '.dex'
MLX_EXTENSION = '.mlx'


def main(infile_name, latex_preamble_file):
  # get current working directory
  root_dir = os.getcwd()

  # get the file and its path
  (path, infile_name_file) = os.path.split(infile_name) 

  # create the various file names
  (infile_name_first, infile_ext) = infile_name_file.split (syntax.PERIOD) 
  core_infile = os_utils.mk_file_name_derivative(infile_name, os_utils.CORE)
  core_infile_mlx = os_utils.mk_file_name_ext(core_infile, os_utils.MLX_EXTENSION)
  outfile_mlx = os_utils.mk_file_name_ext(infile_name, os_utils.MLX_EXTENSION)

   # convert dex to core dex by using the parser
  parser.main(infile_name, True, True)
  print 'Translating', infile_name, 'to core language.'

  # convert core dex to mlx
  print 'Translating to mlx'
  dex2mlx.main(core_infile, latex_preamble_file)


  print 'Elaborating mlx', core_infile_mlx
  mlx.elaborate.main(core_infile_mlx, outfile_mlx_elaborated)

  # rename and copy to Desktop
  os_utils.mv_file_to(core_infile_mlx, outfile_mlx)  
  print 'mlxdex completed.  Output is in', outfile_mlx

  # cd to starting directory
  os.chdir(root_dir)

if __name__ == "__main__":
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 3: 
    print 'Usage: mlxdex.py inputfile latex_preamble_file'
    sys.exit()

  infile_name = sys.argv[1]
  latex_preamble_file = sys.argv[2]
  
  main(infile_name, latex_preamble_file)
