#!/usr/bin/env python

## IMPORTANT:
## This is a copy of ~/mtl/dex_mlx/mlxdex.py
## It should always be maintained as such.

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


def main(infile_name, latex_preamble_file):
  # get current working directory
  root_dir = os.getcwd()

  # get the file and its path
  (path, infile_name) = os.path.split(infile_name) 

  # cd to path
  if path is not '': 
    os.chdir(path)

  # create the various file names
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  parsed_infile = os_utils.mk_file_name_derivative(infile_name, os_utils.PARSED)
  parsed_infile_mlx = os_utils.mk_file_name_ext(parsed_infile, os_utils.MLX_EXTENSION)
  outfile_mlx = os_utils.mk_file_name_ext(infile_name, os_utils.MLX_EXTENSION)
  outfile_mlx_elaborated = os_utils.mk_file_name_derivative(parsed_infile_mlx, os_utils.ELABORATED)

   # convert dex to core dex by using the parser
  return = parser.main(infile_name, False, True, True)

  # convert core dex to mlx
  dex2mlx.main(parsed_infile, latex_preamble_file)
  print 'Executing command:', command
  os.system(command)


   # elaborate mlx
  mlx.elaborate.main(parsed_infile_mlx, outfile_mlx_elaborated)
  print 'Elaborating', parsed_infile_mlx

  # rename and copy to Desktop
  os_utils.mv_file_to(outfile_mlx_elaborated, outfile_mlx)  
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
