#!/usr/bin/python
import re
import sys
import os

import pervasives.os as pos
import pervasives.syntax as syntax 

LOADER = '../dil/loader.py'

def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: dex2latex inputfile'
    sys.exit()

  infile_name = sys.argv[1]
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  elaborated_file_dex = infile_name_first + pos.ELABORATED + pos.DEX_EXTENSION
  elaborated_file_dil = infile_name_first + pos.DIL_EXTENSION

  # dex: elobarate
  command = 'python elaborate.py ' + infile_name
  print 'Executing command:', command
  os.system(command)

  # convert dex (elaborated) to dil
  command = 'python dex2dil.py ' + elaborated_file_dex
  print 'Executing command:', command
  os.system(command)

#  # load file into the dabasate
#  command = 'python ' + LOADER  + ' ' + elaborated_file_dil
#  print 'Executing command:', command
#  os.system(command)

#  print 'Loaded into the database:'

if __name__ == "__main__":
    main(sys.argv)
