#!/usr/bin/python
import re
import sys
import os

import pervasives.os as pos
import pervasives.syntax as syntax

def publish(infile_name, app_root=False, rm_intermediate=False):
  # remove dependency on being in the dex folder
  pythonpath = os.environ['PYTHONPATH']
  paths = pythonpath.split(":")
  prefix = ""
  for path in paths:
    target = "diderot/diderot"
    if target in path:
      prefix = path
      if path[-1] != '/':
        prefix = prefix + '/'
      break
  if len(prefix) == 0:
    print 'Please set your python path to the diderot/diderot folder'
    sys.exit()

  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  elaborated_file_int = infile_name_first + pos.ELABORATED_1 + pos.DEX_EXTENSION
  elaborated_file_dex = infile_name_first + pos.ELABORATED + pos.DEX_EXTENSION
  elaborated_file_dil = infile_name_first + pos.DIL_EXTENSION
  elaborated_file_loaded = infile_name_first + pos.LOADED + pos.DIL_EXTENSION

  if app_root:
    infile_name = prefix + infile_name
    elaborated_file_dex = prefix + elaborated_file_dex
    elaborated_file_dil = prefix + elaborated_file_dil

  # dex: elobarate
  command = 'python ' + prefix + 'dex/elaborate.py ' + infile_name
  print 'Executing command:', command
  os.system(command)

  # convert dex (elaborated) to dil
  command = 'python ' + prefix + 'dex/dex2dil.py ' + elaborated_file_dex
  print 'Executing command:', command
  os.system(command)

  # load file into the dabasate
  command = 'python ' + prefix + 'dil/loader.py ' + elaborated_file_dil
  print 'Executing command:', command
  os.system(command)

  print 'Loaded into the database:'

  if rm_intermediate:
    print 'Cleaning up'
    def rm(file):
      print 'Removing ' + file
      os.system('rm ' + file)
    rm(elaborated_file_dex)
    rm(elaborated_file_dil)
    rm(elaborated_file_int)
    rm(elaborated_file_loaded)


if __name__ == "__main__":
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2:
    print 'Usage: dex2latex inputfile'
    sys.exit()
  publish(sys.argv[1])
