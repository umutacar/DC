#!/usr/bin/python
import re
import sys
import os

import pervasives.os as pos
import pervasives.syntax as syntax 

PYTHON = 'python'


def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: parser inputfile'
    sys.exit()

  # get current working directory
  root_dir = os.getcwd()

  # get the directory to clean
  dir_name = sys.argv[1]

  wild_card = '*'
  file_parsed = wild_card + pos.PARSED + pos.DEX_EXTENSION
  file_ed_0 = wild_card + pos.ELABORATED_0 + pos.DEX_EXTENSION
  file_ed_1 = wild_card + pos.ELABORATED_1 + pos.DEX_EXTENSION
  file_ed = wild_card + pos.ELABORATED + pos.DEX_EXTENSION
  file_dil = wild_card  + pos.DIL_EXTENSION
  file_loaded_dil = wild_card  + pos.LOADED + pos.DIL_EXTENSION

  for x in [file_parsed, file_ed_0, file_ed_1, file_ed, file_dil, file_loaded_dil]:
    command = 'rm'+ syntax.SPACE + dir_name + '/' + x 
    print 'Executing command:', command
    os.system(command)

if __name__ == "__main__":
    main(sys.argv)
