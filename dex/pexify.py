## pexify.py
## Generates latex suitable for pandoc compilation

import os
import sys
import pervasives.syntax as syntax
import pervasives.os_utils as os_utils

import dex2pex
import parser

def main(infile_name):
  # get current working directory
  root_dir = os.getcwd()

  # get the file and its path
  (path, infile_name_file) = os.path.split(infile_name) 

  # create the various file names
  (infile_name_first, infile_ext) = infile_name_file.split (syntax.PERIOD) 
  core_infile = os_utils.mk_file_name_derivative(infile_name, os_utils.CORE)
  core_infile_tex = os_utils.mk_file_name_ext(core_infile, os_utils.TEX_EXTENSION)

  outfile_first = os_utils.mk_file_name_derivative(infile_name, os_utils.PANDOC)
  outfile_tex = os_utils.mk_file_name_ext(outfile_first, os_utils.TEX_EXTENSION)

   # convert dex to core dex by using the parser
  print 'Translating', infile_name, 'to core language.'
  parser.main(infile_name, True, True)
  print 'Done.'

  # convert core dex to LaTeX
  print 'Now, Translating to pandoc-friendly LaTeX.'
  dex2pex.main(core_infile)
  print 'Done.'

  # rename and copy to Desktop
  os_utils.mv_file_to(core_infile_tex, outfile_tex)  

  # mv intermediate file to tmp
  os_utils.mv_file_to(core_infile, '/tmp')  


  print 'pexify completed.  Output is in', outfile_tex

  # cd to starting directory
  os.chdir(root_dir)

if __name__ == "__main__":
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: pexify inputfile'
    sys.exit()

  infile_name = sys.argv[1]
  main(infile_name)
