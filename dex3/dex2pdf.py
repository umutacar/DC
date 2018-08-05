#!/usr/bin/python
import re
import sys
import os

import pervasives.os
import pervasives.syntax as syntax 


def main(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: dex2latex inputfile'
    sys.exit()

  infile_name = sys.argv[1]
  (infile_name_first, infile_ext) = infile_name.split (syntax.PERIOD) 
  elaborated_file_dex = infile_name_first + '_elaborated' + pervasives.os.DEX_EXTENSION
  elaborated_file_tex = infile_name_first + '_elaborated' + pervasives.os.LATEX_EXTENSION
  elaborated_pdf_file = infile_name_first + '_elaborated' + pervasives.os.PDF_EXTENSION
  output_pdf_file = infile_name_first + pervasives.os.PDF_EXTENSION


  command = 'python elaborate.py ' + infile_name
  print 'Executing command:', command
  os.system(command)
  command = 'python dex2tex.py ' + elaborated_file_dex
  print 'Executing command:', command
  os.system(command)
  command = 'pdflatex ' + elaborated_file_tex
  print 'Executing command:', command
  os.system(command) 
  ## Run twice to get the labels right
  print 'Executing command:', command
  os.system(command) 

  
  ## Copy to the original's filename
  command = 'cp ' + elaborated_pdf_file + ' ' + output_pdf_file
  print 'Executing command:', command
  os.system(command) 

  ## Copy  the intermediate  pdf file
  command = 'rm ' + elaborated_pdf_file
  print 'Executing command:', command
  os.system(command) 

  print 'PDF is in file:', output_pdf_file

if __name__ == "__main__":
    main(sys.argv)
