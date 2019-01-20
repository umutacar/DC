import sys    
import os
from pervasives.syntax import *

# Common file extensions
DEX_EXTENSION = 'dex'
DIL_EXTENSION = 'dil'
HTML_EXTENSION = 'html'
LATEX_EXTENSION = 'tex'
TEX_EXTENSION = LATEX_EXTENSION
MLX_EXTENSION = 'mlx'
PDF_EXTENSION = 'pdf'
TEXT_EXTENSION = 'txt'
XML_EXTENSION = 'xml'

# Common file derivatives
BEAMED = '_beamed'
CORE = 'core'
DECORATED = 'decd'
ELABORATED = 'eld'
# File name creation
ELABORATED_0 = 'ed_0'
ELABORATED_1 = 'ed_1'
LOADED = 'loaded'
PANDOC = 'pandoc'
PARSED = 'parsed'
STRICT = 'strict'

TMP_DIR = '/tmp/'
DESKTOP_DIR = '~/Desktop/'


# Common file prefixes
INTRO_EXTENSION = ':intro'
BODY_EXTENSION = ':body'
EXPLAIN_EXTENSION = ':explain'
HINT_EXTENSION = ':hint'
PROMPT_EXTENSION = ':prompt'
SOLUTION_EXTENSION = ':solution'
TITLE_EXTENSION = ':title'
INFO_EXTENSION = ":info"

######################################################################
## DEBUG HANDLING

backup_stdout = sys.stdout

# New destination for stdout
class DevNull(object):
    def write(self, arg):
        pass

def set_stdout(debug_flag):
  if not debug_flag:
    sys.stdout = DevNull()
  else:
    sys.stdout = backup_stdout

def reset_stdout():
  sys.stdout = backup_stdout


######################################################################
## BEGIN: Run commands

def run_command(command):
  print 'Executing command:', command
  result = os.system(command)
  if result:
    print 'Command did not complete successfully.\n Offending command was:\n', command
    exit()

def run_command_no_check(command):
  print 'Executing command:', command
  result = os.system(command)

def rm_file (filename):
  command = 'rm ./filename'
  run_command_no_check(command)

def mv_file_to_tmp (filename):
  command = 'mv ' + PERIOD + SLASH + filename + SPACE + TMP_DIR
  run_command_no_check(command)  

def cp_file_to_desktop (filename):
  command = 'mv ' + PERIOD + SLASH + filename + SPACE + DESKTOP_DIR
  run_command_no_check(command)

def cp_file_to (filename1, filename2):
  command = 'cp ' + SPACE + filename1 + SPACE + filename2
  run_command_no_check(command)
  
def mv_file_to (filename1, filename2):
  command = 'mv ' + SPACE + filename1 + SPACE + filename2
  run_command_no_check(command)    

def mv_file_subdir (filename, dir_name):
  filename2 = PERIOD + SLASH + dir_name + SLASH + filename 
  mv_file_to (filename, filename2)
  


  ## END: Run commands
######################################################################

# mk a new filename out the infilename
# name.ext turns into name_derivative.extension

def mk_file_name (infile_name, derivative, extension):
  (infile_name_first, infile_ext) = infile_name.split (PERIOD) 
  outfile_name = infile_name_first + UNDERSCORE + derivative + PERIOD + extension
  return outfile_name

def mk_file_name_derivative (infile_name, derivative):
  (infile_name_first, infile_ext) = infile_name.split (PERIOD) 
  outfile_name = infile_name_first + UNDERSCORE + derivative + PERIOD + infile_ext
  return outfile_name

# page count is i
def mk_file_name_page (infile_name, i):
  derivative = str(i)
  r = mk_file_name_derivative (infile_name, derivative)
  return r 

def mk_file_name_ext (infile_name, extension):
  (infile_name_first, infile_ext) = infile_name.split (PERIOD) 
  outfile_name = infile_name_first + PERIOD + extension
  return outfile_name

def deep_cp_file_to (infile_name, outfile_name):
  infile = open(infile_name, 'rb')
  outfile = open(outfile_name, 'w')

  data = infile.read ()
  outfile.write (data)
