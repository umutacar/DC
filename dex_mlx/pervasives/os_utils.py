import sys    
import os
from pervasives.syntax import *

# Common file extensions
DEX_EXTENSION = '.dex'
DIL_EXTENSION = '.dil'
HTML_EXTENSION = '.html'
LATEX_EXTENSION = '.tex'
MLX_EXTENSION = '.mlx'
PDF_EXTENSION = '.pdf'
XML_EXTENSION = '.xml'


# Common file prefixes
INTRO_EXTENSION = ':intro'
BODY_EXTENSION = ':body'
EXPLAIN_EXTENSION = ':explain'
HINT_EXTENSION = ':hint'
PROMPT_EXTENSION = ':prompt'
SOLUTION_EXTENSION = ':solution'
TITLE_EXTENSION = ':title'
INFO_EXTENSION = ":info"

# File name creation
BEAMED = '_beamed'
ELABORATED_0 = '_ed_0'
ELABORATED_1 = '_ed_1'
ELABORATED = '_ed'
LOADED = '_loaded'
PARSED = '_parsed'

TMP_DIR = '/tmp/'
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
  
## END: Run commands
######################################################################
