#!/usr/bin/env python
import os
import re
import sys


TEX_EXTENSION = 'tex'

SQUARE_CLOSE = r']'
SQUARE_OPEN = r'['

BRACE_CLOSE = r'}'
BRACE_OPEN = r'{'

NEWLINE = '\n'
PERIOD = '.'


FINISH_LINE = r'[^\n]*$\n'
PATTERN_OPT_ARG = re.compile(r'[\[.*\]]?')

PATTERN_BEGIN_CHAPTER = re.compile(r'[\s]*\\begin\{chapter\}')
PATTERN_END_CHAPTER = re.compile(r'[\s]*\\end\{chapter\}')


BEGIN_CHAPTER = r'\chapter'

def opt_to_arg(arg):
  if arg.startswith(SQUARE_OPEN) and arg.endswith(SQUARE_CLOSE):
    arg = BRACE_OPEN + arg[1:-1] + BRACE_CLOSE
 
  return arg


def texify_begin(pattern_begin, command_begin, line):
  m = pattern_begin.match(line)
  if m: 
    print '* matched begin chapter'
    rest = line[m.end():]
    rest = opt_to_arg(rest)
    return command_begin + rest + NEWLINE
  
  return None

def texify_end(pattern_end, line):
  m = pattern_end.match(line)
  if m: 
    print '* matched end'
    rest = line[m.end():]
    return NEWLINE
  return None

def texify_line(line):
  line = line.strip()
#  print line
 
  # Begin chapter
  r = texify_begin(PATTERN_BEGIN_CHAPTER, BEGIN_CHAPTER, line)
  if r is not None:
    return r
  
  # End chapter
  r = texify_end(PATTERN_END_CHAPTER, line)
  if r is not None:
    return r

  return line + NEWLINE


def texify(argv):
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: texify.py inputfile'
    sys.exit()

  infile_name = sys.argv[1]
  # drop path stuff
  (path, infile_name_) = os.path.split(infile_name) 
  print "infile_name:", infile_name
  (infile_name_first, infile_ext) = infile_name_.split(PERIOD) 
  outfile_name = infile_name_first + PERIOD  + TEX_EXTENSION

  infile = open(infile_name, 'r')
  outfile = open(outfile_name, 'w')

  lines = infile.readlines ()
  for line in lines:   
    line = texify_line(line)
    outfile.write(line)

  outfile.close()
  print "** texify: Done, latex is in in file:", outfile_name


if __name__ == "__main__":
    texify(sys.argv)
