#!/usr/bin/env python
import os
import re
import sys


BACKSLASH = '\\'
## GLOBALS
TEX_EXTENSION = 'tex'

SQUARE_CLOSE = r']'
SQUARE_OPEN = r'['

BRACE_CLOSE = r'}'
BRACE_OPEN = r'{'

NEWLINE = '\n'
PERIOD = '.'




######################################################################
## BEGIN: DEX and TEX syntax

CHAPTER = r'chapter'
CHECKPOINT = r'checkpoint'
CHOICE = r'choice'
GROUP = r'group'

PROBLEM_FR = r'problemfr'
PROBLEM_MA = r'problemma'
PROBLEM_MC = r'problemmc'
PROBLEM_GROUP = r'problemgroup'
PROBLEM_SET = r'problemset'
SELECT = r'select'
SECTION = r'section'
SUBSECTION = r'subsection'
SUBSUBSECTION = r'subsubsection'


# Atoms
ALGO = r'algo'
ALGORITHM = r'algorithm'
ANSWER = r'answer'
CODE = r'code'
COROLLARY = r'corollary'
COST_SPEC = r'costspec'
DATASTR = r'datastr'
DATATYPE = r'datatype'
DEFINITION = r'definition'
EXAMPLE = r'example'
EXERCISE = r'exercise'
HINT = r'hint'
IMPORTANT = r'important'
LEMMA = r'lemma'
NOTE = r'note'
GRAM = r'gram'
PARAGRAPH = r'paragraph'
PARAGRAPH_HTML = r'html'
PROBLEM = r'problem'
PROOF = r'proof'
PROPOSITION = r'proposition'
REMARK = r'remark'
SOLUTION = r'solution'
SYNTAX = r'syntax'
TEACH_ASK = r'teachask'
TEACH_NOTE = r'teachnote'
THEOREM = r'theorem'

MAP_SECTION = {\
  CHAPTER: BACKSLASH + CHAPTER, \
  GRAM: BACKSLASH + PARAGRAPH, \
  GROUP: BACKSLASH + PARAGRAPH, \
  SECTION: BACKSLASH + SECTION, \
  SUBSECTION: BACKSLASH + SUBSECTION, \
  SUBSUBSECTION: BACKSLASH + SUBSUBSECTION, \
  }

## END: DEX and TEX syntax
######################################################################


SECTION_NAME = 'section_name'

FINISH_LINE = r'[^\n]*$\n'
PATTERN_OPT_ARG = re.compile(r'[\[.*\]]?')

## chapters
PATTERN_BEGIN_CHAPTER = re.compile(r'[\s]*\\begin\{chapter\}')
PATTERN_END_CHAPTER = re.compile(r'[\s]*\\end\{chapter\}')

BEGIN_CHAPTER = r'\chapter'

SECTION_KEYWORDS = \
  '(' + r'?P<' + SECTION_NAME + '>' + \
  CHAPTER + '|' + \
  SECTION + '|' + \
  SUBSECTION + '|' + \
  SUBSUBSECTION + '|' + \
  GROUP + '|' + \
  GRAM  + '|' + \
  ALGO + '|' + \
  ALGORITHM + '|' + \
  CODE + '|' + \
  COROLLARY + '|' + \
  COST_SPEC + '|' + \
  DATASTR + '|' + \
  DATATYPE + '|' + \
  DEFINITION + '|' + \
  EXAMPLE + '|' + \
  EXERCISE + '|' + \
  HINT + '|' + \
  IMPORTANT + '|' + \
  LEMMA + '|' + \
  NOTE + '|' + \
  PARAGRAPH + '|' + \
  PARAGRAPH_HTML + '|' + \
  PROBLEM + '|' + \
  PROOF + '|' + \
  PROPOSITION + '|' + \
  REMARK + '|' + \
  SOLUTION + '|' + \
  SYNTAX + '|' + \
  TEACH_ASK + '|' + \
  TEACH_NOTE + '|' + \
  THEOREM +  \
  ')'  

  
PATTERN_BEGIN = re.compile(r'[\s]*\\begin\{' + SECTION_KEYWORDS + '\}')
PATTERN_END = re.compile(r'[\s]*\\end\{' + SECTION_KEYWORDS + '\}')

def opt_to_arg(arg):
  if arg.startswith(SQUARE_OPEN) and arg.endswith(SQUARE_CLOSE):
    arg = BRACE_OPEN + arg[1:-1] + BRACE_CLOSE
  elif re.match('\s*', arg):
    arg = '{}'
  return arg

def texify_begin(line):
  m = PATTERN_BEGIN.match(line)
  if m: 
    matched_section = m.group(SECTION_NAME)
    print '* matched begin:', matched_section
    rest = line[m.end():]
    rest = opt_to_arg(rest)
 

    latex_section = None
    try:
      latex_section = MAP_SECTION[matched_section]
    except KeyError:
      latex_section = BACKSLASH + PARAGRAPH
  
    return latex_section + rest + NEWLINE
  
  return None

def texify_end(line):
  m = PATTERN_END.match(line)
  if m: 
    matched_section = m.group(SECTION_NAME)
    print '* matched end', matched_section
    return NEWLINE
  return None

def texify_line(line):
  line = line.strip()
#  print line
 
  # Begin chapter
  r = texify_begin (line)
#  r = texify_begin(PATTERN_BEGIN_CHAPTER, BEGIN_CHAPTER, line)
  if r is not None:
    return r
  
  # End chapter
  r = texify_end(line)
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
    print 'SECTION_KEYWORDS', SECTION_KEYWORDS
    texify(sys.argv)
