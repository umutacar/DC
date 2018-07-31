######################################################################
## dex/syntax.py
######################################################################

from pervasives.syntax import *

# Commands
COM_CHAPTER = r'\chapter'
COM_LABEL = r'\label'
COM_SECTION = r'\section'
COM_SUBSECTION = r'\subsection'
COM_SUBSUBSECTION = r'\subsubsection'

# block strings
ABOUT = r'about'
ASSIGNMENT = r'assignment'
ASSTPROBLEM = r'asstproblem'
BOOK = r'book'
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
PARAGRAPH = r'gram'
PARAGRAPH_HTML = r'html'
PROBLEM = r'problem'
PROOF = r'proof'
PROPOSITION = r'proposition'
REMARK = r'remark'
SOLUTION = r'solution'
SKIP = r'skip'
SYNTAX = r'syntax'
TEACH_ASK = r'teachask'
TEACH_NOTE = r'teachnote'
THEOREM = r'theorem'
## TODO: delete this



######################################################################
## BEGIN: Headings
######################################################################

def mk_label(label):
  result = COM_LABEL + mk_str_arg(label)
  return result

def mk_heading_chapter(title):
  result = COM_CHAPTER + mk_str_arg(title)
  return result

def mk_heading_section(title):
  result = COM_SECTION + mk_str_arg(title)
  return result

def mk_heading_subsection(title):
  result = COM_SUBSECTION + mk_str_arg(title)
  return result

def mk_heading_subsubsection(title):
  result = COM_SUBSUBSECTION + mk_str_arg(title)
  return result

######################################################################
## END: Headings
######################################################################

######################################################################
## BEGIN: Blocks
######################################################################

def mk_block_chapter (title, label, intro, contents):
  result = \
    mk_heading_chapter(title) + \
    intro + NEWLINE + \
    contents + NEWLINE 
  return result

def mk_block_group (contents):
  return contents

######################################################################
## END: Blocks
######################################################################
