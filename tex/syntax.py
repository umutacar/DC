######################################################################
## dex/syntax.py
######################################################################

from pervasives.syntax import *

# Commands
COM_CHAPTER = r'\chapter'
COM_LABEL = r'\label'
COM_PARAGRAPH = r'\paragraph'
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


# Atoms, *_ are the long versions
ALGO = r'algo'
ALGO_ = r'algo'
ALGORITHM = r'algorithm'
ALGORITHM_ = r'algorithm'
ANSWER = r'answer'
ANSWER_ = r'answer'
CODE = r'code'
CODE_ = r'code'
COROLLARY = r'corollary'
COROLLARY_ = r'corollary'
COST_SPEC = r'costspec'
COST_SPEC_ = r'Cost Specification'
DATASTR = r'datastr'
DATASTR_ = r'Data Structure'
DATATYPE = r'datatype'
DATATYPE_ = r'Datatype'
DEFINITION = r'definition'
DEFINITION_ = r'Definition'
EXAMPLE = r'example'
EXAMPLE_ = r'Example'
EXERCISE = r'exercise'
EXERCISE_ = r'Exercise'
HINT = r'hint'
HINT_ = r'Hint'
IMPORTANT = r'important'
IMPORTANT_ = r'Important'
LEMMA = r'lemma'
LEMMA_ = r'Lemma'
NOTE = r'note'
NOTE_ = r'Note'
PARAGRAPH = r'gram'
PARAGRAPH_ = r''
PARAGRAPH_HTML = r'html'
PARAGRAPH_HTML_ = r'html'
PROBLEM = r'problem'
PROBLEM_ = r'Problem'
PROOF = r'proof'
PROOF_ = r'Proof'
PROPOSITION = r'proposition'
PROPOSITION_ = r'Proposition'
REMARK = r'remark'
REMARK_ = r'Remark'
SOLUTION = r'solution'
SOLUTION_ = r'Solution'
SKIP = r'skip'
SKIP_ = r'Skip'
SYNTAX = r'syntax'
SYNTAX_ = r'Syntax'
TEACH_ASK = r'teachask'
TEACH_ASK_ = r'Teaching Question'
TEACH_NOTE = r'teachnote'
TEACH_NOTE_ = r'Teaching Note'
THEOREM = r'theorem'
THEOREM_ = r'Theorem'
## TODO: delete this



######################################################################
## BEGIN: Headings
######################################################################

def mk_label(label):
  result = COM_LABEL + mk_str_arg(label)
  return result

def mk_heading_chapter(title):
  if valid_title (title):
    title = title
  else:
    title = ''

  result = COM_CHAPTER + mk_str_arg(title)
  return result

def mk_heading_section(title):
  if valid_title (title):
    title = title
  else:
    title = ''
  result = COM_SECTION + mk_str_arg(title)
  return result

def mk_heading_subsection(title):
  if valid_title (title):
    title = title
  else:
    title = ''
  result = COM_SUBSECTION + mk_str_arg(title)
  return result

def mk_heading_subsubsection(title):
  if valid_title (title):
    title = title
  else:
    title = ''
  result = COM_SUBSUBSECTION + mk_str_arg(title)
  return result

def mk_heading_paragraph(title):
  if valid_title (title):
    title = title
  else:
    title = ''
  result = COM_PARAGRAPH + mk_str_arg(title)
  return result 

######################################################################
## END: Headings
######################################################################

######################################################################
## BEGIN: Blocks
######################################################################

def mk_block_atom (name, title, label, contents):
  if valid_title(title):
    if name == '':
      title = title 
    else:
      title = name.capitalize() + COLON + SPACE + title 

  else:
    title = name.capitalize()

  result = \
    mk_heading_paragraph(title) + NEWLINE + \
    contents.lstrip()
  return result

def mk_block_chapter (title, label, intro, contents):
  result = \
    mk_heading_chapter(title) + NEWLINE + NEWLINE + \
    intro + NEWLINE + \
    contents.lstrip() 

  return result + NEWLINE

def mk_block_section (title, label, contents):
  result = \
    mk_heading_section(title) + NEWLINE + NEWLINE + \
    contents.strip() 
  return NEWLINE + NEWLINE + result + NEWLINE

def mk_block_subsection (title, label, contents):
  result = \
    mk_heading_subsection(title) + NEWLINE + NEWLINE + \
    contents.lstrip()
 
  return NEWLINE + NEWLINE + result + NEWLINE

def mk_block_subsubsection (title, label, contents):
  result = \
    mk_heading_subsubsection(title) + NEWLINE + NEWLINE + \
    contents.lstrip() 
  return NEWLINE + NEWLINE + result + NEWLINE

def mk_block_group (contents):
  return NEWLINE + contents.lstrip() + NEWLINE

######################################################################
## END: Blocks
######################################################################
