######################################################################
## dex/elaborate.py
######################################################################

import os
import re
import sys
from functools import partial as curry
import pyparsing as pp 

import pervasives.os 
from pervasives.parser import *
from pervasives.syntax import *

import blocks as blocks
import parser as dex_parser
import syntax as dex
import uniques as uniques

######################################################################
## BEGIN Algorithm
##
# The algorithm is a two-pass algorithm.
#
# In the zeroth pass, we eliminate syntactic sugar by simply
# parsing the input file and writing it out in the canonical form.
#
# In the first pass, we parse the dex file and collect various info.
# In the first pass, we assign a unique to each block.
#
# In the second pass, we parse the dex file again and use the info
# collected to make some modifications.  The second pass outputs a dex
# file that should be parseable by the parser.py but it has additional
# constraints.  For example, each block now has a label.
#
## END Algorithm
######################################################################





######################################################################
## BEGIN Globals

# Label map, mapping user defined labels 
# to auto-generated 
LABEL_MAP = {}

GROUP_FLAG = False

## END Globals
######################################################################


######################################################################
## BEGIN: Block Nesting  
##
## Closely integrated with uniques. 
##
## Mostly but not completely straightforward.
## Some care is needed in ensuring that we create
## groups for atoms that don't have their own groups.
##

def process_block_begin (this_block, toks):
  global GROUP_FLAG

  if this_block == Block.BOOK:
     pass
#    print ('process_block_begin: this block is book')
  elif this_block == Block.CHAPTER:
#    print ('process_block_begin: this  block is chapter')
    uniques.new_chapter ()
  elif this_block == Block.CHECKPOINT:
#    print ('process_block_begin: this  block is section')
    uniques.new_checkpoint ()
  elif this_block == Block.SECTION:
#    print ('process_block_begin: this  block is section')
    uniques.new_section ()
  elif this_block == Block.UNIT:
#    print ('process_block_begin: this  block is unit')
    uniques.new_unit()
  elif this_block == Block.GROUP:
    uniques.new_group()
    GROUP_FLAG = True
  elif this_block == Block.ASSIGNMENT:
    uniques.new_assignment ()
  elif this_block == Block.ASSTPROBLEM:
    uniques.new_asstproblem ()
    uniques.new_group()
  elif this_block == Block.ATOM or this_block == Block.ALGO:
#    print ('process_block_begin: this  block is atom')

    # Create a group if not already in a group
    if not GROUP_FLAG:
      uniques.new_group()
    # now create atom
    uniques.new_atom()
  elif this_block == Block.QUESTION:
#    print ('process_block_begin: this block is a question')
    uniques.new_question()
  elif this_block == Block.ANSWER:
#    print ('process_block_begin: this block is answer')
    uniques.new_answer()
  elif this_block == Block.CHOICE:
#    print ('process_block_begin: this block is choice')
    uniques.new_choice()
  else :
    print 'this block is UNKNOWN. Fatal ERROR:', this_block

  return toks

def process_block_end (this_block, toks):
  global GROUP_FLAG

  if this_block == Block.BOOK:
#    print ('process_block_end: this  block is book')
    pass
  elif this_block == Block.CHAPTER:
#    print ('process_block_end: this  block is chapter')
    pass
  elif this_block == Block.CHECKPOINT:
#    print ('process_block_begin: this  block is section')
    pass
  elif this_block == Block.SECTION:
#    print ('process_block_end: this  block is section')
    pass
  elif this_block == Block.UNIT:
#    print ('process_block_end: this  block is unit')
    pass
  elif this_block == Block.GROUP:
    GROUP_FLAG = False
  elif this_block == Block.ATOM or this_block == Block.ALGO:
    # treat algo as atom
#    print ('process_block_end: this  block is atom')
    pass
  elif this_block == Block.QUESTION:
#    print ('process_block_begin: this block is problem')
    pass
  elif this_block == Block.ANSWER:
#    print ('process_block_begin: this block is answer')
    pass
  elif this_block == Block.CHOICE:
#    print ('process_block_begin: this block is choice')
    pass
  elif this_block == Block.ASSIGNMENT:
    uniques.exit_assignment()
  else :
#    print 'process_block_end: this  block is UNKNOWN. Fatal ERROR:', this_block
    pass
  return toks

## END: Block Nesting  
######################################################################

######################################################################
## BEGIN: block processors for the zeroth pass
#
## Each process_block function takes a block, processes it
## and returns a string for the processed block.

def process_course_zero (toks): 
  print "matched course:"
  block = blocks.Course(toks)
  result = block.to_string ()
  return result  

def process_book_zero (toks): 
  print "matched book:"
  block = blocks.Book(toks)
  result = block.to_string ()
  return result  

def process_chapter_zero (toks): 
  print "matched chapter:"
  block = blocks.Chapter(toks)
  result = block.to_string ()
  return result  

def process_section_zero (toks): 
  print "matched section:"
  block = blocks.Section(toks)
  result = block.to_string ()
  return result  

def process_unit_zero (toks): 
  print "matched unit:"
  block = blocks.Unit(toks)
  result = block.to_string ()
  return result  

def process_group_zero (toks): 
  print "matched group:"
  block = blocks.Group(toks)
  result = block.to_string ()
  return result  

def process_checkpoint_zero (toks): 
  print "matched checkpoint:"
  block = blocks.Checkpoint(toks)
  result = block.to_string ()
  return result  

def process_assignment_zero (toks):
  print "matched assignment:"
  block = blocks.Assignment(toks)
  result = block.to_string ()
  return result  

def process_asstproblem_zero (toks):
  print "matched problem:"
  block = blocks.AsstProblem(toks)
  result = block.to_string ()
  return result  

def process_algo_zero (toks): 
  block = blocks.Algo(toks)
  result = block.to_string ()
  return result  

def process_atom_zero (name, toks): 
  print "matched atom:"
  block = blocks.Atom(name, toks)
  result = block.to_string ()
  return result  

def process_question_fr_zero (toks): 
  print "matched question_fr."
  block = blocks.QuestionFR(toks)
  result = block.to_string ()
  return result  

def process_question_ma_zero (toks): 
  print "matched question_ma."
  block = blocks.QuestionMA(toks)
  result = block.to_string ()
  return result  

def process_question_mc_zero (toks): 
  print "matched question_mc."
  block = blocks.QuestionMC(toks)
  result = block.to_string ()
  return result  

def process_answer_zero (toks): 
  print "matched answer:"
  block = blocks.Answer(toks)
  result = block.to_string ()
  return result  

def process_choice_zero (toks): 
  print "matched choice:"
  block = blocks.Choice(toks)
  result = block.to_string ()
  return result  

def process_select_zero (toks): 
  print "matched select:"
  block = blocks.Select(toks)
  result = block.to_string ()
  return result  

## END: block processing for the zeroth pass
######################################################################


######################################################################
## BEGIN: block processors for the first pass
#
## Each process_block function takes a block, processes it
## and returns a string for the processed block.

def update_labels_one(block):
  user_label = block.label
  auto_label = block.mk_label()

  if valid_label(user_label): 
    print 'User label found:', user_label
    print '... and mapping it to label:', auto_label
  else:
    print 'user label is invalid, user_label:', user_label
    print 'user label is invalid, auto_label:', auto_label

  block.label = auto_label
  LABEL_MAP[user_label] = auto_label
  
def process_block_one(block):
  ## Update labels
  update_labels_one(block)  
  ## Update no to be its unique
  block.no = block.mk_no()
  ## Update unique to be its unique
  block.unique = block.mk_unique()
  # return string
  result = block.to_string ()
  return result

def process_course_one (toks): 
  print "matched course:"
  block = blocks.Course(toks)

  # Set unique to be the same as the course number
  block.unique = block.number

  # Set course
  uniques.provide_course(block.unique)

  # Set book counter
  uniques.provide_book(block.provides_book)

  # Set chapter counter 
  uniques.provide_chapter(block.provides_chapter)

  # Set section counter 
  uniques.provide_section(block.provides_section)

  # Set unit counter 
  uniques.provide_unit(block.provides_unit)

  # Course labels are set by initialization.
  # No need for label management.
  uniques.provide_assignment(block.provides_assignment)

  # Return 
  result = block.to_string ()
  return result  

def process_book_one (toks): 
  print "matched book:"
  block = blocks.Book(toks)
  return process_block_one(block)

def process_chapter_one (toks): 
  print "matched chapter:"
  block = blocks.Chapter(toks)
  return process_block_one(block)

def process_section_one (toks): 
  print "matched section:"
  block = blocks.Section(toks)
  return process_block_one(block)

def process_unit_one (toks): 
  print "matched unit:"
  block = blocks.Unit(toks)
  return process_block_one(block)

def process_group_one (toks): 
  print "matched group:"
  block = blocks.Group(toks)
  return process_block_one(block)

def process_checkpoint_one (toks): 
  print "matched checkpoint:"
  block = blocks.Checkpoint(toks)
  return process_block_one(block)

def process_assignment_one (toks):
  print "matched assignment:"
  block = blocks.Assignment(toks)
  return process_block_one(block)

def process_asstproblem_one (toks):
  print "matched problem:"
  block = blocks.AsstProblem(toks)
  return process_block_one(block)

def process_algo_one (toks): 
  print "matched algo:"
  block = blocks.Algo(toks)
  print 'algo initial:', block.to_string()
  block.elaborate_contents()
  print 'algo:', block.to_string()
  algo = process_block_one(block)
  print 'algo (post processing):', algo
  global GROUP_FLAG
  if GROUP_FLAG:
    return algo
  else:
    # Make a group, nest the atom inside and return that
    group = blocks.Group.singleton_group(algo)
    # Process group
    result = process_block_one(group)
    return result

def process_atom_one (name, toks): 
  print "matched atom:"
  block = blocks.Atom(name, toks)
  atom = process_block_one(block)
  global GROUP_FLAG
  if GROUP_FLAG:
    return atom
  else:
    # Make a group, nest the atom inside and return that
    group = blocks.Group.singleton_group(atom)
    # Process group
    result = process_block_one(group)
    return result

def process_question_fr_one (toks): 
  print "matched question_fr."
  block = blocks.QuestionFR(toks)
  return process_block_one(block)

def process_question_ma_one (toks): 
  print "matched question_ma."
  block = blocks.QuestionMA(toks)
  return process_block_one(block)

def process_question_mc_one (toks): 
  print "matched question_mc."
  block = blocks.QuestionMC(toks)
  return process_block_one(block)

def process_answer_one (toks): 
  print "matched answer:"
  block = blocks.Answer(toks)
  return process_block_one(block)

def process_choice_one (toks): 
  print "matched choice:"
  block = blocks.Choice(toks)
  return process_block_one(block)

def process_select_one (toks): 
  print "matched select:"
  block = blocks.Select(toks)
  return process_block_one(block)

## END: block processing for the first pass
######################################################################

######################################################################
## BEGIN: second pass
##
## Algorithm: no parsing needed.  Simply replace labels with their new
## versions.  To unsure plain words don't get caught in the process,
## we look for the patterns of the form {user_label} and replace it
## with patters of the form {auto_label}.
## 
## This could cause problems in the future but should suffice for the
## time being.
 

def wrap (x): 
  return  (CURLY_BRACKET_OPEN + x + CURLY_BRACKET_CLOSE)

def process_pass_two (data):
  REPLACE_MAP = {wrap(k): wrap(v) for k, v in LABEL_MAP.items()}
  pattern = re.compile (r'\b' + '|'.join(REPLACE_MAP.keys()) + r'\b')
  result = pattern.sub (lambda x: REPLACE_MAP[x.group()], data)
  return result

## END: second pass
######################################################################


######################################################################
## BEGIN Mainline
## Two pass elaborator

def main(argv):

  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: elaborator inputfile'
    sys.exit()

  infile_name = sys.argv[1]

  # drop path stuff - why
  # (path, infile_name) = os.path.split(infile_name) 
  print "infile_name:", infile_name
  (infile_name_first, infile_ext) = infile_name.split(PERIOD) 




## Pass 0
  outfile_name = infile_name_first + pervasives.os.ELABORATED_0 + PERIOD + infile_ext
  print "outfile_name:", outfile_name

  infile = open(infile_name, 'r')
  data = infile.read ()
  infile.close ()

  labels_optional = True
  nos_optional = True
  uniques_optional = True
  parents_optional = True
  titles_optional = True
  course_label_on = False

  print '** BEGIN Pass 0 ...'
  uniques.reset ()

  ## False: Turns off all prints by sending them to blackhole
  print 'Turning off all prints'
  pervasives.os.set_stdout(False)

  result = dex_parser.parse ( \
             labels_optional, \
             nos_optional, \
             uniques_optional, \
             parents_optional, \
             titles_optional, \
             course_label_on, \
             process_block_begin, \
             process_block_end, \
             process_algo_zero, \
             process_atom_zero, \
             process_answer_zero, \
             process_choice_zero, \
             process_select_zero, \
             process_book_zero, \
             process_chapter_zero, \
             process_course_zero, \
             process_group_zero, \
             process_question_fr_zero, \
             process_question_ma_zero, \
             process_question_mc_zero, \
             process_checkpoint_zero, \
             process_assignment_zero, \
             process_asstproblem_zero, \
             process_section_zero, \
             process_unit_zero, \
             data)

  pervasives.os.reset_stdout()
  print 'Turned printing back on'

  ## Write output of first pass
  # The result consists of a course and a book
  course = result[0]
  book = result[1]

  outfile = open(outfile_name, 'w')
  outfile.write(course + NEWLINE + book + NEWLINE)
  outfile.close()

  print "Pass 0 complete result written into file:", outfile_name
  print '** END Pass 0.'


## Pass 1
  # Course label is on.
  course_label_on = True

  # Read result of pass 0.
  infile = open(outfile_name, 'r')
  data = infile.read ()
  infile.close ()

  print '** BEGIN Pass 1 ...'
  uniques.reset ()

  ## False: Turns off all prints by sending them to blackhole
  print 'Turning off all prints'
  pervasives.os.set_stdout(False)

  result = dex_parser.parse ( \
             labels_optional, \
             nos_optional, \
             uniques_optional, \
             parents_optional, \
             titles_optional, \
             course_label_on, \
             process_block_begin, \
             process_block_end, \
             process_algo_one, \
             process_atom_one, \
             process_answer_one, \
             process_choice_one, \
             process_select_one, \
             process_book_one, \
             process_chapter_one, \
             process_course_one, \
             process_group_one, \
             process_question_fr_one, \
             process_question_ma_one, \
             process_question_mc_one, \
             process_checkpoint_one, \
             process_assignment_one, \
             process_asstproblem_one, \
             process_section_one, \
             process_unit_one, \
             data)

  print 'Printing LABEL_MAP:'
  print  LABEL_MAP

  # Reset print blackhole to normal
  pervasives.os.reset_stdout()
  print 'Turned printing back on'

  ## Write output of first pass
  # The result consists of a course and a book
  course = result[0]
  book = result[1]

  outfile_name = infile_name_first + pervasives.os.ELABORATED_1 + PERIOD + infile_ext
  outfile = open(outfile_name, 'w')
  outfile.write(course + NEWLINE + book + NEWLINE)
  outfile.close()

  print "Pass 1 complete result written into file:", outfile_name
  print '** END Pass 1.'

  # Second pass
  print "** BEGIN Pass 2."  
  # Read result of pass 1.
  infile = open(outfile_name, 'r')
  data = infile.read ()

  ## replace all labels with their corresponding ones.
  result = process_pass_two(data)
  # write result
  outfile_name = infile_name_first + pervasives.os.ELABORATED + PERIOD + infile_ext
  outfile = open(outfile_name, 'w')
  outfile.write(result)
  outfile.close()
  print "Pass 2 complete, result written into file:", outfile_name
  print '** END Pass 2.'

if __name__ == "__main__":
    main(sys.argv)

## END Mainline
######################################################################



