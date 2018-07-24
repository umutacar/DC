######################################################################
## dex/uniques.py
######################################################################

######################################################################
## BEGIN uniques

from pervasives.syntax import *

## INTERFACE [Incomplete]
##
## Unique start counting from 1 but some can be set by the user.
## When set they start counting at the set value.
## Currently, we allow the user to set book, chapter, section, and unit
## counters.
## 
## 
## ALGORITHM
## counters are nested as follows
## course -> book -> chapter -> section -> 
## unit -> group -> atom
##      -> checkpoint -> questions -> choices/answers
##      -> atom
## The handling of course -> book -> chapter -> section -> unit
## is relatively easy.  
## We assume (require) that course and book are set by the user.
##
## The rest gets incremented as new ones are defined.
## When a new one is defined it resets the counter nested inside.
## As an important special case the counters for book, chapcer, section
## and unit can also be set by the user. This supports uploading
## of individual blocks of the corresponding kind.
##
## The counters of unit is a bit more complex.
## This is because there is group and checkpoint and also
## an atom can be inside a unit directly.
##
## We deal with this as follows: the idea is to push atoms and
## questions, which are the terminals to the same level is in the tree.
## To this end, we will count groups and checkpoints together.  We will
## also imagine each atom as wrapped under a singleton group (a group
## containing one atom).  So the picture is like this:
## unit -> element[group] -> atom
##      -> element[checkpoint] -> questions -> answers/choices
## 
## Here I use the term element to refer to the set consisting of 
## groups and checkpoints.
##
## Thus we will assign each element a unique number accross all of the
## unit.  The group and the checkpoint will take this number and the atoms
## and questions within it will be given their number accordingly.
##
## We will make one refinement here: we think of questions simply as
## atoms.  So we will use the same counter for them. (In other words,
## questions are just atoms.)  Similarly, we will use one counter for 
## answers and choices.
##
## One issue is creating "phantom" or singleton groups require knowing
## whether an atom is nested within a group or not.  We can do this by
## setting a group flag that indicates that we are nested inside of a
## group.  We set the flat to false when the group ends.  
## We assume that this is handled by the caller.  Elaboration does this.
##
## Finally, we nested choices inside questions, creating choices for
## each question.
##
##

## These are set by the user
BOOK_UNIQUE = 'thisbook'
COURSE_UNIQUE = 'thiscourse'


PREFIX_ANSWER = 'a'
PREFIX_ASSIGNMENT = 'ASG'
PREFIX_ASSTPROBLEM = 'p'
PREFIX_ATOM = 'AT'
PREFIX_BOOK = ''
PREFIX_CHAPTER = 'CH'
PREFIX_CHOICE = 'c'
PREFIX_COURSE = ''
PREFIX_GROUP = 'GR'
PREFIX_QUESTION = 'Q'
PREFIX_CHECKPOINT = 'CP'
PREFIX_SELECT = 's'
PREFIX_SECTION = 'SEC'
PREFIX_UNIT = 'UN'


## These are auto generated, 
## though chapter could alse be set by the user.
BOOK_COUNTER = 0  # This should never be set except here.
CHAPTER_COUNTER = 0
CHECKPOINT_COUNTER = 0
SECTION_COUNTER = 0
UNIT_COUNTER = 0
ELEMENT_COUNTER = 0
ATOM_COUNTER = 0
ANSWER_COUNTER = 0
CHOICE_COUNTER = 0

ASSIGNMENT_COUNTER = 0
ASSTPROBLEM_COUNTER = 0
IN_ASSIGNMENT = False

CHAPTER_PROVIDED = False
SECTION_PROVIDED = False
UNIT_PROVIDED = False
ASSIGNMENT_PROVIDED = False


# reset all but the course counter
def reset ():
  global BOOK_COUNTER, CHAPTER_COUNTER, CHECKPOINT_COUNTER, SECTION_COUNTER, UNIT_COUNTER, ELEMENT_COUNTER, ATOM_COUNTER, ANSWER_COUNTER, \
         ASSIGNMENT_COUNTER, ASSTPROBLEM_COUNTER, IN_ASSIGNMENT
  global CHAPTER_PROVIDED, SECTION_PROVIDED, UNIT_PROVIDED, ASSIGNMENT_PROVIDED

  BOOK_COUNTER = 0
  CHAPTER_COUNTER = 0
  SECTION_COUNTER = 0
  UNIT_COUNTER = 0
  CHECKPOINT_COUNTER = 0
  ELEMENT_COUNTER = 0
  ATOM_COUNTER = 0
  ANSWER_COUNTER = 0
  ASSIGNMENT_COUNTER = 0
  ASSTPROBLEM_COUNTER = 0
  IN_ASSIGNMENT = False

  CHAPTER_PROVIDED = False
  SECTION_PROVIDED = False
  UNIT_PROVIDED = False
  ASSIGNMENT_PROVIDED = False

def provide_course (course_num):
  global COURSE_UNIQUE
  COURSE_UNIQUE = course_num

def get_course ():
  global COURSE_UNIQUE
  return COURSE_UNIQUE


# We assume that this is called 
# in the beginning as provided by the course
def provide_book (u):
  global BOOK_UNIQUE
  BOOK_UNIQUE = u

def provide_assignment (u):
  global ASSIGNMENT_COUNTER
  ASSIGNMENT_COUNTER = int(u) - 1

def new_assignment ():
  global ASSIGNMENT_COUNTER, IN_ASSIGNMENT
  ASSIGNMENT_COUNTER += 1
  IN_ASSIGNMENT = True

def exit_assignment ():
  global IN_ASSIGNMENT
  IN_ASSIGNMENT = False

def get_assignment ():
  global ASSIGNMENT_COUNTER
  return str(ASSIGNMENT_COUNTER)

def new_asstproblem ():
  global ASSTPROBLEM_COUNTER
  ASSTPROBLEM_COUNTER += 1

def get_asstproblem ():
  global ASSTPROBLEM_COUNTER
  return str(ASSTPROBLEM_COUNTER)

def get_question ():
  global ATOM_COUNTER
  return str(ATOM_COUNTER)

# There book unique should be set
# by the course, and so should the 
# chapter counter. 
# so there is nothing to do.
def new_book ():
  pass

def get_book (): 
  global BOOK_COUNTER
  return str(BOOK_COUNTER)

def new_chapter ():
  global CHAPTER_COUNTER, SECTION_COUNTER
  CHAPTER_COUNTER = CHAPTER_COUNTER + 1
  CHAPTER_PROVIDED = False
  # Reset nested counters
  if not SECTION_PROVIDED: 
    SECTION_COUNTER = 0
  print 'new chapter:', CHAPTER_COUNTER

def get_chapter ():
  global CHAPTER_COUNTER
  print 'uniques.get_chapter: ', CHAPTER_COUNTER
  return str(CHAPTER_COUNTER)

# We assume that this is called 
# before the beginning of the chapter
# and that the new chapter will be created 
# with new_chapter, which will increment.
# Therefore, we decrement.
def provide_chapter (no_str):
  global CHAPTER_COUNTER, CHAPTER_PROVIDED
  CHAPTER_COUNTER = int(no_str) - 1
  CHAPTER_PROVIDED = True
  print 'uniques.provide_chapter:', CHAPTER_COUNTER

def new_section ():
  global CHAPTER_COUNTER, SECTION_COUNTER, UNIT_COUNTER
  SECTION_COUNTER = SECTION_COUNTER + 1
  SECTION_PROVIDED = False
  print 'uniques.new_section, section:', SECTION_COUNTER

  # Reset nested counters
  if not UNIT_PROVIDED: 
    UNIT_COUNTER = 0

def get_section (): 
  global SECTION_COUNTER
  return str(SECTION_COUNTER)

# We assume that this is called 
# before the beginning of the section
# and that the new section will be created 
# with new_section, which will increment.
# Therefore, we decrement.
def provide_section (no_str):
  global SECTION_COUNTER, SECTION_PROVIDED
  SECTION_COUNTER = int(no_str) - 1
  SECTION_PROVIDED = True
  print 'set section:', SECTION_COUNTER

def new_unit ():
  global UNIT_COUNTER, ATOM_COUNTER, ELEMENT_COUNTER
  UNIT_COUNTER = UNIT_COUNTER + 1
  UNIT_PROVIDED = False
  # Reset nested counters
  ELEMENT_COUNTER = 0
  ATOM_COUNTER = 0

def get_unit (): 
  global UNIT_COUNTER
  return str(UNIT_COUNTER)

# We assume that this is called 
# before the beginning of the unit
# and that the new unit will be created 
# with new_unit, which will increment.
# Therefore, we decrement.
def provide_unit (no_str):
  global UNIT_COUNTER, UNIT_PROVIDED
  UNIT_COUNTER = int(no_str) - 1
  UNIT_PROVIDED = True
  print 'set unit:', UNIT_COUNTER


def new_element ():
  global ELEMENT_COUNTER, ATOM_COUNTER
  ELEMENT_COUNTER = ELEMENT_COUNTER + 1
  # Reset nested counters
  ATOM_COUNTER = 0

def new_group ():
  # a group is an element
  global GROUP_FLAG
  result = new_element ()
  GROUP_FLAG = True

def get_group (): 
  global ELEMENT_COUNTER
  return str(ELEMENT_COUNTER)

def new_checkpoint ():
  # a checkpoint is an element
  result = new_element ()

# def new_atom (do_create_group):
#   global ATOM_COUNTER, ELEMENT_COUNTER
#   ATOM_COUNTER = ATOM_COUNTER + 1
#   # Create a group (an element) if not nested within a group.
#   if do_create_group:
#     ELEMENT_COUNTER = ELEMENT_COUNTER + 1

def get_checkpoint (): 
  global ELEMENT_COUNTER
  return str(ELEMENT_COUNTER)

def new_atom ():
  global ATOM_COUNTER
  ATOM_COUNTER = ATOM_COUNTER + 1

def get_atom (): 
  global ATOM_COUNTER
  return str(ATOM_COUNTER)

def new_question ():
# questions are treated as atoms
  global ATOM_COUNTER, ANSWER_COUNTER, CHOICE_COUNTER
  ATOM_COUNTER = ATOM_COUNTER + 1
  # reset choice counter
#  print 'new question, resetting counters.'
  ANSWER_COUNTER = 0
  CHOICE_COUNTER = 0

def get_question (): 
  global ATOM_COUNTER
  return str(ATOM_COUNTER)

def new_answer ():
  global ANSWER_COUNTER
  ANSWER_COUNTER = ANSWER_COUNTER + 1

def get_answer (): 
  global ANSWER_COUNTER
  return str(ANSWER_COUNTER)

def new_choice ():
  global ANSWER_COUNTER
  ANSWER_COUNTER = ANSWER_COUNTER + 1

def get_choice (): 
  global ANSWER_COUNTER
  return str(ANSWER_COUNTER)

def get_select (): 
  global ANSWER_COUNTER
  return str(ANSWER_COUNTER)

def mk_unique_course():
  return PREFIX_COURSE + COURSE_UNIQUE

def mk_unique_book():
  return COURSE_UNIQUE + COLON + PREFIX_BOOK + BOOK_UNIQUE

def mk_unique_chapter():
  res = mk_unique_book() + COLON + PREFIX_CHAPTER + str(CHAPTER_COUNTER)
  return res 

def mk_unique_section():
  res = mk_unique_chapter() + COLON + PREFIX_SECTION + str(SECTION_COUNTER) 
  return res 

def mk_unique_unit():
  res = mk_unique_section() + COLON + PREFIX_UNIT + str(UNIT_COUNTER) 
  return res 

def mk_unique_group():
  res = mk_unique_unit() + COLON + PREFIX_GROUP + str(ELEMENT_COUNTER) 
  return res 

def mk_unique_checkpoint():
  res = mk_unique_group() + COLON + PREFIX_CHECKPOINT + str(ELEMENT_COUNTER) 
  return res 

def mk_unique_assignment():
  res = mk_unique_book() + COLON + PREFIX_ASSIGNMENT + str(ASSIGNMENT_COUNTER)
  return res

def mk_unique_asstproblem():
  res = mk_unique_assignment() + COLON + PREFIX_ASSTPROBLEM + str(ASSTPROBLEM_COUNTER)
  return res

def mk_unique_atom():
  res = mk_unique_group() + COLON + PREFIX_ATOM + str(ATOM_COUNTER) 
  return res 

def mk_unique_question():
  if IN_ASSIGNMENT:
    res = mk_unique_asstproblem() + COLON + PREFIX_QUESTION + str(ATOM_COUNTER)
    return res
  res = mk_unique_checkpoint() + COLON + PREFIX_QUESTION + str(ATOM_COUNTER) 
  return res 

def mk_unique_question_fr():
  return mk_unique_question() 

def mk_unique_question_ma():
  return mk_unique_question() 

def mk_unique_question_mc():
  return mk_unique_question() 


def mk_unique_answer():
  res = mk_unique_question_fr() + COLON + PREFIX_ANSWER + str(ANSWER_COUNTER) 
  return res

def mk_unique_choice():
  res = mk_unique_question_mc() + COLON + PREFIX_CHOICE + str(ANSWER_COUNTER) 
  return res

def mk_unique_select():
  res = mk_unique_question_ma() + COLON + PREFIX_SELECT + str(ANSWER_COUNTER) 
  return res

## END Uniques
######################################################################
