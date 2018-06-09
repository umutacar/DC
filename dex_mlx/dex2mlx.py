#!/usr/bin/python
import os
import re
import sys
from functools import partial as curry
import pyparsing as pp 

import pervasives.os
from pervasives.parser import *
from pervasives.syntax import *

import syntax as dex
import parser as dex_parser
import mlx.syntax as mlx
import blocks as blocks


######################################################################
## BEGIN GLOBALS

# used during mlx translation to locate course specif info and files
COURSE_NUMBER = 0

## MLX Preamble
MLX_PREAMBLE = '<?xml version="1.0" encoding="UTF-8"?>'


# These are not used but for future reference. 
DIDEROT_PREAMBLE = '<diderot: xmlns = "http://umut-acar.org/diderot">'
DIDEROT_POSTAMBLE = '</diderot>'

## END: GLOBALS
######################################################################


######################################################################
## Translators to mlx

## TODO
# translate latex code to mlx
def latex_to_mlx (s):
  return s

def process_block_begin (this_block, toks):
  return toks

def process_block_end (this_block, toks):
  return toks

def process_book(process_label, toks): 
  print "book matched:"
#  print "toks:", toks

  book = blocks.Book(toks)
  r = book.to_mlx_string()
  return r

def process_chapter(process_label, toks): 
  print "chapter_matched:"
#  print "toks:", toks

  chapter = blocks.Chapter(toks)
  r = chapter.to_mlx_string()
  return r

def process_course(process_label, toks): 
  global COURSE_NUMBER
  print "course matched:"
#  print "toks:", toks

  course = blocks.Course(toks)
  print "course number:", course.number

# TODO
#  blocks.init_latex2html(course.number)

#  r = course.to_mlx_string()
  return course

def process_group (toks): 
  print 'group matched:'

  group = blocks.Group(toks)
  r = group.to_mlx_string()
  return r

def process_checkpoint (toks): 
  print 'checkpoint matched:'

  checkpoint = blocks.Checkpoint(toks)
  r = checkpoint.to_mlx_string()
#  print 'checkpoint: ', r
  return r

def process_assignment (toks): 
  print 'assignment matched:'

  asst = blocks.Assignment(toks)
  r = asst.to_mlx_string()
 # print(r)
  return r

def process_asstproblem (toks):
  print "asstproblem matched:"

  problem = blocks.AsstProblem(toks)
  r = problem.to_mlx_string()
  return r

def process_section(process_label, toks): 
  print "section_matched:"
#  print "toks:", toks

  section = blocks.Section(toks)
  r = section.to_mlx_string()
  return r

# convert unit to  mlx 
def process_unit(toks): 
  print "unit_matched."
#  print "toks:", toks
  unit = blocks.Unit(toks)
  r = unit.to_mlx_string()
  return r

# convert an atom to mlx
def process_atom (atom_name_dex, atom_name_mlx, toks):
  print 'atom matched:', atom_name_dex

  atom = blocks.Atom(atom_name_dex, toks)
  r = atom.to_mlx_string(atom_name_mlx)
  return r


def process_question_fr (toks): 
  print 'question_fr matched'

  problem = blocks.QuestionFR(toks)
  r = problem.to_mlx_string()
  return r

def process_question_ma (toks): 
  print 'question_ma matched.'

  problem = blocks.QuestionMA(toks)
  r = problem.to_mlx_string()
#  print 'question_ma matched result =', r
  return r


def process_question_mc (toks): 
  print 'question_mc matched.'

  problem = blocks.QuestionMC(toks)
  r = problem.to_mlx_string()
  return r


# convert a answer to mlx, bogus probably, TODO
def process_answer ( toks):
  print 'answer matched.'
  answer = blocks.Answer(toks)
  r = answer.to_mlx_string()
  return r

# convert a choice to mlx
def process_choice ( toks):
  print 'choice matched.'

  choice = blocks.Choice(toks)
  r = choice.to_mlx_string()
  return r

# convert a select to mlx
def process_select ( toks):
  print 'select matched.'

  select = blocks.Select(toks)
  r = select.to_mlx_string()
  return r

# convert an algo to mlx
def process_algo (toks):
  print 'algo matched:'

  algo = blocks.Algo(toks)
  r = algo.to_mlx_string()
  return r


## END Translators to mlx
######################################################################

######################################################################
## Begin: Parser function

def parse (process_algo, \
           process_atom, \
           process_book, \
           process_chapter, \
           process_course, \
           process_group, \
           process_question_fr, \
           process_question_ma, \
           process_question_mc, \
           process_checkpoint, \
           process_section, \
           process_unit, \
           data):

  ## Make parser, no's, labels, not optional
  labels_optional = True
  nos_optional = True
  uniques_optional = True
  parents_optional = True
  titles_optional = True
  course_label_on = True

  parser = dex_parser.Parser (\
             labels_optional, \
             nos_optional, \
             uniques_optional, \
             parents_optional, \
             titles_optional, \
             course_label_on, \
             process_block_begin, \
             process_block_end, \
             process_algo, \
             curry(process_atom, dex.ALGORITHM, mlx.ALGORITHM), \
             curry(process_atom, dex.CODE, mlx.CODE), \
             curry(process_atom, dex.COROLLARY, mlx.COROLLARY), \
             curry(process_atom, dex.COST_SPEC, mlx.COST_SPEC), \
             curry(process_atom, dex.DATASTR, mlx.DATASTR), \
             curry(process_atom, dex.DATATYPE, mlx.DATATYPE), \
             curry(process_atom, dex.DEFINITION, mlx.DEFINITION), \
             curry(process_atom, dex.EXAMPLE, mlx.EXAMPLE), \
             curry(process_atom, dex.EXERCISE, mlx.EXERCISE), \
             curry(process_atom, dex.HINT, mlx.HINT), \
             curry(process_atom, dex.IMPORTANT, mlx.IMPORTANT), \
             curry(process_atom, dex.LEMMA, mlx.LEMMA), \
             curry(process_atom, dex.NOTE, mlx.NOTE), \
             curry(process_atom, dex.PARAGRAPH, mlx.PARAGRAPH), \
             curry(process_atom, dex.PROBLEM, mlx.PROBLEM), \
             curry(process_atom, dex.PROOF, mlx.PROOF), \
             curry(process_atom, dex.PROPOSITION, mlx.PROPOSITION), \
             curry(process_atom, dex.REMARK, mlx.REMARK), \
             curry(process_atom, dex.SOLUTION, mlx.SOLUTION), \
             curry(process_atom, dex.SYNTAX, mlx.SYNTAX), \
             curry(process_atom, dex.TEACH_ASK, mlx.TEACH_ASK), \
             curry(process_atom, dex.TEACH_NOTE, mlx.TEACH_NOTE), \
             curry(process_atom, dex.THEOREM, mlx.THEOREM), \
             process_answer, \
             process_choice, \
             process_select, \
             process_book, \
             process_chapter, \
             process_course, \
             process_group, \
             process_question_fr, \
             process_question_ma, \
             process_question_mc, \
             process_checkpoint, \
             process_assignment, \
             process_asstproblem, \
             process_section, \
             process_unit)

  try:
    result = parser.document.parseString(data)
  except pp.ParseException as pe:
    print "Parse Exception:", pe.line
    print(' '*(pe.col-1) + '^')
    print(pe)

  return result

## End: Parse
######################################################################


######################################################################
## BEGIN Mainline

def main(argv):

  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 3: 
    print 'Usage: dex2mlx inputfile latex_preamble'
    sys.exit()

  infile_name = sys.argv[1]
  latex_preamble_name = sys.argv[2]

  print "infile_name:", infile_name
  print "latex_preamble_name:", latex_preamble_name

  (infile_name_first, infile_ext) = infile_name.split(PERIOD) 
  mlxfile_name = infile_name_first + pervasives.os.MLX_EXTENSION
  print "mlxfile_name:", mlxfile_name
  infile = open(infile_name, 'r')
  mlxfile = open(mlxfile_name, 'w')

  data = infile.read ()

  blocks.init_latex2html(latex_preamble_name)

  result = parse (
             process_algo, \
             process_atom, \
             process_book, \
             process_chapter, \
             process_course, \
             process_group, \
             process_question_fr, \
             process_question_ma, \
             process_question_mc, \
             process_checkpoint, \
             process_section, \
             process_unit, data)


  ## Write output
  # The result consists of a course block and a book
  course_block = result[0]
#  print 'dex2mlx: course_block:', course_block
  book = result[1]
#  print 'dex2mlx: book:', book
  course_block.book = book
  result = MLX_PREAMBLE + course_block.to_mlx_string ()
  mlxfile.write(result)
#  book = NEWLINE.join(result[1:])
#  contents = course + NEWLINE + book  
#  mlxfile.write(contents)

  mlxfile.close()
  print "mlx code written into file:", mlxfile_name

if __name__ == "__main__":
    main(sys.argv)
## END Mainline
######################################################################
