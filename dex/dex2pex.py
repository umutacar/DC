#!/usr/bin/python
######################################################################
##
## Translate dex to pandoc-friendly LaTeX.
##
## Assumes that the dex has been converted to the core language. 
##
## This requires running the parser once and using its output 
## (instead of the original file) to translate to TeX.

import os
import re
import sys
from functools import partial as curry
import pyparsing as pp 

import pervasives.os_utils as os_utils
from pervasives.parser import *
from pervasives.syntax import *

import syntax as dex
import parser as dex_parser
import blocks as blocks
import pex.syntax as pex

######################################################################
## BEGIN GLOBALS


## END: GLOBALS
######################################################################


######################################################################
## Translators to tex

def process_block_begin (this_block, toks):
  return toks

def process_block_end (this_block, toks):
  return toks

def process_chapter(process_label, toks): 
  print "chapter_matched:"

  chapter = blocks.Chapter(toks)
  r = chapter.to_pex_string()
  return r

def process_group (toks): 
  print 'group matched:'

  group = blocks.Group(toks)
  r = group.to_pex_string()
  return r

def process_problem_group (toks): 
  print 'problemgroup matched:'

  block = blocks.ProblemGroup(toks)
  r = block.to_pex_string()
#  print 'problemgroup: ', r
  return r


def process_problem_set (toks): 
  print 'problem_set matched:'

  problem_set = blocks.ProblemSet(toks)
  r = problem_set.to_pex_string()
#  print 'problem_set: ', r
  return r

def process_assignment (toks): 
  print 'assignment matched:'

  asst = blocks.Assignment(toks)
  r = asst.to_pex_string()
 # print(r)
  return r

def process_asstproblem (toks):
  print "asstproblem matched:"

  problem = blocks.AsstProblem(toks)
  r = problem.to_pex_string()
  return r

def process_section(process_label, toks): 
  print "section_matched:"
#  print "toks:", toks

  section = blocks.Section(toks)
  r = section.to_pex_string()
  return r

# convert subsection to tex
def process_subsection(toks): 
  print "subsection_matched."
#  print "toks:", toks
  subsection = blocks.Subsection(toks)
  r = subsection.to_pex_string()
  return r


# convert subsection to  tex 
def process_subsubsection(toks): 
  print "subsubsection_matched."
#  print "toks:", toks
  subsubsection = blocks.Subsubsection(toks)
  r = subsubsection.to_pex_string()
  return r

# convert an atom to tex
def process_atom (atom_name_dex, atom_name_tex, toks):
  print 'atom matched:', atom_name_dex, 'tex_name: ', atom_name_tex

  atom = blocks.Atom(atom_name_dex, False, toks)
  r = atom.to_pex_string(atom_name_tex)
  return r


def process_problem_fr (toks): 
  print 'problem_fr matched'

  problem = blocks.ProblemFR(toks)
  r = problem.to_pex_string()
  return r

def process_problem_ma (toks): 
  print 'problem_ma matched.'

  problem = blocks.ProblemMA(toks)
  r = problem.to_pex_string()
#  print 'problem_ma matched result =', r
  return r


def process_problem_mc (toks): 
  print 'problem_mc matched.'

  problem = blocks.ProblemMC(toks)
  r = problem.to_pex_string()
  return r


def process_question_fr (toks): 
  print 'question_fr matched'

  problem = blocks.QuestionFR(toks)
  r = problem.to_pex_string()
  return r

def process_question_ma (toks): 
  print 'question_ma matched.'

  problem = blocks.QuestionMA(toks)
  r = problem.to_pex_string()
#  print 'question_ma matched result =', r
  return r


def process_question_mc (toks): 
  print 'question_mc matched.'

  problem = blocks.QuestionMC(toks)
  r = problem.to_pex_string()
  return r


# convert a answer to tex, bogus probably, TODO
def process_answer ( toks):
  print 'answer matched.'
  answer = blocks.Answer(toks)
  r = answer.to_pex_string()
  return r

# convert a choice to tex
def process_choice ( toks):
  print 'choice matched.'

  choice = blocks.Choice(toks)
  r = choice.to_pex_string()
  return r

# convert a select to tex
def process_select ( toks):
  print 'select matched.'

  select = blocks.Select(toks)
  r = select.to_pex_string()
  return r

# convert an algo to tex
def process_algo (toks):
  print 'algo matched:'

  algo = blocks.Algo(toks)
  r = algo.to_pex_string()
  return r


## END Translators to tex
######################################################################

######################################################################
## Begin: Parser function

def parse (process_algo, \
           process_atom, \
           process_chapter, \
           process_group, \
           process_problem_fr, \
           process_problem_ma, \
           process_problem_mc, \
           process_problem_group, \
           process_problem_set, \
           process_section, \
           process_subsection, \
           data):

  ## Make parser, no's, labels, not optional
  labels_optional = True
  nos_optional = True
  uniques_optional = True
  parents_optional = True
  titles_optional = True

  parser = dex_parser.Parser (\
             parents_optional, \
             titles_optional, \
             process_block_begin, \
             process_block_end, \
             process_algo, \
             curry(process_atom, dex.ALGORITHM, pex.ALGORITHM_), \
             curry(process_atom, dex.CODE, pex.CODE_), \
             curry(process_atom, dex.COROLLARY, pex.COROLLARY_), \
             curry(process_atom, dex.COST_SPEC, pex.COST_SPEC_), \
             curry(process_atom, dex.DATASTR, pex.DATASTR_), \
             curry(process_atom, dex.DATATYPE, pex.DATATYPE_), \
             curry(process_atom, dex.DEFINITION, pex.DEFINITION_), \
             curry(process_atom, dex.EXAMPLE, pex.EXAMPLE_), \
             curry(process_atom, dex.EXERCISE, pex.EXERCISE_), \
             curry(process_atom, dex.HINT, pex.HINT_), \
             curry(process_atom, dex.IMPORTANT, pex.IMPORTANT_), \
             curry(process_atom, dex.LEMMA, pex.LEMMA_), \
             curry(process_atom, dex.NOTE, pex.NOTE_), \
             curry(process_atom, dex.PARAGRAPH, pex.PARAGRAPH_), \
             curry(process_atom, dex.PARAGRAPH_HTML, pex.PARAGRAPH_HTML_), \
             curry(process_atom, dex.PROBLEM, pex.PROBLEM_), \
             curry(process_atom, dex.PROOF, pex.PROOF_), \
             curry(process_atom, dex.PROPOSITION, pex.PROPOSITION_), \
             curry(process_atom, dex.REMARK, pex.REMARK_), \
             curry(process_atom, dex.SKIP, pex.SKIP_), \
             curry(process_atom, dex.SOLUTION, pex.SOLUTION_), \
             curry(process_atom, dex.SYNTAX, pex.SYNTAX_), \
             curry(process_atom, dex.TEACH_ASK, pex.TEACH_ASK_), \
             curry(process_atom, dex.TEACH_NOTE, pex.TEACH_NOTE_), \
             curry(process_atom, dex.THEOREM, pex.THEOREM_), \
             process_answer, \
             process_choice, \
             process_select, \
             process_chapter, \
             process_group, \
             process_problem_fr, \
             process_problem_ma, \
             process_problem_mc, \
             process_problem_group, \
             process_problem_set, \
             process_assignment, \
             process_section, \
             process_subsection, \
             process_subsubsection)

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


def main(infile_name):
  print "infile_name:", infile_name

  # (infile_name_first, infile_ext) = infile_name.split(PERIOD) 
  # lxfile_name = infile_name_first + os_utils.TEX_EXTENSION
  tex_file_name = os_utils.mk_file_name_ext(infile_name, os_utils.TEX_EXTENSION)
  print "tex_file_name:", tex_file_name
  infile = open(infile_name, 'r')
  tex_file = open(tex_file_name, 'w')

  data = infile.read ()

  result = parse (
             process_algo, \
             process_atom, \
             process_chapter, \
             process_group, \
             process_problem_fr, \
             process_problem_ma, \
             process_problem_mc, \
             process_problem_group, \
             process_problem_set, \
             process_section, \
             process_subsection, data)

  if len(result) == 1:
    result = result[0]
  else:
    print "Fatal Error: Unknown Input."
    exit(1)

  tex_file.write(result)
  tex_file.close()
  print "LaTeX code written into file:", tex_file_name
  return 0


if __name__ == "__main__":
  print 'Executing:', sys.argv[0], str(sys.argv)
  if len(sys.argv) != 2: 
    print 'Usage: dex2pex inputfile'
    sys.exit()

  infile_name = sys.argv[1]

  main(infile_name)
## END Mainline
######################################################################
