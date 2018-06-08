######################################################################
## dex/blocks.py
######################################################################

######################################################################
## Label generation: we keep user labels but prepend them with the
## current chapter label to ensure uniques.  This is because we expect
## chapters to be uploaded one by one.
##
## If no label is provided for a block, then we generate an auto label
## by using uniques (pervasives/uniques)
## 
## There are several important invariants in the generation of labels.
######################################################################


 

from pervasives.parser import *
from pervasives.syntax import *
import pervasives.os as pos

import syntax as dex
import tokens as tokens
import uniques as uniques

import mlx.syntax as mlx
import latex.latex2html as latex2html
import string

######################################################################
## BEGIN: Globals

## Where latex generation takes place
TMP_DIR = r'/tmp'

## Where latex files are
LATEX_FILES_DIR = './latex/latex_files/'

# Default latex-to-html generator is None
Tex2Html = None

def init_latex2html (latex_preamble_file):
  global Tex2Html
  
  latex_postamble = r'\end{document}' + '\n'

  Tex2Html = latex2html.Latex2Html(\
                 TMP_DIR, \
                 latex_preamble_file, \
                 latex_postamble)

## END: Globals
######################################################################


######################################################################
## BEGIN: Utilities

def label_to_string (toks): 
  label = tokens.get_label(toks)
  if label is None:
    label = KW_UNLABELED
  return label

def unique_to_string (toks): 
  unique = tokens.get_unique(toks)
  if unique is None:
    unique = KW_NO_UNIQUE
  return unique


def extract_common(toks):
  title = tokens.get_title(toks)

  label = label_to_string(toks)
  no = tokens.get_no(toks)
#  unique = unique_to_string(toks)
  unique = tokens.get_unique(toks)
  parents = tokens.get_parents(toks)
#  print 'label:', label
#  print 'title:', title
#  print 'extract: unique:', unique
  return (title,  label, no, unique, parents)

def extract_common_pset (toks):
  course_number = tokens.get_course_number(toks)
  instance = tokens.get_instance(toks)
  folder = tokens.get_folder(toks)
  title = tokens.get_title_force(toks)
  points = tokens.get_points(toks)
  topics = tokens.get_topics(toks)
  prompt = tokens.get_prompt(toks)  

  unique = tokens.get_unique(toks)
#  print 'label:', label
  print 'title:', title
#  print 'extract: unique:', unique
  return (course_number, instance, folder,  points, prompt, title, topics)

def mk_str_generic(block_name, title, label, no, unique, parents, contents): 
  if contents:
    contents = contents.strip() + NEWLINE
  else:
    contents = ''
  result = NEWLINE + \
           dex.mk_str_begin(block_name) + \
           dex.mk_str_opt_arg(title) + NEWLINE + \
           dex.mk_str_label(label)  + NEWLINE + \
           dex.mk_str_no(no)  + NEWLINE + \
           dex.mk_str_unique(unique)  + NEWLINE + \
           dex.mk_str_parents (parents) + NEWLINE + NEWLINE + \
           contents + \
           dex.mk_str_end(block_name)
  return result

def mk_str_pset(pset): 
  if pset.contents:
    contents = pset.contents.strip() + NEWLINE
  else:
    contents = ''
  result = NEWLINE + \
           dex.mk_str_begin(dex.PROBLEMSET) +  NEWLINE + \
           dex.mk_str_course(pset.course)  + NEWLINE + \
           dex.mk_str_instance(pset.instance)  + NEWLINE + \
           dex.mk_str_folder(pset.folder)  + NEWLINE + \
           dex.mk_str_title_noarg(pset.title)  + NEWLINE + \
           dex.mk_str_points(pset.points)  + NEWLINE + \
           dex.mk_str_topics(pset.topics)  + NEWLINE + \
           dex.mk_str_prompt(pset.prompt)  + NEWLINE + \
           contents + \
           dex.mk_str_end(dex.PROBLEMSET)
  return result

# def mk_mlx_str_fields_common (title, label, no, unique, parents):
#   r = [mlx.mk_str_title(title), \
#        mlx.mk_str_unique(unique), \
#        mlx.mk_str_label(label), \
#        mlx.mk_str_no(no), \
#        mlx.mk_str_parents (parents)]
#   return r

def mk_mlx_str_fields_common (title, label, no, unique, parents, convert_title):

  if convert_title: 
    title_html = mlx.mk_str_title(Tex2Html.translate(unique+pos.TITLE_EXTENSION, title, True))
  else:
    title_html = mlx.mk_str_title(title)

  title_dex = mlx.mk_str_title_dex(title)

  r = [title_html, \
       title_dex, \
       mlx.mk_str_unique(unique), \
       mlx.mk_str_label(label), \
       mlx.mk_str_no(no), \
       mlx.mk_str_parents (parents)]
  return r

def mk_mlx_str_pset(pset): 
  if pset.contents:
    contents = pset.contents.strip() + NEWLINE
  else:
    contents = ''
  result = NEWLINE + \
           mlx.mk_str_begin(dex.PROBLEMSET) +  NEWLINE + \
           mlx.mk_str_course(pset.course)  + NEWLINE + \
           mlx.mk_str_instance(pset.instance)  + NEWLINE + \
           mlx.mk_str_folder(pset.folder)  + NEWLINE + \
           mlx.mk_str_title(pset.title)  + NEWLINE + \
           mlx.mk_str_points(pset.points)  + NEWLINE + \
           mlx.mk_str_topics(pset.topics)  + NEWLINE + \
           mlx.mk_str_prompt(pset.prompt)  + NEWLINE + \
           contents + \
           mlx.mk_str_end(dex.PROBLEMSET)
  return result

def mk_mlx_bodies (unique, body):
  body_dex = mlx.mk_str_body_dex(body)
  body_html = mlx.mk_str_body(Tex2Html.translate(unique+pos.BODY_EXTENSION, body, False))
  return (body_html, body_dex)

def mk_mlx_explains (unique, explain):
  explain_html = mlx.mk_str_explain(Tex2Html.translate(unique+pos.EXPLAIN_EXTENSION, explain, False))
  explain_dex = mlx.mk_str_explain_dex(explain)
  return (explain_html, explain_dex)

def mk_mlx_hints (unique, hint):
  hint_dex = mlx.mk_str_hint_dex(hint)
  hint_html = mlx.mk_str_hint(Tex2Html.translate(unique+pos.HINT_EXTENSION, hint, False))
  return (hint_html, hint_dex)

def mk_mlx_infos (unique, info):
  info_html = mlx.mk_str_info(Tex2Html.translate(unique + pos.INFO_EXTENSION, info, False))
  info_dex = mlx.mk_str_info_dex(info)
  return (info_html, info_dex)

def mk_mlx_intros (unique, intro):
  intro_html = mlx.mk_str_intro(Tex2Html.translate(unique+pos.INTRO_EXTENSION, intro, False))
  intro_dex = mlx.mk_str_intro_dex(intro)
  return (intro_html, intro_dex)

def mk_mlx_prompts (unique, prompt):
  prompt_dex = mlx.mk_str_prompt_dex(prompt)
  prompt_html = mlx.mk_str_prompt(Tex2Html.translate(unique+pos.PROMPT_EXTENSION, prompt, False))
  return (prompt_html, prompt_dex)

def mk_mlx_titles (unique, title):
  title_html = mlx.mk_str_title(Tex2Html.translate(unique+pos.TITLE_EXTENSION, title, True))
  title_dex = mlx.mk_str_title_dex(title)
  return (title_html, title_dex)



## END: Utilitios
######################################################################



######################################################################
## BEGIN: string converters

## Books
class Book:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.authors = tokens.get_authors(toks)
    self.contents = tokens.get_contents(toks)
    self.asst = tokens.get_assignment(toks)
#    print 'book.contents = ', self.contents

  def mk_unique (self):
    u = uniques.mk_unique_book ()
#    print 'book.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_BOOK_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_BOOK_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_book ()
#    print 'book.mk_no:', n
    return n

  def to_string (self): 
    contents = self.contents.strip ()
    asst = ''
    if self.asst is not None:
      asst = self.asst.strip()
    result = dex.mk_str_begin(dex.BOOK) + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_no(self.no)  + NEWLINE + \
             dex.mk_str_unique(self.unique)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + \
             dex.mk_str_authors(self.authors) + NEWLINE +  NEWLINE + \
             contents + NEWLINE + asst + NEWLINE + \
             dex.mk_str_end(dex.BOOK)
    return result
  
  def to_mlx_string (self):
    authors = mlx.mk_str_authors(self.authors)
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([authors, self.contents])
    if self.asst is not None:
      fields.extend([self.asst])
    r = mlx.mk_str_book(fields)
#    print 'book.to_mlx_string:', r
    return NEWLINE + r

## Chapters
class Chapter:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.picture = tokens.get_picture(toks)
    self.intro = tokens.get_intro(toks)
    self.contents = tokens.get_contents(toks)

  def mk_unique (self):
    u = uniques.mk_unique_chapter ()
#    print 'chapter.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_CHAPTER_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_CHAPTER_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_chapter ()
#    print 'chapter.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_picture(self.picture)  + NEWLINE + \
               self.intro + NEWLINE + \
               self.contents

    result = mk_str_generic (dex.CHAPTER, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result

  def to_mlx_string (self):
    picture = mlx.mk_str_picture(self.picture)
    
    (intro_html, intro_dex) = mk_mlx_intros (self.unique, self.intro)

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([picture, intro_html, intro_dex, self.contents])

    r = mlx.mk_str_chapter(fields)
    return NEWLINE + r

## Course
class Course:
  def __init__(self, toks):

    (self.title, self.label, self.no, self.unique, self.parents) = extract_common (toks)
    self.picture = tokens.get_picture(toks)
    self.provides_book = tokens.get_provides_book(toks)
    self.provides_chapter = tokens.get_provides_chapter(toks)
    self.provides_section = tokens.get_provides_section(toks)
    self.provides_unit = tokens.get_provides_unit(toks)
    self.provides_assignment = tokens.get_provides_assignment(toks)
    # intro
    self.intro = tokens.get_joker(toks)
    self.intro = self.intro.strip()

    self.number = tokens.get_course_number(toks)
    self.semester = tokens.get_semester(toks)
    self.website = tokens.get_website(toks)
  
    # self.book is None by default
    self.book = None

    # Set course label to be the number    
    self.label = self.number

    # Set course no to be its number    
    self.no = self.number


  def to_string (self): 
    # in DEX, book is not part of a course.
    result = NEWLINE + \
             dex.mk_str_document_class() + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_no(self.no)  + NEWLINE + \
             dex.mk_str_unique(self.unique)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + NEWLINE + \
             dex.mk_str_course_number(self.number)  + NEWLINE + \
             dex.mk_str_picture(self.picture)  + NEWLINE + \
             dex.mk_str_provides_book(self.provides_book)  + NEWLINE + \
             dex.mk_str_provides_chapter(self.provides_chapter)  + NEWLINE + \
             dex.mk_str_provides_section(self.provides_section)  + NEWLINE + \
             dex.mk_str_provides_unit(self.provides_unit)  + NEWLINE + \
             dex.mk_str_provides_assignment(self.provides_assignment) + NEWLINE + \
             dex.mk_str_semester(self.semester)  + NEWLINE + \
             dex.mk_str_website(self.website)    + NEWLINE + \
             self.intro

    return result

   ## INVARIANT requires self.book to be set
  def to_mlx_string (self):
    # TODO raise exception here
    if self.book == None:
      print 'Fatal Error.  Book must be set.'

    course_number = mlx.mk_str_course_number(self.number)
    picture = mlx.mk_str_picture(self.picture)
    semester = mlx.mk_str_semester(self.semester)
    website = mlx.mk_str_website(self.website)
    provides_book = mlx.mk_str_provides_book(self.provides_book)
    provides_chapter = mlx.mk_str_provides_chapter(self.provides_chapter)
    provides_section = mlx.mk_str_provides_section(self.provides_section)
    provides_unit = mlx.mk_str_provides_unit(self.provides_unit)
    provides_assignment = mlx.mk_str_provides_assignment(self.provides_assignment)
    (intro_html, intro_dex) = mk_mlx_intros (self.unique, self.intro)
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([course_number, picture, semester, website, \
                   provides_book, provides_chapter, provides_section, provides_unit, provides_assignment, \
                   intro_html, intro_dex, self.book])
    r = mlx.mk_str_course(fields)
#    print 'blocks.course: course:', r
    return NEWLINE + r  + NEWLINE

## Group
class Group:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)

    # could be None
    self.contents = tokens.get_group_contents(toks)  

  @classmethod
  def singleton_group(cls, block):
    # Make token dictionary
    toks = {tokens.KEY_GROUP_CONTENTS: block}
    return cls(toks)

  def mk_unique (self):
    u = uniques.mk_unique_group ()
#    print 'group.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_GROUP_LABEL + COLON + self.mk_unique_() + COLON + self.label
    else:
      return KW_PREFIX_GROUP_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_group ()
#    print 'group.mk_no:', n
    return n

  def to_string (self): 
#    print 'group: self.contents:', self.contents

    result = mk_str_generic (dex.GROUP, self.title, self.label, self.no, self.unique, self.parents, self.contents)
    return result

  def to_mlx_string (self):
#    print 'group: self.contents:', self.contents
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)
    if self.contents:
      fields.extend([self.contents])

    r = mlx.mk_str_group(fields)
    return NEWLINE + r


## Checkpoint
class Checkpoint:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.contents = tokens.get_contents(toks) 

  def mk_unique (self):
    u = uniques.mk_unique_checkpoint ()
#    print 'checkpoint.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_CHECKPOINT_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_CHECKPOINT_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_checkpoint ()
#    print 'checkpoint.mk_no:', n
    return n

  def to_string (self): 
    result = mk_str_generic (dex.CHECKPOINT, self.title, self.label, self.no, self.unique, self.parents, self.contents)
    return result

  def to_mlx_string (self):
    contents = self.contents.strip()
#    print 'checkpoint.to_mlx_string: contens: ', contents

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([contents])
    r = mlx.mk_str_checkpoint(fields)
    return NEWLINE + r

## Problemset
class ProblemSet:
  def __init__(self, toks):
    (self.course, self.instance, self.folder,  self.points, self.prompt, self.title, self.topics) = extract_common_pset(toks)
    self.contents = tokens.get_contents(toks) 

  def mk_unique (self):
    u = uniques.mk_unique_problemset ()
#    print 'problemset.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_PROBLEMSET_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_PROBLEMSET_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_problemset ()
#    print 'problemset.mk_no:', n
    return n

  def to_string (self): 
    result = mk_str_pset (self)
    return result

  def to_mlx_string (self):
    contents = self.contents.strip()
#    print 'problemset.to_mlx_string: contens: ', contents

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([contents])
    r = mlx.mk_str_problemset(fields)
    return NEWLINE + r


## Assignment
class Assignment:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.title = toks[1]
    self.duedate = tokens.get_duedate(toks)
    self.contents = tokens.get_contents(toks)

  #TODO: parse assignment number from latex
  def mk_unique (self):
    u = uniques.mk_unique_assignment ()
    return u

  def mk_label (self):
    if valid_label(self.label):
      return KW_PREFIX_ASSIGNMENT_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_ASSIGNMENT_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_assignment ()
#    print 'assignment.mk_no:', n
    return n

  def to_string (self):
    result = dex.mk_str_begin(dex.ASSIGNMENT) + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_no(self.no) + NEWLINE + \
             dex.mk_str_unique(self.unique)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + \
             dex.mk_str_duedate (self.duedate) + NEWLINE + \
             self.contents + NEWLINE + \
             dex.mk_str_end(dex.ASSIGNMENT) + NEWLINE
    return NEWLINE + result

  def to_mlx_string (self):
    contents = self.contents.strip()

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    duedate = mlx.mk_str_duedate (self.duedate.strip())
    fields.extend([duedate, contents])
    r = mlx.mk_str_assignment(fields)
    return NEWLINE + r

## Problem
class AsstProblem:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    # TODO: not sure if necessary
    self.title = toks[1]
    self.contents = tokens.get_contents(toks)
    self.info = tokens.get_info(toks)

  def mk_unique (self):
    u = uniques.mk_unique_asstproblem ()
    return u

  def mk_label (self):
    if valid_label(self.label):
      return KW_PREFIX_ASSTPROBLEM_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_ASSTPROBLEM_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_asstproblem ()
#    print 'asstproblem.mk_no:', n
    return n

  def to_string (self):
    result = dex.mk_str_begin(dex.ASSTPROBLEM) + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_no(self.no) + NEWLINE + \
             dex.mk_str_unique(self.unique)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + \
             dex.mk_str_info(self.info) + NEWLINE + \
             self.contents + NEWLINE + \
             dex.mk_str_end(dex.ASSTPROBLEM) + NEWLINE
    return NEWLINE + result

  def to_mlx_string (self):
    contents = self.contents.strip()
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    (info, info_dex) = mk_mlx_infos(self.unique, self.info)
    fields.extend([info, info_dex, contents])
    r = mlx.mk_str_asstproblem(fields)
    return NEWLINE + r


## Section
class Section:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.intro = tokens.get_intro(toks)
    self.contents = tokens.get_contents(toks) 

  def mk_unique (self):
    u = uniques.mk_unique_section ()
#    print 'section.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_SECTION_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_SECTION_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_section ()
#    print 'section.mk_no:', n
    return n

  def to_string (self): 
    contents = self.intro + NEWLINE + self.contents
    result = mk_str_generic (dex.SECTION, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result

  def to_mlx_string (self):
    (intro_html, intro_dex) = mk_mlx_intros (self.unique, self.intro)

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([intro_html, intro_dex, self.contents])
    r = mlx.mk_str_section(fields)
    return NEWLINE + r

## Unit
class Unit:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.contents = tokens.get_contents(toks)
    self.checkpoint = tokens.get_checkpoint(toks) 
#    print 'unit.constructor: checkpoint:', self.checkpoint

  def mk_unique (self):
    u = uniques.mk_unique_unit ()
#    print 'unit.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_UNIT_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_UNIT_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_unit ()
#    print 'uniques.mk_no:', n
    return n

  def to_string (self): 
    contents = self.contents + NEWLINE + self.checkpoint
    result = mk_str_generic (dex.UNIT, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result

  def to_mlx_string (self):
    contents = self.contents + NEWLINE + self.checkpoint
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, False)
    fields.extend([contents])
    r = mlx.mk_str_unit(fields)
    return NEWLINE + r

## Atom
class Atom:

  def __init__(self, name, toks):
    self.name = name
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.contents =  tokens.get_body(toks) 

  def mk_unique (self):
    u = uniques.mk_unique_atom ()
#    print 'atom.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_ATOM_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_ATOM_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_atom ()
#    print 'atom.mk_no:', n
    return n

  def to_string (self): 
    result = mk_str_generic (self.name, self.title, self.label, self.no, self.unique, self.parents, self.contents)
    return result

  def to_mlx_string (self, atom_name_mlx):
    contents =  self.contents
    (contents_html, contents_dex) = mk_mlx_bodies(self.unique, contents)

    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)
    fields.extend([contents_html, contents_dex])
    r = mlx.mk_str_block_atom(atom_name_mlx, fields)
    return NEWLINE + r

## Algo(rithm)
class Algo:

  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    # contents is a list of lines
    self.contents =  tokens.get_body(toks)

  # treat this as an atom for uniques
  def mk_unique (self):
    u = uniques.mk_unique_atom ()
#    print 'algo.mk_unique:', u
    return u

  # Treat it as an atom for labels purposes
  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_ATOM_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_ATOM_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_atom ()
#    print 'algo.mk_no:', n
    return n

  def indent_line (self, line):
    suffix = line.lstrip(SPACE)
#    print 'suffix:', suffix
    # number of spaces (indentation)
    n = len(line) - len(suffix)
    indentation = TILDE * n
    result = KW_TEX_ITEM + SPACE + indentation + suffix + NEWLINE
    return result

  def elaborate_contents (self): 
#    print '** matched algo.'
#    print 'contents:', self.contents
    contents = map(lambda x:self.indent_line(x), self.contents)
    contents = EMPTY_STRING.join(contents)
    contents = \
      KW_TEX_BEGIN_ENUMERATE + NEWLINE + \
      contents + NEWLINE + \
      KW_TEX_END_ENUMERATE 

    # contents is a always list
    self.contents = [contents]
#    print 'elaborated contents:', self.contents
    return contents

  def to_string (self): 
#    print '** matched algo.'
    contents = NEWLINE.join(self.contents)
    result = mk_str_generic (dex.ALGO, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result
    
  def to_mlx_string (self):
    contents = NEWLINE.join(self.contents)
    (title_html, title_dex) = mk_mlx_titles(self.unique, self.title)
    (contents_html, contents_dex) = mk_mlx_bodies(self.unique, contents)
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)
    fields.extend([contents_html, contents_dex])
    r = mlx.mk_str_atom(mlx.ALGORITHM, fields)
    return NEWLINE + r


## Answers
class Answer:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points_opt(toks)
    self.body = tokens.get_body(toks)
    self.explain = tokens.get_explain(toks)

  def mk_unique (self):
    u = uniques.mk_unique_answer ()
#    print 'answer.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_ANSWER_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_ANSWER_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_answer ()
#    print 'answer.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_title(self.title) + NEWLINE + \
               self.body + NEWLINE + \
               dex.mk_str_explain(self.explain)

    # Use points as title.
    result = mk_str_generic (dex.ANSWER, self.points, self.label, self.no, self.unique, self.parents, contents)
#    print 'answer:', result

    return result

  def to_mlx_string (self): 

    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # body
    (body_html, body_dex) = mk_mlx_bodies(self.unique, self.body)
    
    # explain
    (explain_html, explain_dex) = mk_mlx_explains(self.unique, self.explain)

    fields.extend([points, body_html, body_dex, explain_html, explain_dex])

    r = mlx.mk_str_answer(fields)
    return NEWLINE + r

## Choices
class Choice:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.body = tokens.get_body(toks)
    self.explain = tokens.get_explain(toks)
    self.points = tokens.get_points_opt(toks)
#    print 'self.point:', self.points
#    print 'self.title:', self.title


  def mk_unique (self):
    u = uniques.mk_unique_choice ()
#    print 'choice.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_CHOICE_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_CHOICE_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_choice ()
#    print 'choice.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_title(self.title) + NEWLINE + \
               self.body + NEWLINE + \
               dex.mk_str_explain(self.explain)

    # Use points as title.
    result = mk_str_generic (dex.CHOICE, self.points, self.label, self.no, self.unique, self.parents, contents)
#    print 'choice:', result

    return result

  def to_mlx_string (self): 

    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # body
    (body_html, body_dex) = mk_mlx_bodies(self.unique, self.body)
    # explain
    (explain_html, explain_dex) = mk_mlx_explains(self.unique, self.explain)

    fields.extend([points, body_html, body_dex, explain_html, explain_dex])

    r = mlx.mk_str_choice(fields)
    return NEWLINE + r

## Selects
class Select:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.body = tokens.get_body(toks)
    self.explain = tokens.get_explain(toks)
    self.points = tokens.get_points_opt(toks)
#    print 'self.point:', self.points
#    print 'self.title:', self.title


  def mk_unique (self):
    u = uniques.mk_unique_select ()
#    print 'select.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_SELECT_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_SELECT_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_select ()
#    print 'select.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_title(self.title) + NEWLINE + \
               self.body + NEWLINE + \
               dex.mk_str_explain(self.explain)

    # Use points as title.
    result = mk_str_generic (dex.SELECT, self.points, self.label, self.no, self.unique, self.parents, contents)
#    print 'select:', result

    return result

  def to_mlx_string (self): 

    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # body
    (body_html, body_dex) = mk_mlx_bodies(self.unique, self.body)
    # explain
    (explain_html, explain_dex) = mk_mlx_explains(self.unique, self.explain)

    fields.extend([points, body_html, body_dex, explain_html, explain_dex])

    r = mlx.mk_str_select(fields)
    return NEWLINE + r



## Problem_FR
class ProblemFR:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
# simple answer are converted to answer blocks.
#    self.ans = tokens.get_ans(toks)
#    self.explain = tokens.get_explain(toks) 
    self.answers = tokens.get_answers(toks) 
#    print 'problemfr: answers:', self.answers

  def mk_unique (self):
    u = uniques.mk_unique_problem_fr ()
#    print 'problem.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_PROBLEM_FR_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_PROBLEM_FR_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_problem ()
#    print 'problem.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.answers

    result = mk_str_generic (dex.PROBLEM_FR, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)

    # ### BEGIN :DELETE THIS
    # # explain
    # (explain_html, explain_dex) = mk_mlx_explains(self.unique, self.explain)

    # # solution - @umut - I changed self.solution here to self.ans for the code to compile
    # field_solution_dex = mlx.mk_str_solution_dex(self.ans)  
    # field_solution = mlx.mk_str_solution(Tex2Html.translate(self.unique+pos.SOLUTION_EXTENSION, self.ans, False))

    # ### END:Q DELETE THIS
    
    # put all fields together
    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, self.answers])

    # make the block
    r = mlx.mk_str_problem_fr(fields)

    return  NEWLINE + r


## Problem_ma
## TODO THIS NEEDS WORK CURRENTL COPY OF MC
class ProblemMA:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'problemmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.selects = tokens.get_selects(toks) 
#    print 'problemmc: self.selects', self.selects

  def mk_unique (self):
    u = uniques.mk_unique_problem_ma ()
#    print 'problemma.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_PROBLEM_MA_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_PROBLEM_MA_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_problem ()
#    print 'problem.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.selects


    result = mk_str_generic (dex.PROBLEM_MA, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)


    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, \
                   self.selects])

    # make the block
    r = mlx.mk_str_problem_ma(fields)

    return NEWLINE + r

## Problem_Mc
class ProblemMC:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'problemmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.choices = tokens.get_choices(toks) 
#    print 'problemmc: self.choices', self.choices

  def mk_unique (self):
    u = uniques.mk_unique_problem_mc ()
#    print 'problemmc.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_PROBLEM_MC_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_PROBLEM_MC_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_problem ()
#    print 'problem.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.choices

    result = mk_str_generic (dex.PROBLEM_MC, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)
    
    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)

    # put all fields together
    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, \
                   self.choices])

    # make the block
    r = mlx.mk_str_problem_mc(fields)

    return NEWLINE + r

## Question_FR
class QuestionFR:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
# simple answer are converted to answer blocks.
#    self.ans = tokens.get_ans(toks)
#    self.explain = tokens.get_explain(toks) 
    self.answers = tokens.get_answers(toks) 
#    print 'questionfr: answers:', self.answers

  def mk_unique (self):
    u = uniques.mk_unique_question_fr ()
#    print 'question.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_QUESTION_FR_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_QUESTION_FR_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_question ()
#    print 'question.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.answers

    result = mk_str_generic (dex.QUESTION_FR, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)

    # ### BEGIN :DELETE THIS
    # # explain
    # (explain_html, explain_dex) = mk_mlx_explains(self.unique, self.explain)

    # # solution - @umut - I changed self.solution here to self.ans for the code to compile
    # field_solution_dex = mlx.mk_str_solution_dex(self.ans)  
    # field_solution = mlx.mk_str_solution(Tex2Html.translate(self.unique+pos.SOLUTION_EXTENSION, self.ans, False))

    # ### END:Q DELETE THIS
    
    # put all fields together
    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, self.answers])

    # make the block
    r = mlx.mk_str_question_fr(fields)

    return  NEWLINE + r


## Question_ma
## TODO THIS NEEDS WORK CURRENTL COPY OF MC
class QuestionMA:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'questionmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.selects = tokens.get_selects(toks) 
#    print 'questionmc: self.selects', self.selects

  def mk_unique (self):
    u = uniques.mk_unique_question_ma ()
#    print 'questionma.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_QUESTION_MA_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_QUESTION_MA_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_question ()
#    print 'question.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.selects


    result = mk_str_generic (dex.QUESTION_MA, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)


    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, \
                   self.selects])

    # make the block
    r = mlx.mk_str_question_ma(fields)

    return NEWLINE + r

## Question_Mc
class QuestionMC:
  def __init__(self, toks):
    (self.title, self.label, self.no, self.unique, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'questionmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.choices = tokens.get_choices(toks) 
#    print 'questionmc: self.choices', self.choices

  def mk_unique (self):
    u = uniques.mk_unique_question_mc ()
#    print 'questionmc.mk_unique:', u
    return u

  def mk_label (self): 
    if valid_label(self.label):
      return KW_PREFIX_QUESTION_MC_LABEL + COLON + self.mk_unique() + COLON + self.label
    else:
      return KW_PREFIX_QUESTION_MC_LABEL + COLON + self.mk_unique()

  def mk_no (self):
    n = uniques.get_question ()
#    print 'question.mk_no:', n
    return n

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.choices

    result = mk_str_generic (dex.QUESTION_MC, self.title, self.label, self.no, self.unique, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self.title, self.label, self.no, self.unique, self.parents, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.unique, self.prompt)
    
    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.unique, self.hint)

    # put all fields together
    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, \
                   self.choices])

    # make the block
    r = mlx.mk_str_question_mc(fields)

    return NEWLINE + r

## END: block classes
######################################################################


## END: block classes
######################################################################

######################################################################
## BEGIN: string converters

def answer_to_string(toks): 
  block = Answer(toks)
  print '        matched answer'
  return block.to_string()

def algo_to_string(toks): 
  block = Algo(toks)
  print '        matched algo', '[', block.title, '].'
  return block.to_string()

def assignment_to_string(toks):
  print '      matched assignment.'
  block = Assignment(toks)
  return block.to_string()

def asstproblem_to_string(toks):
  print '      matched asstproblem.'
  block = AsstProblem(toks)
  return block.to_string()

def atom_to_string(name, toks): 
#  print 'atom to string: toks = ', toks 
  block = Atom(name, toks)
  print '        matched atom', name, '[', block.title, '].'
  return block.to_string()

def book_to_string(toks): 
  # Book is wrapped in a group, so unwray
  block = Book(toks)
  print 'matched book', '[', block.title, '].'
  return block.to_string()

def chapter_to_string(toks): 
  print 'matched chapter.'
  block = Chapter(toks)
  print 'matched chapter', '[', block.title, '].'
  return block.to_string()

def choice_to_string(toks): 
  print '          matched choice.'
  block = Choice(toks)
  return block.to_string()

def course_to_string(toks): 
  print 'matched course.'
  block = Course(toks)
  return block.to_string()

def group_to_string(toks): 
  block = Group(toks)
  print '      matched group', '[', block.title, '].'
  return block.to_string()

def section_to_string(toks): 
  block = Section(toks)
  print '  matched section', '[', block.title, '].'
  return block.to_string()

def unit_to_string(toks): 
  block = Unit(toks)
  print '    matched unit', '[', block.title, '].'
  return block.to_string()

def question_fr_to_string(toks): 
  print '        matched question_fr.'
  block = QuestionFR(toks)
  return block.to_string()

def question_ma_to_string(toks): 
  print '        matched question_ma.'
  block = QuestionMA(toks)
  return block.to_string()

def question_mc_to_string(toks): 
  print '        matched question_mc.'
  block = QuestionMC(toks)
  return block.to_string()

def problem_fr_to_string(toks): 
  print '        matched problem_fr.'
  block = ProblemFR(toks)
  return block.to_string()

def problem_ma_to_string(toks): 
  print '        matched problem_ma.'
  block = ProblemMA(toks)
  return block.to_string()

def problem_mc_to_string(toks): 
  print '        matched problem_mc.'
  block = ProblemMC(toks)
  return block.to_string()

def checkpoint_to_string(toks): 
  print '      matched checkpoint.'
  block = Checkpoint(toks)
  return block.to_string()

def problemset_to_string(toks): 
  print '      matched problemset.'
  block = ProblemSet(toks)
  return block.to_string()

def select_to_string(toks): 
  print '          matched select.'
  block = Select(toks)
  return block.to_string()

## END: string converters
######################################################################


