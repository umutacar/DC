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
import pervasives.os_utils as os_utils

import syntax as dex
import tokens as tokens

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

INDEX = 0

def init (latex_preamble_file):
  global Tex2Html

  INDEX = 0
  latex_postamble = r'\end{document}' + '\n'

  Tex2Html = latex2html.Latex2Html(\
                 TMP_DIR, \
                 latex_preamble_file, \
                 latex_postamble)


def new_index ():
  global INDEX

  INDEX = INDEX + 1
  return str(INDEX)

## END: Globals
######################################################################


######################################################################
## BEGIN: Utilities

def label_to_string (toks): 
  label = tokens.get_label(toks)
  if label is None:
    label = KW_UNLABELED
  return label

def label_to_string_force (toks): 
  label = tokens.get_label_force(toks)
  if label is None:
    label = KW_UNLABELED
  return label

def extract_common(toks):
  title = tokens.get_title(toks)

  label = label_to_string(toks)
  parents = tokens.get_parents(toks)
  return (title,  label, parents)

def extract_common_problem(toks):
  hint = tokens.get_hint(toks) 
  label = label_to_string_force(toks)
  points = tokens.get_points(toks) 
  prompt = tokens.get_prompt(toks)
  title = tokens.get_title(toks)
  topics = tokens.get_topics(toks) 
  return (hint, label, points, prompt, title, topics)

def extract_common_solution(toks):
  body = tokens.get_body(toks)
  print 'body:', body
  explain = tokens.get_explain(toks) 
  print 'explain:', explain
  label = label_to_string_force(toks)
  print 'label', label
  points = tokens.get_points_opt(toks) 
  print 'points', points
  title = tokens.get_title(toks)
  print 'title', title
  return (body, explain, label, points, title)

def extract_problem_group (toks):
  title = tokens.get_title(toks)
  label = label_to_string_force(toks)
  points = tokens.get_points(toks)
  topics = tokens.get_topics(toks)
  prompt = tokens.get_prompt(toks)  
  return (title, label, points, prompt, topics)


def extract_problem_set (toks):
  course_number = tokens.get_course_number(toks)
  instance = tokens.get_instance(toks)
  folder = tokens.get_folder(toks)
  title = tokens.get_title_force(toks)
  label = label_to_string_force(toks)
  parents = tokens.get_parents(toks)  
  points = tokens.get_points(toks)
  topics = tokens.get_topics(toks)
  prompt = tokens.get_prompt(toks)  

  return (course_number, instance, folder, title, label, parents, points, prompt, topics)

def mk_str_generic(block_name, title, label, parents, contents): 
  if contents:
    contents = contents.strip() + NEWLINE
  else:
    contents = ''
  result = NEWLINE + \
           dex.mk_str_begin(block_name) + \
           dex.mk_str_opt_arg(title) + NEWLINE + \
           dex.mk_str_label(label)  + NEWLINE + \
           dex.mk_str_parents (parents) + NEWLINE + NEWLINE + \
           contents + \
           dex.mk_str_end(block_name)
  return result

#           dex.mk_str_parents_noarg (parents) + NEWLINE + NEWLINE + \

def mk_str_generic_problem(block_name, title, label, contents): 
  if contents:
    contents = contents.strip() + NEWLINE
  else:
    contents = ''
  result = NEWLINE + \
           dex.mk_str_begin(block_name) + \
           dex.mk_str_opt_arg(title) + NEWLINE + \
           dex.mk_str_label_noarg (label)  + NEWLINE + \
           contents + \
           dex.mk_str_end(block_name)
  return result

def mk_str_generic_solution(block_name, solution): 
  result = NEWLINE + \
           dex.mk_str_begin(block_name) + \
           dex.mk_str_opt_arg(solution.title) + NEWLINE + \
           dex.mk_str_label_noarg (solution.label)  + NEWLINE + \
           dex.mk_str_points (solution.points)  + NEWLINE + \
           dex.mk_str_body_noarg(solution.body) + NEWLINE + \
           dex.mk_str_explain(solution.explain) + NEWLINE + \
           dex.mk_str_end(block_name)
  return result


def mk_mlx_str_fields_common (block, convert_title):
  index = block.index
  title = block.title
  label = block.label
  parents = block.parents
  
  if convert_title: 
    title_html = mlx.mk_str_title(Tex2Html.translate(index+os_utils.TITLE_EXTENSION, title, True))
  else:
    title_html = mlx.mk_str_title(title)

  title_dex = mlx.mk_str_title_dex(title)

  r = [title_html, \
       title_dex, \
       mlx.mk_str_label(label), \
       mlx.mk_str_parents (parents)]
  return r

def mk_mlx_str_fields_problem_group(block): 

  if block.contents:
    contents = block.contents.strip() + NEWLINE
  else:
    contents = ''

  result = [\
    mlx.mk_str_title(block.title), \
    mlx.mk_str_label(block.label), \
    mlx.mk_str_points(block.points), \
    mlx.mk_str_topics(block.topics), \
    mlx.mk_str_prompt(block.prompt), \
    contents\
   ]            
  return result


def mk_mlx_str_fields_problem_set(pset): 
  if pset.contents:
    contents = pset.contents.strip() + NEWLINE
  else:
    contents = ''
  result = [\
    mlx.mk_str_course_number(pset.course_number), \
    mlx.mk_str_instance(pset.instance), \
    mlx.mk_str_folder(pset.folder), \
    mlx.mk_str_label(pset.label), \
    mlx.mk_str_title(pset.title), \
    mlx.mk_str_points(pset.points), \
    mlx.mk_str_topics(pset.topics), \
    mlx.mk_str_prompt(pset.prompt), \
    contents\
   ]            
  return result


def mk_mlx_str_fields_problem (block, convert_title):
  index = block.index
  title = block.title
  label = block.label
  points = block.points
  prompt = block.prompt
  hint = block.hint

  # title  
  title_dex = mlx.mk_str_title_dex(title)
  if convert_title: 
    title_html = mlx.mk_str_title(Tex2Html.translate(index+os_utils.TITLE_EXTENSION, title, True))
  else:
    title_html = mlx.mk_str_title(title)



  # label
  label = mlx.mk_str_label(label)

  # points
  points = mlx.mk_str_points(points)

  # prompt
  (prompt_html, prompt_dex) = mk_mlx_bodies(index, prompt)

    
  # hint
  (hint_html, hint_dex) = mk_mlx_hints(index, hint)

 
  fields = [title_html, title_dex, label, points, prompt_dex, prompt_html, hint_dex, hint_html] 

  return fields

def mk_mlx_str_fields_solution (block, convert_title):
  index = block.index
  title = block.title
  label = block.label
  points = block.points
  body = block.body
  explain = block.explain

  # title  
  title_dex = mlx.mk_str_title_dex(title)
  if convert_title: 
    title_html = mlx.mk_str_title(Tex2Html.translate(index+os_utils.TITLE_EXTENSION, title, True))
  else:
    title_html = mlx.mk_str_title(title)



  # body
  (body_html, body_dex) = mk_mlx_bodies(index, body)
    
  # explain
  (explain_html, explain_dex) = mk_mlx_explains(index, explain)

  # label
  label = mlx.mk_str_label(label)

  # points
  points = mlx.mk_str_points(points)
 
  fields = [title_html, title_dex, label, points, body_dex, body_html, explain_dex, explain_html] 

  return fields


def mk_mlx_bodies (index, body):
  body_dex = mlx.mk_str_body_dex(body)
  body_html = mlx.mk_str_body(Tex2Html.translate(index+os_utils.BODY_EXTENSION, body, False))
  return (body_html, body_dex)

def mk_mlx_explains (index, explain):
  explain_html = mlx.mk_str_explain(Tex2Html.translate(index+os_utils.EXPLAIN_EXTENSION, explain, False))
  explain_dex = mlx.mk_str_explain_dex(explain)
  return (explain_html, explain_dex)

def mk_mlx_hints (index, hint):
  hint_dex = mlx.mk_str_hint_dex(hint)
  hint_html = mlx.mk_str_hint(Tex2Html.translate(index+os_utils.HINT_EXTENSION, hint, False))
  return (hint_html, hint_dex)

def mk_mlx_infos (index, info):
  info_html = mlx.mk_str_info(Tex2Html.translate(index + os_utils.INFO_EXTENSION, info, False))
  info_dex = mlx.mk_str_info_dex(info)
  return (info_html, info_dex)

def mk_mlx_intros (index, intro):
  intro_html = mlx.mk_str_intro(Tex2Html.translate(index+os_utils.INTRO_EXTENSION, intro, False))
  intro_dex = mlx.mk_str_intro_dex(intro)
  return (intro_html, intro_dex)

def mk_mlx_prompts (index, prompt):
  prompt_dex = mlx.mk_str_prompt_dex(prompt)
  prompt_html = mlx.mk_str_prompt(Tex2Html.translate(index+os_utils.PROMPT_EXTENSION, prompt, False))
  return (prompt_html, prompt_dex)

def mk_mlx_titles (index, title):
  title_html = mlx.mk_str_title(Tex2Html.translate(index+os_utils.TITLE_EXTENSION, title, True))
  title_dex = mlx.mk_str_title_dex(title)
  return (title_html, title_dex)



## END: Utilitios
######################################################################



######################################################################
## BEGIN: string converters

## Books
class Book:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.authors = tokens.get_authors(toks)
    self.contents = tokens.get_contents(toks)
#    print 'book.contents = ', self.contents

  def mk_label (self):
    return self.label

  def to_string (self): 
    contents = self.contents.strip ()
    result = dex.mk_str_begin(dex.BOOK) + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + \
             dex.mk_str_authors(self.authors) + NEWLINE +  NEWLINE + \
             contents + NEWLINE + \
             dex.mk_str_end(dex.BOOK)
    return result
  
  def to_mlx_string (self):
    authors = mlx.mk_str_authors(self.authors)
    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([authors, self.contents])
    r = mlx.mk_str_book(fields)
#    print 'book.to_mlx_string:', r
    return NEWLINE + r

## Chapters
class Chapter:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.picture = tokens.get_picture(toks)
    self.intro = tokens.get_intro(toks)
    self.contents = tokens.get_contents(toks)

  def mk_label (self):
    return self.label

  def to_string (self): 
    contents = dex.mk_str_picture(self.picture)  + NEWLINE + \
               self.intro + NEWLINE + \
               self.contents

    result = mk_str_generic (dex.CHAPTER, self.title, self.label, self.parents, contents)
    return result

  def to_mlx_string (self):
    picture = mlx.mk_str_picture(self.picture)
    
    (intro_html, intro_dex) = mk_mlx_intros (self.index, self.intro)

    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([picture, intro_html, intro_dex, self.contents])

    r = mlx.mk_str_chapter(fields)
    return NEWLINE + r

## Course
class Course:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common (toks)
    self.picture = tokens.get_picture(toks)
    self.provides_book = tokens.get_provides_book(toks)
    self.provides_chapter = tokens.get_provides_chapter(toks)
    self.provides_section = tokens.get_provides_section(toks)
    self.provides_subsection = tokens.get_provides_subsection(toks)
    self.provides_assignment = tokens.get_provides_assignment(toks)
    # intro
    self.intro = tokens.get_joker(toks)
    self.intro = self.intro.strip()

    self.number = tokens.get_course_number(toks)
    self.semester = tokens.get_semester(toks)
    self.website = tokens.get_website(toks)
  
    # self.book is None by default
    self.book = None

    # Set course no to be its number    
    self.no = self.number

  def to_string (self): 
    # in DEX, book is not part of a course.
#
# Books don't have labels and parents
#             dex.mk_str_label(self.label)  + NEWLINE + \
#             dex.mk_str_parents (self.parents) + NEWLINE + NEWLINE + \

    result = NEWLINE + \
             dex.mk_str_document_class() + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_course_number(self.number)  + NEWLINE + \
             dex.mk_str_picture(self.picture)  + NEWLINE + \
             dex.mk_str_provides_book(self.provides_book)  + NEWLINE + \
             dex.mk_str_provides_chapter(self.provides_chapter)  + NEWLINE + \
             dex.mk_str_provides_section(self.provides_section)  + NEWLINE + \
             dex.mk_str_provides_subsection(self.provides_subsection)  + NEWLINE + \
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
    provides_subsection = mlx.mk_str_provides_subsection(self.provides_subsection)
    provides_assignment = mlx.mk_str_provides_assignment(self.provides_assignment)
    (intro_html, intro_dex) = mk_mlx_intros (self.index, self.intro)
    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([course_number, picture, semester, website, \
                   provides_book, provides_chapter, provides_section, provides_subsection, provides_assignment, \
                   intro_html, intro_dex, self.book])
    r = mlx.mk_str_course(fields)
#    print 'blocks.course: course:', r
    return NEWLINE + r  + NEWLINE

## Group
class Group:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)

    # could be None
    self.contents = tokens.get_group_contents(toks)  

  @classmethod
  def singleton_group(cls, block):
    # Make token dictionary
    toks = {tokens.KEY_GROUP_CONTENTS: block}
    return cls(toks)

  def mk_label (self):
    return self.label

  def to_string (self): 
#    print 'group: self.contents:', self.contents

    result = mk_str_generic (dex.GROUP, self.title, self.label, self.parents, self.contents)
    return result

  def to_mlx_string (self):
#    print 'group: self.contents:', self.contents
    fields = mk_mlx_str_fields_common(self, True)
    if self.contents:
      fields.extend([self.contents])

    r = mlx.mk_str_group(fields)
    return NEWLINE + r


## Checkpoint
class Checkpoint:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.contents = tokens.get_contents(toks) 

  def mk_label (self): 
    return self.label

  def to_string (self): 
    result = mk_str_generic (dex.CHECKPOINT, self.title, self.label, self.parents, self.contents)
    return result

  def to_mlx_string (self):
    contents = self.contents.strip()
#    print 'checkpoint.to_mlx_string: contens: ', contents

    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([contents])
    r = mlx.mk_str_checkpoint(fields)
    return NEWLINE + r


## Problemset
class ProblemGroup:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.points, self.prompt, self.topics) = extract_problem_group(toks)
    print "title = ", self.title
    print "label = ", self.label
    print "points = ", self.points
    print "prompt = ", self.prompt
    print "topics = ", self.topics            

    self.contents = tokens.get_contents(toks) 

  def mk_label (self):
    return self.label    

  def to_string (self): 
    if self.contents:
      contents = self.contents.strip() + NEWLINE
    else:
      contents = ''

    result = NEWLINE + \
      dex.mk_str_begin(dex.PROBLEM_GROUP) + \
      dex.mk_str_opt_arg(self.title)  + NEWLINE + \
      dex.mk_str_label_noarg(self.label)  + NEWLINE + \
      dex.mk_str_points(self.points)  + NEWLINE + \
      dex.mk_str_topics(self.topics)  + NEWLINE + \
      dex.mk_str_prompt(self.prompt)  + NEWLINE + \
      contents + \
      dex.mk_str_end(dex.PROBLEM_GROUP)

    return result


  def to_mlx_string (self):
    fields = mk_mlx_str_fields_problem_group(self)
    r = mlx.mk_str_problem_group(fields)
    return NEWLINE + r


## Problemset
class ProblemSet:
  def __init__(self, toks):
    self.index = new_index ()
    (self.course_number, self.instance, self.folder, self.title, self.label, self.parents,  self.points, self.prompt, self.topics) = extract_problem_set(toks)
    print "course number = ", self.course_number
    print "title = ", self.title
    print "label = ", self.label
    print "parents = ", self.parents
    print "instance = ", self.instance
    print "folder = ", self.folder
    print "points = ", self.points
    print "prompt = ", self.prompt
    print "topics = ", self.topics            
    
    self.contents = tokens.get_contents(toks) 

  def mk_label (self):
    return self.label    

  def to_string (self): 
    if self.contents:
      contents = self.contents.strip() + NEWLINE
    else:
      contents = ''

    result = NEWLINE + \
      dex.mk_str_begin(dex.PROBLEM_SET) +  NEWLINE + \
      dex.mk_str_course(self.course_number)  + NEWLINE + \
      dex.mk_str_instance(self.instance)  + NEWLINE + \
      dex.mk_str_folder(self.folder)  + NEWLINE + \
      dex.mk_str_title_noarg(self.title)  + NEWLINE + \
      dex.mk_str_label_noarg(self.label)  + NEWLINE + \
      dex.mk_str_points(self.points)  + NEWLINE + \
      dex.mk_str_topics(self.topics)  + NEWLINE + \
      dex.mk_str_prompt(self.prompt)  + NEWLINE + \
      contents + \
      dex.mk_str_end(dex.PROBLEM_SET)

    return result


  def to_mlx_string (self):
    contents = self.contents.strip()
    print 'problemset.to_mlx_string: title: ', self.title

    fields = mk_mlx_str_fields_problem_set(self)
    r = mlx.mk_str_problem_set(fields)
    return NEWLINE + r


## Assignment
class Assignment:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.title = toks[1]
    self.duedate = tokens.get_duedate(toks)
    self.contents = tokens.get_contents(toks)

  def mk_label (self):
    return self.label


  def to_string (self):
    result = dex.mk_str_begin(dex.ASSIGNMENT) + NEWLINE + \
             dex.mk_str_title(self.title) + NEWLINE + \
             dex.mk_str_label(self.label)  + NEWLINE + \
             dex.mk_str_parents (self.parents) + NEWLINE + \
             dex.mk_str_duedate (self.duedate) + NEWLINE + \
             self.contents + NEWLINE + \
             dex.mk_str_end(dex.ASSIGNMENT) + NEWLINE
    return NEWLINE + result

  def to_mlx_string (self):
    contents = self.contents.strip()

    fields = mk_mlx_str_fields_common(self, False)
    duedate = mlx.mk_str_duedate (self.duedate.strip())
    fields.extend([duedate, contents])
    r = mlx.mk_str_assignment(fields)
    return NEWLINE + r


## Section
class Section:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.intro = tokens.get_intro(toks)
    self.contents = tokens.get_contents(toks) 

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = self.intro + NEWLINE + self.contents
    result = mk_str_generic (dex.SECTION, self.title, self.label, self.parents, contents)
    return result

  def to_mlx_string (self):
    (intro_html, intro_dex) = mk_mlx_intros (self.index, self.intro)

    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([intro_html, intro_dex, self.contents])
    r = mlx.mk_str_section(fields)
    return NEWLINE + r

## Subsection
class Subsection:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.contents = tokens.get_contents(toks)
    self.checkpoint = tokens.get_checkpoint(toks) 
#    print 'subsection.constructor: checkpoint:', self.checkpoint

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = self.contents + NEWLINE + self.checkpoint
    result = mk_str_generic (dex.SUBSECTION, self.title, self.label, self.parents, contents)
    return result

  def to_mlx_string (self):
    contents = self.contents + NEWLINE + self.checkpoint
    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([contents])
    r = mlx.mk_str_subsection(fields)
    return NEWLINE + r

## Subsubsection
class Subsubsection:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.contents = tokens.get_contents(toks)
    self.checkpoint = tokens.get_checkpoint(toks) 
#    print 'subsection.constructor: checkpoint:', self.checkpoint

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = self.contents + NEWLINE + self.checkpoint
    result = mk_str_generic (dex.SUBSUBSECTION, self.title, self.label, self.parents, contents)
    return result

  def to_mlx_string (self):
    contents = self.contents + NEWLINE + self.checkpoint
    fields = mk_mlx_str_fields_common(self, False)
    fields.extend([contents])
    r = mlx.mk_str_subsubsection(fields)
    return NEWLINE + r


## Atom
class Atom:

  def __init__(self, name, translate_to_html, toks):
    self.translate_to_html = translate_to_html
    self.index = new_index ()
    self.name = name
    (self.title, self.label, self.parents) = extract_common(toks)
    self.contents =  tokens.get_body(toks) 

  def mk_label (self): 
    return self.label

  def to_string (self): 
    result = mk_str_generic (self.name, self.title, self.label, self.parents, self.contents)
    return result

  def to_mlx_string (self, atom_name_mlx):
    contents =  self.contents
    if self.translate_to_html: 
      (contents_html, contents_dex) = mk_mlx_bodies(self.index,  contents)
    else:
      contents_dex = mlx.mk_str_body_dex(contents)
      contents_html = mlx.mk_str_body(contents)


    fields = mk_mlx_str_fields_common(self, True)
    fields.extend([contents_html, contents_dex])
    r = mlx.mk_str_block_atom(atom_name_mlx, fields)
    return NEWLINE + r

## Algo(rithm)
class Algo:

  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    # contents is a list of lines
    self.contents =  tokens.get_body(toks)

  # Treat it as an atom for labels purposes
  def mk_label (self): 
    return self.label

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
    result = mk_str_generic (dex.ALGO, self.title, self.label, self.parents, contents)
    return result
    
  def to_mlx_string (self):
    contents = NEWLINE.join(self.contents)
    (title_html, title_dex) = mk_mlx_titles(self.index, self.title)
    (contents_html, contents_dex) = mk_mlx_bodies(self.index, contents)
    fields = mk_mlx_str_fields_common(self, True)
    fields.extend([contents_html, contents_dex])
    r = mlx.mk_str_atom(mlx.ALGORITHM, fields)
    return NEWLINE + r


## Answers
class Answer:
  def __init__(self, toks):
    self.index = new_index ()
    (self.body, self.explain, self.label, self.points, self.title) = extract_common_solution(toks)

  def mk_label (self): 
    return self.label

  def to_string (self): 
    print "Answer.to_string"
    result = mk_str_generic_solution (dex.ANSWER, self)
    print 'answer:', result
    return result

  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_solution(self, True)
    r = mlx.mk_str_answer(fields)
    return NEWLINE + r

## Choices
class Choice:
  def __init__(self, toks):
    self.index = new_index ()
    (self.body, self.explain, self.label, self.points, self.title) = extract_common_solution(toks)

  def mk_label (self): 
    return self.label

  def to_string (self): 
    print "Choice.to_string"
    result = mk_str_generic_solution (dex.CHOICE, self)
    print 'choice:', result
    return result

  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_solution(self, True)
    r = mlx.mk_str_choice(fields)
    return NEWLINE + r

## Selects
class Select:
  def __init__(self, toks):
    self.index = new_index ()
    (self.body, self.explain, self.label, self.points, self.title) = extract_common_solution(toks)

  def mk_label (self): 
    return self.label

  def to_string (self): 
    print "Select.to_string"
    result = mk_str_generic_solution (dex.SELECT, self)
    print 'select:', result
    return result


  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_solution(self, True)
    r = mlx.mk_str_select(fields)
    return NEWLINE + r



## Problem_FR
class ProblemFR:
  def __init__(self, toks):
    self.index = new_index ()
    (self.hint, self.label, self.points, self.prompt, self.title, self.topics) = extract_common_problem(toks)
    self.answers = tokens.get_answers(toks) 
#    print 'problemfr: answers:', self.answers

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.answers

    result = mk_str_generic_problem (dex.PROBLEM_FR, self.title, self.label, contents)
    return result


  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_problem(self, True)
    r = mlx.mk_str_problem_fr(fields)

    return  NEWLINE + r


## Problem_ma
## TODO THIS NEEDS WORK CURRENTL COPY OF MC
class ProblemMA:
  def __init__(self, toks):
    self.index = new_index ()
    (self.hint, self.label, self.points, self.prompt, self.title, self.topics) = extract_common_problem(toks)
    self.selects = tokens.get_selects(toks) 
#    print 'problemmc: self.selects', self.selects

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.selects


    result = mk_str_generic_problem (dex.PROBLEM_MA, self.title, self.label, contents)
    return result


  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_problem(self, True)
    r = mlx.mk_str_problem_ma(fields)

    return NEWLINE + r

## Problem_Mc
class ProblemMC:
  def __init__(self, toks):
    self.index = new_index ()
    (self.hint, self.label, self.points, self.prompt, self.title, self.topics) = extract_common_problem(toks)
    self.choices = tokens.get_choices(toks) 
#    print 'problemmc: self.choices', self.choices

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.choices

    result = mk_str_generic_problem (dex.PROBLEM_MC, self.title, self.label, contents)
    return result


  def to_mlx_string (self): 
    fields = mk_mlx_str_fields_problem(self, True)
    r = mlx.mk_str_problem_mc(fields)

    return NEWLINE + r

## Question_FR
class QuestionFR:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
# simple answer are converted to answer blocks.
#    self.ans = tokens.get_ans(toks)
#    self.explain = tokens.get_explain(toks) 
    self.answers = tokens.get_answers(toks) 
#    print 'questionfr: answers:', self.answers

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.answers

    result = mk_str_generic_problem (dex.QUESTION_FR, self.title, self.label, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.index, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.index, self.hint)

    # ### BEGIN :DELETE THIS
    # # explain
    # (explain_html, explain_dex) = mk_mlx_explains(self.index, self.explain)

    # # solution - @umut - I changed self.solution here to self.ans for the code to compile
    # field_solution_dex = mlx.mk_str_solution_dex(self.ans)  
    # field_solution = mlx.mk_str_solution(Tex2Html.translate(self.unique+os_utils.SOLUTION_EXTENSION, self.ans, False))

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
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'questionmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.selects = tokens.get_selects(toks) 
#    print 'questionmc: self.selects', self.selects

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.selects


    result = mk_str_generic (dex.QUESTION_MA, self.title, self.label, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.index, self.prompt)

    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.index, self.hint)


    fields.extend([points, prompt_html, prompt_dex, \
                   hint_html, hint_dex, \
                   self.selects])

    # make the block
    r = mlx.mk_str_question_ma(fields)

    return NEWLINE + r

## Question_Mc
class QuestionMC:
  def __init__(self, toks):
    self.index = new_index ()
    (self.title, self.label, self.parents) = extract_common(toks)
    self.points = tokens.get_points(toks) 
#    print 'questionmc: self.points', self.points
    self.prompt = tokens.get_prompt(toks)
    self.hint = tokens.get_hint(toks) 
    self.choices = tokens.get_choices(toks) 
#    print 'questionmc: self.choices', self.choices

  def mk_label (self): 
    return self.label

  def to_string (self): 
    contents = dex.mk_str_points(self.points) + NEWLINE + \
               dex.mk_str_prompt(self.prompt) + NEWLINE + \
               dex.mk_str_hint(self.hint) + NEWLINE + \
               self.choices

    result = mk_str_generic (dex.QUESTION_MC, self.title, self.label, self.parents, contents)
    return result


  def to_mlx_string (self): 
    # Common fields
    fields = mk_mlx_str_fields_common(self, True)

    # points
    points = mlx.mk_str_points(self.points)

    # prompt
    (prompt_html, prompt_dex) = mk_mlx_prompts(self.index, self.prompt)
    
    # hint
    (hint_html, hint_dex) = mk_mlx_hints(self.index, self.hint)

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

def atom_to_string(name, toks): 
#  print 'atom to string: toks = ', toks 

  if name == dex.PARAGRAPH_HTML:
    translate_to_html = False
  else: 
    translate_to_html = True

  block = Atom(name, translate_to_html, toks)
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

def subsection_to_string(toks): 
  block = Subsection(toks)
  print '    matched subsection', '[', block.title, '].'
  return block.to_string()

def subsubsection_to_string(toks): 
  block = Subsubsection(toks)
  print '    matched subsubsection', '[', block.title, '].'
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

def problem_group_to_string(toks): 
  print '      matched problem_group.'
  block = ProblemGroup(toks)
  return block.to_string()

def problem_set_to_string(toks): 
  print '      matched problem_set.'
  block = ProblemSet(toks)
  return block.to_string()

def select_to_string(toks): 
  print '          matched select.'
  block = Select(toks)
  return block.to_string()

## END: string converters
######################################################################


