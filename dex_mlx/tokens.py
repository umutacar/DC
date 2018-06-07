from pervasives.syntax import *

######################################################################
## BEGIN: KEYS

## these are used locally for name generation in the parser 
KEY_ANS = 'KEY_ANS'
KEY_ANSWER = 'KEY_ANSWER'
KEY_ANSWERS = 'KEY_ANSWERS'
KEY_ASSIGNMENT = 'KEY_ASSIGNMENT'
KEY_ASSTPROBLEM = 'KEY_ASSTPROBLEM'
KEY_ATOM = 'KEY_ATOM'
KEY_AUTHOR = 'KEY_AUTHOR'
KEY_AUTHORS = 'KEY_AUTHORS'
KEY_BEGIN = 'KEY_BEGIN'
KEY_BODY = 'KEY_BODY'
KEY_BODY_DEX = 'KEY_BODY_DEX'
KEY_BODY_POP = 'KEY_BODY_POP'
KEY_BOOK = 'KEY_BOOK'
KEY_CHAPTER = 'KEY_CHAPTER'
KEY_CHOI = 'KEY_CHOI'
KEY_CHOICE = 'KEY_CHOICE'
#KEY_CHOICE_T = 'KEY_CHOICE_T'
KEY_CHOICES = 'KEY_CHOICES'
KEY_CHOICES_DEX = 'KEY_CHOICES_DEX'
KEY_CONTENTS = 'KEY_CONTENTS'
KEY_CONTENTS_DEX = 'KEY_CONTENTS_DEX'
KEY_CORRECTNESS = 'KEY_CORRECTNESS'
KEY_COURSE = 'KEY_COURSE'
KEY_COURSE_NUMBER = 'KEY_COURSE_NUMBER'
KEY_DUEDATE = 'KEY_DUEDATE'
KEY_END = 'KEY_END_OF_SOMETHING'
KEY_EXPLAIN = 'KEY_EXPLAIN'
KEY_EXPLAIN_DEX = 'KEY_EXPLAIN_DEX'
KEY_FOLDER = 'KEY_FOLDER'
KEY_FRIENDS = 'KEY_FRIENDS'
KEY_GROUP = 'KEY_GROUP'
KEY_GROUP_CONTENTS = 'KEY_GROUP_CONTENTS'
KEY_HINT = 'KEY_HINT'
KEY_HINT_DEX = 'KEY_HINT_DEX'
KEY_INFO = 'KEY_INFO'
KEY_INSTANCE = 'KEY_INSTANCE'
KEY_INTRO = 'KEY_INTRO'
KEY_INTRO_DEX = 'KEY_INTRO_DEX'
KEY_JOKER = 'KEY_JOKER'
KEY_LABEL = 'KEY_LABEL'
KEY_MODULE = 'KEY_MODULE'
KEY_NO = 'KEY_NO'
KEY_PARENTS = 'KEY_PARENTS'
KEY_PICTURE = 'KEY_PICTURE'
KEY_POINTS = 'KEY_POINTS'
KEY_PROBLEM = 'KEY_PROBLEM'
KEY_PROBLEMSET = 'KEY_PROBLEM_SET'
KEY_QUESTION = 'KEY_QUESTION'
KEY_PROMPT = 'KEY_PROMPT'
KEY_PROMPT_DEX = 'KEY_PROMPT_DEX'
KEY_PROVIDES_ASSIGNMENT = 'KEY_PROVIDES_ASSIGNMENT'
KEY_PROVIDES_BOOK = 'KEY_PROVIDES_BOOK'
KEY_PROVIDES_CHAPTER = 'KEY_PROVIDES_CHAPTER'
KEY_PROVIDES_SECTION = 'KEY_PROVIDES_SECTION'
KEY_PROVIDES_UNIT = 'KEY_PROVIDES_UNIT'
KEY_CHECKPOINT = 'KEY_CHECKPOINT'
KEY_SECTION = 'KEY_SECTION'
KEY_SEL = 'KEY_SEL'
KEY_SELECT = 'KEY_SELECT'
KEY_SELECTS = 'KEY_SELECTS'
KEY_SEMESTER = 'KEY_SEMESTER'
KEY_SOLUTION = 'KEY_SOLUTION'
KEY_SOLUTION_DEX = 'KEY_SOLUTION_DEX'
KEY_TITLE = 'KEY_TITLE'
KEY_TOPICS = 'KEY_TOPICS'
KEY_UNIT = 'KEY_UNIT'
KEY_UNIQUE = 'KEY_UNIQUE'
KEY_WEBSITE = 'KEY_WEBSITE'

## END: KEYS
######################################################################


######################################################################
## BEGIN: ParserResult Key Setters

def set_key_ans(parser):
  parser = parser.setName(KEY_ANS).setResultsName(KEY_ANS)
  return parser

def set_key_answer(parser):
  parser = parser.setName(KEY_ANSWER).setResultsName(KEY_ANSWER)
  return parser

def set_key_answers(parser):
  parser = parser.setName(KEY_ANSWERS).setResultsName(KEY_ANSWERS)
  return parser

def set_key_assignment(parser):
  parser = parser.setName(KEY_ASSIGNMENT).setResultsName(KEY_ASSIGNMENT)
  return parser

def set_key_asstproblem(parser):
  parser = parser.setName(KEY_ASSTPROBLEM).setResultsName(KEY_ASSTPROBLEM)
  return parser

def set_key_atom(parser, atom_name):
  parser = parser.setName(KEY_KEY + atom_name).setResultsName(KEY_KEY + atom_name)
  return parser

def set_key_authors(parser):
  parser = parser.setName(KEY_AUTHORS).setResultsName(KEY_AUTHORS)
  return parser

def set_key_begin(parser):
  parser = parser.setName(KEY_BEGIN).setResultsName(KEY_BEGIN)
  return parser

def set_key_body(parser):
  parser = parser.setName(KEY_BODY).setResultsName(KEY_BODY)
  return parser

def set_key_body_dex(parser):
  parser = parser.setName(KEY_BODY_DEX).setResultsName(KEY_BODY_DEX)
  return parser

def set_key_body_pop(parser):
  parser = parser.setName(KEY_BODY_POP).setResultsName(KEY_BODY_POP)
  return parser

def set_key_book(parser):
  parser = parser.setName(KEY_BOOK).setResultsName(KEY_BOOK)
  return parser

def set_key_chapter(parser):
  parser = parser.setName(KEY_CHAPTER).setResultsName(KEY_CHAPTER)
  return parser

def set_key_choi(parser):
  parser = parser.setName(KEY_CHOI).setResultsName(KEY_CHOI)
  return parser

def set_key_choice(parser):
  parser = parser.setName(KEY_CHOICE).setResultsName(KEY_CHOICE)
  return parser

def set_key_choice_t(parser):
  parser = parser.setName(KEY_CHOICE_T).setResultsName(KEY_CHOICE_T)
  return parser

def set_key_choices(parser):
  parser = parser.setName(KEY_CHOICES).setResultsName(KEY_CHOICES)
  return parser

def set_key_choices_dex(parser):
  parser = parser.setName(KEY_CHOICES_DEX).setResultsName(KEY_CHOICES_DEX)
  return parser

def set_key_contents(parser):
  parser = parser.setName(KEY_CONTENTS).setResultsName(KEY_CONTENTS)
  return parser

def set_key_correctness(parser):
  parser = parser.setName(KEY_CORRECTNESS).setResultsName(KEY_CORRECTNESS)
  return parser

def set_key_course(parser):
  parser = parser.setName(KEY_COURSE).setResultsName(KEY_COURSE)
  return parser

def set_key_course_number(parser):
  parser = parser.setName(KEY_COURSE_NUMBER).setResultsName(KEY_COURSE_NUMBER)
  return parser

def set_key_duedate(parser):
  parser = parser.setName(KEY_DUEDATE).setResultsName(KEY_DUEDATE)
  return parser

def set_key_end(parser):
  parser = parser.setName(KEY_END).setResultsName(KEY_END)
  return parser

def set_key_explain(parser):
  parser = parser.setName(KEY_EXPLAIN).setResultsName(KEY_EXPLAIN)
  return parser

def set_key_explain_dex(parser):
  parser = parser.setName(KEY_EXPLAIN_DEX).setResultsName(KEY_EXPLAIN_DEX)
  return parser

def set_key_folder(parser):
  parser = parser.setName(KEY_FOLDER).setResultsName(KEY_FOLDER)
  return parser

def set_key_friends(parser):
  parser = parser.setName(KEY_FRIENDS).setResultsName(KEY_FRIENDS)
  return parser

def set_key_group(parser):
  parser = parser.setName(KEY_GROUP).setResultsName(KEY_GROUP)
  return parser

def set_key_group_contents(parser):
  parser = parser.setName(KEY_GROUP_CONTENTS).setResultsName(KEY_GROUP_CONTENTS)
  return parser

def set_key_hint(parser):
  parser = parser.setName(KEY_HINT).setResultsName(KEY_HINT)
  return parser

def set_key_hint_dex(parser):
  parser = parser.setName(KEY_HINT_DEX).setResultsName(KEY_HINT_DEX)
  return parser

def set_key_joker(parser):
  parser = parser.setName(KEY_JOKER).setResultsName(KEY_JOKER)
  return parser

def set_key_info(parser):
  parser = parser.setName(KEY_INFO).setResultsName(KEY_INFO)
  return parser

def set_key_instance(parser):
  parser = parser.setName(KEY_INSTANCE).setResultsName(KEY_INSTANCE)
  return parser

def set_key_intro(parser):
  parser = parser.setName(KEY_INTRO).setResultsName(KEY_INTRO)
  return parser

def set_key_intro_dex(parser):
  parser = parser.setName(KEY_INTRO_DEX).setResultsName(KEY_INTRO_DEX)
  return parser

def set_key_label(parser):
  parser = parser.setName(KEY_LABEL).setResultsName(KEY_LABEL)
  return parser

def set_key_module(parser):
  parser = parser.setName(KEY_MODULE).setResultsName(KEY_MODULE)
  return parser

def set_key_no(parser):
  parser = parser.setName(KEY_NO).setResultsName(KEY_NO)
  return parser

def set_key_parents(parser):
  parser = parser.setName(KEY_PARENTS).setResultsName(KEY_PARENTS)
  return parser

def set_key_picture(parser):
  parser = parser.setName(KEY_PICTURE).setResultsName(KEY_PICTURE)
  return parser

def set_key_points(parser):
  parser = parser.setName(KEY_POINTS).setResultsName(KEY_POINTS)
  return parser

def set_key_problem(parser):
  parser = parser.setName(KEY_PROBLEM).setResultsName(KEY_PROBLEM)
  return parser

def set_key_problemset(parser):
  parser = parser.setName(KEY_PROBLEMSET).setResultsName(KEY_PROBLEMSET)
  return parser

def set_key_provides_book(parser):
  parser = parser.setName(KEY_PROVIDES_BOOK).setResultsName(KEY_PROVIDES_BOOK)
  return parser

def set_key_provides_chapter(parser):
  parser = parser.setName(KEY_PROVIDES_CHAPTER).setResultsName(KEY_PROVIDES_CHAPTER)
  return parser

def set_key_provides_section(parser):
  parser = parser.setName(KEY_PROVIDES_SECTION).setResultsName(KEY_PROVIDES_SECTION)
  return parser

def set_key_provides_unit(parser):
  parser = parser.setName(KEY_PROVIDES_UNIT).setResultsName(KEY_PROVIDES_UNIT)
  return parser

def set_key_provides_assignment(parser):
  parser = parser.setName(KEY_PROVIDES_ASSIGNMENT).setResultsName(KEY_PROVIDES_ASSIGNMENT)
  return parser

def set_key_question(parser):
  parser = parser.setName(KEY_QUESTION).setResultsName(KEY_QUESTION)
  return parser

def set_key_prompt(parser):
  parser = parser.setName(KEY_PROMPT).setResultsName(KEY_PROMPT)
  return parser

def set_key_prompt_dex(parser):
  parser = parser.setName(KEY_PROMPT_DEX).setResultsName(KEY_PROMPT_DEX)
  return parser

def set_key_checkpoint(parser):
  parser = parser.setName(KEY_CHECKPOINT).setResultsName(KEY_CHECKPOINT)
  return parser

def set_key_section(parser):
  parser = parser.setName(KEY_SECTION).setResultsName(KEY_SECTION)
  return parser

def set_key_sel(parser):
  parser = parser.setName(KEY_SEL).setResultsName(KEY_SEL)
  return parser

def set_key_select(parser):
  parser = parser.setName(KEY_SELECT).setResultsName(KEY_SELECT)
  return parser

def set_key_selects(parser):
  parser = parser.setName(KEY_SELECTS).setResultsName(KEY_SELECTS)
  return parser

def set_key_semester(parser):
  parser = parser.setName(KEY_SEMESTER).setResultsName(KEY_SEMESTER)
  return parser

def set_key_solution(parser):
  parser = parser.setName(KEY_SOLUTION).setResultsName(KEY_SOLUTION)
  return parser

def set_key_solution_dex(parser):
  parser = parser.setName(KEY_SOLUTION_DEX).setResultsName(KEY_SOLUTION_DEX)
  return parser

def set_key_title(parser):
  parser = parser.setName(KEY_TITLE).setResultsName(KEY_TITLE)
  return parser

def set_key_topics(parser):
  parser = parser.setName(KEY_TOPICS).setResultsName(KEY_TOPICS)
  return parser

def set_key_unique(parser):
  parser = parser.setName(KEY_UNIQUE).setResultsName(KEY_UNIQUE)
  return parser

def set_key_unit(parser):
  parser = parser.setName(KEY_UNIT).setResultsName(KEY_UNIT)
  return parser

def set_key_website(parser):
  parser = parser.setName(KEY_WEBSITE).setResultsName(KEY_WEBSITE)
  return parser

## END: Field Setters
######################################################################



######################################################################
## BEGIN: Token Extractors
##

# small ans(wer)

def get_ans(toks):
  try: 
    result = toks[KEY_ANS]
  except KeyError:
    result = KW_NO_ANS
  return result


def get_answer(toks):
  try: 
    result = toks[KEY_ANSWER]
  except KeyError:
    result = KW_NO_ANSWER
  return result

# small choice
def get_choi(toks):
  try: 
    result = toks[KEY_CHOI]
  except KeyError:
    result = KW_NO_CHOI
  return result

def get_choice(toks):
  try: 
    result = toks[KEY_CHOICE]
  except KeyError:
    result = KW_NO_CHOICE
  return result

def get_answers(toks):
#  print 'get_answers: toks = ', toks
  try: 
    result = toks[KEY_ANSWERS]
  except KeyError:
    result = KW_NO_ANSWERS
  return result

def get_assignment(toks):
  return toks.get(KEY_ASSIGNMENT)

def get_authors(toks):  
  return toks[KEY_AUTHORS]

def get_begin(toks):  
  return toks[KEY_BEGIN]

def get_body(toks):
  try:
    result = toks[KEY_BODY]
  except KeyError:
    result = KW_NO_ANSWER
  return  result

def get_book(toks):  
  return toks[KEY_BOOK]

def get_choices(toks):
  result = toks[KEY_CHOICES]
  return result

def get_correctness(toks):  
  return toks[KEY_CORRECTNESS]

def get_course_number(toks):
  return toks[KEY_COURSE_NUMBER]

def get_contents(toks):
#  print 'get_contents: toks = ', toks
  try:
    result = toks[KEY_CONTENTS]
  except KeyError:
    result = ''
#  print 'get_contents: result = ', result
  return result

def get_duedate(toks):
  return toks[KEY_DUEDATE]

def get_group_contents(toks):
  try:
    result = toks[KEY_GROUP_CONTENTS]
  except KeyError:
    result = None
  return result

def get_end(toks):
  return toks[KEY_END]

def get_explain(toks):
  try:
    result = toks[KEY_EXPLAIN]
  except KeyError:
    result = KW_NO_EXPLAIN
  return result

def get_folder(toks):
  try:
    result = toks[KEY_FOLDER]
  except KeyError:
    result = KW_NO_FOLDER
  return result

def get_group(toks):
  return toks[KEY_GROUP]

def get_hint(toks):
  try:
    result = toks[KEY_HINT]
  except KeyError:
    result = KW_NO_HINT
  return result

def get_info(toks):
  try:
    result = toks[KEY_INFO]
  except KeyError:
    result = KW_NO_INFO
  return result

def get_instance(toks):
  try:
    result = toks[KEY_INSTANCE]
  except KeyError:
    result = KW_NO_INSTANCE
  return result

def get_intro(toks):
  try:
    result = toks[KEY_INTRO]
  except KeyError:
    result = KW_NO_INTRO
  return result

def get_joker(toks):
  try:
    result = toks[KEY_JOKER]
  except KeyError:
    result = ''
  return result


# I don't know why I need the 0 here.
# One possibility: if it may or may not exists, then you get a conditional.
# if none found return None.
def get_label(toks):
  try:
    label = toks[KEY_LABEL][0]
  except KeyError:
    # label = KW_UNLABELED
    label = None
  return label

# I don't know why I need the 0 here.
# One possibility: if it may or may not exists, then you get a conditional.
# if none found return None.
def get_no(toks):
  try: 
    no = toks[KEY_NO][0]
  except KeyError:
    no = KW_NO_NO
  return no

## DO NOT DELETE THIS
# returns parents as a comma separeted list
# this is very buggy because pyparsing may
# return parents as a list or not as a list, e.g., if there
# is just one parent.  in this case
# python treats the string NO_PARENTS as a list.
#
# This is intellectually unjustifiable stuff and 
# a reason for why programs should be written in strongly typed 
# languages.
#
# We don't need  this stuff for Spring 2018.  So 
# I am going to treat them an nonexisting.
def get_parents(toks):
  try:
    parents = toks[KEY_PARENTS]
  except KeyError:
    parents = [KW_NO_PARENTS] 

#  print 'get_parents: parents = ', parents
  return parents

def get_picture(toks):
  try:
    result = toks[KEY_PICTURE][0]
  except KeyError:
    result = KW_NO_PICTURE
  return result

def get_points(toks):
  result = toks[KEY_POINTS]
  return result

def get_points_opt(toks):
  try:
    result = toks[KEY_POINTS][0]
  except KeyError:
    result = KW_NO_POINTS  # this should return 0
  return result

def get_provides_book(toks):
  result = toks[KEY_PROVIDES_BOOK][0]
  return result

def get_provides_chapter(toks):
  try:
    result = toks[KEY_PROVIDES_CHAPTER][0]
  except KeyError:
    result = KW_NO_CHAPTER_PROVISION
  return result

def get_provides_section(toks):
  try:
    result = toks[KEY_PROVIDES_SECTION][0]
  except KeyError:
    result = KW_NO_SECTION_PROVISION
  return result

def get_provides_unit(toks):
  try:
    result = toks[KEY_PROVIDES_UNIT][0]
  except KeyError:
    result = KW_NO_UNIT_PROVISION
  return result

def get_provides_assignment(toks):
  try:
    result = toks[KEY_PROVIDES_ASSIGNMENT][0]
  except KeyError:
    result = KW_NO_ASSIGNMENT_PROVISION
  return result

def get_prompt(toks):
  result = toks[KEY_PROMPT]
  return result

def get_topics(toks):
  try:
    result = toks[KEY_TOPICS]
  except KeyError:
    result = KW_NO_TOPICS
  return result


def get_checkpoint(toks):
  try:
    result = toks[KEY_CHECKPOINT][0]
  except KeyError:
    result = ''
#    result = KW_NO_CHECKPOINT
  return result

# small select
def get_sel(toks):
  try: 
    result = toks[KEY_SEL]
  except KeyError:
    result = KW_NO_SEL
  return result

def get_select(toks):
  try: 
    result = toks[KEY_SELECT]
  except KeyError:
    result = KW_NO_SELECT
  return result

def get_selects(toks):
  result = toks[KEY_SELECTS]
  return result


# def get_parents_with_flag(toks):
#   parents_flag = True
#   try:
#     parents = toks[KEY_PARENTS]
#   except KeyError:
#     parents = KW_NO_PARENTS
# #    print 'no parents found'
#     parents_flag = False

# #  print 'parents_flag', parents_flag

#   return(parents, parents_flag)
def get_semester(toks):
  try:
    result = toks[KEY_SEMESTER][0]
  except KeyError:
    result = KW_NO_SEMESTER

  return result

def get_solution(toks):
  try: 
    result = toks[KEY_SOLUTION]
  except KeyError:
    result = KW_NO_SOLUTION
  return result


def get_unique(toks):
#  print 'get_unique: toks = ', toks
  try:
    unique = toks[KEY_UNIQUE][0]
  except KeyError:
    unique = KW_NO_UNIQUE
  return unique

def get_website(toks):
  try:
    result = toks[KEY_WEBSITE][0]
  except KeyError:
    result = KW_NO_WEBSITE
  return result

# I don't know why I need the 0 here.
def get_title(toks):
  try:
    title = toks[KEY_TITLE][0]
  except KeyError:
    title = KW_UNTITLED
  return title

def get_title_force(toks):
  try:
    title = toks[KEY_TITLE]
  except KeyError:
    title = KW_UNTITLED
  return title

## END: Field Extractors
######################################################################
