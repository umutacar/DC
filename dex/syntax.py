######################################################################
## dex/syntax.py
######################################################################

from pervasives.syntax import *


# Commands
COM_ANSWER = r'\answer'
COM_AUTHORS = r'\authors'
COM_BODY = r'\body'
COM_COURSE_NUMBER = r'\coursenumber'
COM_DOCUMENT_CLASS = r'\documentclass{course}'
COM_CHOICE = r'\choice'
COM_CHOICE_S = r'\choice*'
COM_COURSE = BACKSLASH + r'course'
COM_DUEDATE = r'\duedate'
COM_EXPLAIN = r'\explain'
COM_FOLDER = BACKSLASH + r'folder'
COM_HINT = r'\hint'
COM_INFO = r'\info'
COM_INSTANCE = BACKSLASH + r'instance'
COM_NO = r'\no'
COM_PARENTS = r'\parents'
COM_PICTURE = r'\picture'
COM_POINTS = r'\points'
COM_PROMPT = r'\prompt'
COM_PROVIDES_ASSIGNMENT = r'\providesassignment'
COM_PROVIDES_BOOK = r'\providesbook' 
COM_PROVIDES_CHAPTER = r'\provideschapter' 
COM_PROVIDES_SECTION = r'\providessection' 
COM_PROVIDES_SUBSECTION = r'\providessubsection' 
COM_SELECT = r'\select'
COM_SELECT_S = r'\select*'
COM_SEMESTER = r'\semester'
COM_SOLUTION = r'\solution'
COM_WEBSITE = r'\website'
COM_TITLE = r'\title'
COM_TOPICS = BACKSLASH + r'topics'



# block strings
ABOUT = r'about'
ASSIGNMENT = r'assignment'
ASSTPROBLEM = r'asstproblem'
BOOK = r'book'
CHAPTER = r'chapter'
CHECKPOINT = r'checkpoint'
CHOICE = r'choice'
GROUP = r'group'

#QUESTION_FR = r'questionfr'
#QUESTION_MA = r'questionma'
#QUESTION_MC = r'questionmc'

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
PREAMBLE = r'preamble'
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



def mk_str_ans(x):
  x = x.strip()
  result = COM_ANSWER + NEWLINE + x 
  return result

def mk_str_authors(authors):
  result = COM_AUTHORS + mk_str_arg(authors)
  return result 

def mk_str_body_noarg (body):
  result = COM_BODY + SPACE + body
  return result 


def mk_str_choices(xs): 
  ys = map(lambda x: str(x[0]).strip() + ' ' + str(x[1]).strip(), xs)  
  result = '\n'.join(ys)
  return result

def mk_str_course(number):
  result = COM_COURSE + SPACE + number 
  return result 

def mk_str_course_number(number):
  result = COM_COURSE_NUMBER + mk_str_arg(number) 
  return result 

def mk_str_document_class():
  result = COM_DOCUMENT_CLASS
  return result 

def mk_str_duedate(duedate):
  result = COM_DUEDATE + mk_str_arg(duedate)
  return result

def mk_str_explain(x): 
  x = x.strip()
  result = COM_EXPLAIN + NEWLINE + x
  return result

def mk_str_folder(x): 
  x = x.strip()
  result = COM_FOLDER + SPACE + x
  return result

def mk_str_hint(x): 
  x = x.strip()
  result = COM_HINT + NEWLINE + x
  return result

def mk_str_info(x):
  x = x.strip()
  result = COM_INFO + NEWLINE + x
  return result

def mk_str_instance(x):
  x = x.strip()
  result = COM_INSTANCE + SPACE + x
  return result

def mk_str_label(label):
  result = COM_LABEL + mk_str_arg(label)
  return result

def mk_str_label_noarg(label):
  print "label:", label
  result = COM_LABEL + SPACE + label
  return result

def mk_str_no(x): 
  result = COM_NO + mk_str_arg(x)
  return result

def mk_str_parent(x): 
  result = COM_PARENT + mk_str_arg(x)
  return result

def mk_str_parent_noarg(x): 
  result = COM_PARENT + SPACE + x
  return result

def mk_str_parents(x): 
  y = map(lambda p: mk_str_parent(p), x)
  result = '\n'.join(y)
#  if result == '':
#    result = mk_str_parent(KW_NO_PARENTS)

  return result

def mk_str_parents_noarg (x): 
  y = map(lambda p: mk_str_parent_noarg(p), x)
  result = '\n'.join(y)
#  if result == '':
#    result = mk_str_parent(KW_NO_PARENTS)

  return result

def mk_str_picture(picture):
  result = COM_PICTURE + mk_str_arg(picture)
  return result 

def mk_str_provides_book(x):
  result = COM_PROVIDES_BOOK + mk_str_arg(x)
  return result 

def mk_str_provides_chapter(x):
  result = COM_PROVIDES_CHAPTER + mk_str_arg(x)
  return result 

def mk_str_provides_section(x):
  result = COM_PROVIDES_SECTION + mk_str_arg(x)
  return result 

def mk_str_provides_subsection(x):
  result = COM_PROVIDES_SUBSECTION + mk_str_arg(x)
  return result

def mk_str_provides_assignment(x):
  result = COM_PROVIDES_ASSIGNMENT + mk_str_arg(x)
  return result 

def mk_str_semester(semester):
  result = COM_SEMESTER + mk_str_arg(semester)
  return result 

def mk_str_website(website):
  result = COM_WEBSITE + mk_str_arg(website)
  return result 

def mk_str_points(x): 
  x = x.strip()
  print "points:", x, "!"
  result = COM_POINTS + SPACE + x
  return result

def mk_str_prompt(x):
  x = x.strip()
  result = COM_PROMPT + NEWLINE + x 
  return result

def mk_str_solution(x):
  x = x.strip()
  result = COM_SOLUTION + NEWLINE + x 
  return result

def mk_str_title(title):
  result = COM_TITLE + mk_str_arg(title)
  return result

def mk_str_title_noarg(title):
  result = COM_TITLE + SPACE + title
  return result

def mk_str_topics(topics):
  result = COM_TOPICS + SPACE + topics
  return result 

def mk_str_opt_arg_title(title):
  result = mk_str_opt_arg(title) 
  return result 
