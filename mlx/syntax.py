######################################################################
## mlx/syntax.py
######################################################################

from pervasives.parser import *
 
CDATA_BEGIN = '<![CDATA['
CDATA_END = ']]>'
MISSING = '__missing__'


# Tags 
ATOM = 'atom'
BLOCK = 'block'
FIELD = 'field'

# Attribute names
NAME = 'name'
DATA_REFERENCE_TYPE = 'data-reference-type'

# Attribute values
DIDEROT_URL = '__diderot_url__'

# Block tags
ANSWER = 'answer'
ASSIGNMENT = 'assignment'
ASSTPROBLEM = 'asstproblem'
BOOK = 'book'
CHAPTER = 'chapter'
CHECKPOINT = 'checkpoint'
CHOICE = 'choice'
COURSE = 'course'
PROBLEM = r'problem'
PROBLEM_GROUP = r'problem_group'
PROBLEM_FR = r'problemfr'
PROBLEM_MA = r'problemma'
PROBLEM_MC = r'problemmc'
QUESTION_FR = r'questionfr'
QUESTION_MA = r'questionma'
QUESTION_MC = r'questionmc'
PROBLEM_SET= 'problem_set'
SECTION = 'section'
SELECT = 'select'
SUBSECTION = 'subsection'
SUBSUBSECTION = 'subsubsection'

# Attribute values
ALGO = r'algorithm'
ALGORITHM = r'algorithm'
AUTHORS = r'authors'
BODY = r'body'
BODY_SRC = r'body_src'
BODY_POP = r'body_pop'
CHOICE = r'choice'
CHOICE_SRC = r'choice_src'
CODE = r'code'
COROLLARY = r'corollary'
COST_SPEC = r'costspec'
COURSE_NUMBER = r'coursenumber'
DATASTR = r'datastr'
DATATYPE = r'datatype'
DEFINITION = r'definition'
DUEDATE = r'duedate'
EXAMPLE = r'example'
EXERCISE = r'exercise'
EXPLAIN = r'explain'
EXPLAIN_SRC = r'explain_src'
FOLDER = r'folder'
FRIENDS = r'friends'
GRAM = 'gram'
GROUP = r'group'
HINT = r'hint'
HINT = r'hint'
HINT_SRC = r'hint_src'
IMPORTANT = r'important'
INFO = r'info'
INFO_SRC = r'info_src'
INSTANCE = r'instance'
INTRO = r'intro'
INTRO_SRC = r'intro_src'
LABEL = r'label'
LEMMA = r'lemma'
NO = r'no'
NOTE = r'note'
ORDER = r'order'
PARAGRAPH = r'gram'
PARAGRAPH_HTML = r'html'
PARENTS = r'parents'
PICTURE = r'picture'
POINTS = r'points'
PREAMBLE = r'preamble'
PROBLEM = r'problem'
PROBLEM_SET = r'problem_set'
PROOF = r'proof'
PROPOSITION = r'proposition'
PROVIDES_BOOK = r'providesbook'
PROVIDES_CHAPTER = r'provideschapter'
PROVIDES_SECTION = r'providessection'
PROVIDES_SUBSECTION = r'providessubsection'
PROVIDES_ASSIGNMENT = r'providesassignment'
PROMPT = r'prompt'
PROMPT_SRC = r'prompt_src'
RANK = r'rank'
REMARK = r'remark'
SKIP = r'skip'
SEMESTER = r'semester'
SOLUTION = r'solution'
SOLUTION_SRC = r'solution_src'
SYNTAX = r'syntax'
TASK = r'task'
TEACH_ASK = r'teachask'
TEACH_NOTE = r'teachnote'
THEOREM = r'theorem'
TITLE = r'title'
TITLE_SRC = r'title_src'
TITLE_SRC = r'title_src'
TITLE_MISSING = r'untitled'
TOPICS = r'topics'
UNIQUE = r'unique'
SUBSECTIONS = r'subsections'
WEBSITE = r'website'

######################################################################
## BEGIN: Utilities

def mk_comment (s):
  return "<!-- " + s + " -->"

def mk_str_begin(tag):
  return '<' + tag + '>'

def mk_str_begin_atom(name):
  return '<' + ATOM + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

def mk_str_begin_block(name):
  return '<' + BLOCK + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

def mk_str_begin_field(name):
  return '<' + FIELD + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

def mk_str_end(tag):
  return '</' + tag + '>'

def mk_str_end_atom(name):
  return '</' + ATOM + '>' + SPACE + mk_comment(name)

def mk_str_end_block(name):
  return '</' + BLOCK + '>' + SPACE + mk_comment(name)

def mk_str_end_field(name):
  return '</' + FIELD + '>'+ SPACE + mk_comment(name)

## TODO: used?
def mk_xml_node(tag, text): 
  result = \
    mk_str_begin(tag) + NEWLINE + \
    text.strip() + NEWLINE + \
    mk_str_end(tag)
#  print 'mk_xml_node:\n', result
  return result

def mk_cdata(body):
  return CDATA_BEGIN + NEWLINE + body.strip() + NEWLINE + CDATA_END

def mk_str_block_atom(name, fields):
  print 'mk_str_block_atom:', name
#  print 'mk_str_block_generic:', fields
  begin = mk_str_begin_atom(name)
  end = mk_str_end_atom(name)  
  result = reduce (lambda x,y: x + NEWLINE + y, fields)
  return begin + NEWLINE + result + NEWLINE + end

def mk_str_block_generic(name, fields):
  print 'mk_str_block_generic:', name
#  print 'mk_str_block_generic:', fields
  begin = mk_str_begin_block(name)
  end = mk_str_end_block(name)  
  result = reduce (lambda x,y: x + NEWLINE + y, fields)
  return begin + NEWLINE + result + NEWLINE + end

def mk_str_field_generic(name, contents):
  begin = mk_str_begin_field(name)
  end = mk_str_end_field(name)
  result = begin + NEWLINE + contents + NEWLINE + end
  return result


## END: Utilities
######################################################################

######## 
## BEGIN: Block makers

## book maker
def mk_str_book (fields):
  return mk_str_block_generic(BOOK, fields)

## chapter maker
def mk_str_chapter (fields): 
  return mk_str_block_generic(CHAPTER, fields)

## course maker
def mk_str_course (fields):
  return mk_str_block_generic(COURSE, fields)

## course maker
def mk_str_group (fields):
  return mk_str_block_generic(GROUP, fields)

def mk_str_problem_group (fields):
  return mk_str_block_generic(PROBLEM_GROUP, fields)

def mk_str_problem_set (fields):
  return mk_str_block_generic(PROBLEM_SET, fields)

## section maker
def mk_str_section (fields):
  return mk_str_block_generic(SECTION, fields)

## subsection maker
def mk_str_subsection (fields):
  return mk_str_block_generic(SUBSECTION, fields)

## subsubsection maker
def mk_str_subsubsection (fields):
  return mk_str_block_generic(SUBSUBSECTION, fields)

## un maker
def mk_str_checkpoint (fields):
  return mk_str_block_generic(CHECKPOINT, fields)


## assignment maker
def mk_str_assignment (fields):
  return mk_str_block_generic(ASSIGNMENT, fields)

## atom maker
def mk_str_atom (atom_name, fields):
  return mk_str_block_generic(atom_name, fields)

## atom maker
def mk_str_atom (atom_name, fields):
  return mk_str_block_atom(atom_name, fields)

## algo maker
def mk_str_algo (fields):
  return mk_str_block_generic(ALGO, fields)

## problem_ff maker
def mk_str_problem_fr (fields):
  return mk_str_block_generic(PROBLEM_FR, fields)

## problem_mc maker
def mk_str_problem_ma (fields):
  return mk_str_block_generic(PROBLEM_MA, fields)

## problem_mc maker
def mk_str_problem_mc (fields):
  return mk_str_block_generic(PROBLEM_MC, fields)

## question_ff maker
def mk_str_question_fr (fields):
  return mk_str_block_generic(QUESTION_FR, fields)

## question_mc maker
def mk_str_question_ma (fields):
  return mk_str_block_generic(QUESTION_MA, fields)

## question_mc maker
def mk_str_question_mc (fields):
  return mk_str_block_generic(QUESTION_MC, fields)

## answer maker
def mk_str_answer (fields):
  return mk_str_block_generic(ANSWER, fields)

## choice maker
def mk_str_choice (fields):
  return mk_str_block_generic(CHOICE, fields)

## select maker
def mk_str_select (fields):
  return mk_str_block_generic(SELECT, fields)


## END: Block makers
######## 

######## 
## BEGIN: Field makers
def mk_str_authors(authors): 
  return mk_str_field_generic(AUTHORS, authors)

def mk_str_body (body): 
  return mk_str_field_generic(BODY, mk_cdata(body))

def mk_str_body_src (body): 
  return mk_str_field_generic(BODY_SRC, mk_cdata(body))

def mk_str_body_pop (body): 
  return mk_str_field_generic(BODY_POP, body)

def mk_str_correctness(correctness): 
  return mk_str_field_generic(CORRECTNESS, correctness)

def mk_str_course_number(number): 
  return mk_str_field_generic(COURSE_NUMBER, number)

def mk_str_duedate (duedate):
  return mk_str_field_generic(DUEDATE, duedate)

def mk_str_explain (explain): 
  return mk_str_field_generic(EXPLAIN, explain)

def mk_str_explain_src (explain): 
  return mk_str_field_generic(EXPLAIN_SRC, explain)

def mk_str_folder (folder): 
  return mk_str_field_generic(FOLDER, folder)

def mk_str_hint (hint): 
  return mk_str_field_generic(HINT, hint)

def mk_str_hint_src (hint): 
  return mk_str_field_generic(HINT_SRC, hint)

def mk_str_info(info):
  return mk_str_field_generic(INFO, info)

def mk_str_info_src(info):
  return mk_str_field_generic(INFO_SRC, info)

def mk_str_instance(info):
  return mk_str_field_generic(INSTANCE, info)

def mk_str_intro(intro): 
  return mk_str_field_generic(INTRO, mk_cdata(intro))

def mk_str_intro_src(intro): 
  return mk_str_field_generic(INTRO_SRC, mk_cdata(intro))

def mk_str_label(label): 
#  print 'label:', label
  return mk_str_field_generic(LABEL, label)

def mk_str_no(no): 
#  print 'no:', no
  return mk_str_field_generic(NO, no)

def mk_str_parents(parents): 
#  print 'mk_str_field: parents = ', parents
  parents = ', '.join(parents)
  return mk_str_field_generic(PARENTS, parents)

def mk_str_picture(picture): 
  return mk_str_field_generic(PICTURE, picture)

def mk_str_points(points): 
  return mk_str_field_generic(POINTS, points)

def mk_str_prompt (prompt): 
  return mk_str_field_generic(PROMPT, prompt)

def mk_str_prompt_src (prompt): 
  return mk_str_field_generic(PROMPT_SRC, prompt)

def mk_str_provides_book (pb): 
  return mk_str_field_generic(PROVIDES_BOOK, pb)

def mk_str_provides_chapter (pc): 
  return mk_str_field_generic(PROVIDES_CHAPTER, pc)

def mk_str_provides_section (pc): 
  return mk_str_field_generic(PROVIDES_SECTION, pc)

def mk_str_provides_subsection (pc): 
  return mk_str_field_generic(PROVIDES_SUBSECTION, pc)

def mk_str_provides_assignment (pc):
  return mk_str_field_generic(PROVIDES_ASSIGNMENT, pc)

def mk_str_semester(semester): 
  return mk_str_field_generic(SEMESTER, semester)

def mk_str_solution (solution): 
  return mk_str_field_generic(SOLUTION, solution)

def mk_str_solution_src (solution): 
  return mk_str_field_generic(SOLUTION_SRC, solution)

# def mk_str_title(title): 
#   if title:
#     return mk_str_field_generic(FIELD_TITLE, title)
#   else:
#     return KW_UNTITLED

def mk_str_title(title): 
  return mk_str_field_generic(TITLE, mk_cdata(title))

def mk_str_title_src(title): 
  return mk_str_field_generic(TITLE_SRC, mk_cdata(title))

def mk_str_topics (topics): 
  return mk_str_field_generic(TOPICS, topics)

def mk_str_unique(unique): 
  return mk_str_field_generic(UNIQUE, unique)

def mk_str_website(website): 
  return mk_str_field_generic(WEBSITE, website)

## END: Field makers
######## 
