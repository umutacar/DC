######################################################################
## pervasives/syntax.py
######################################################################

ENGLISH_CONNECTIVES = set(\
    ['and', 'at', 'for', 'from', 'in', 'of', 'to']
  )

EMPTY_STRING = ''

# Some symbols
AND = '&'
AT = '@'
BACKSLASH = '\\'
BAR = '|'
COMMA = r','
COLON = r':'
CURLY_BRACKET_OPEN = r'{'
CURLY_BRACKET_CLOSE = r'}'
DASH = r'-'
DOLLAR = r'$'
DOUBLE_QUOTE = '\"'
EQUAL = r'='
EXCLAMATION = r'!'
GREATER = '>'
HASH ='#'
HAT = '^'
LESS = '<'
MINUS = '-'
NEWLINE = '\n'
PAREN_OPEN = '('
PAREN_CLOSE = ')'
PERCENTAGE = '%'
QUESTION_MARK = '?'
QUOTE = '\''
PERIOD = '.'
PLUS = '+'
SLASH = '/'
SPACE = ' '
STAR = '*'
SEMI_COLON = r';'
SQUARE_BRACKET_OPEN = r'['
SQUARE_BRACKET_CLOSE = r']'
TILDE = r'~'
UNDERSCORE = r'_'



## TEX STUFF
TEX_SEPARATOR = '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' 
KW_TEX_ITEM = r'\item'
KW_TEX_BEGIN_ENUMERATE = r'\begin{enumerate}'
KW_TEX_END_ENUMERATE = r'\end{enumerate}'


## Error conditions that can occur as tokens
KW_NOT_PROVIDED = '...NOT.PROVIDED.'
KW_NO_ANS = KW_NOT_PROVIDED + 'ANSWER...' 
KW_NO_ANSWER = KW_NOT_PROVIDED + 'ANSWER...' 
KW_NO_ANSWERS = KW_NOT_PROVIDED + 'ANSWERS...' 
KW_NO_CHECKPOINT = KW_NOT_PROVIDED + 'CHECKPOINT...' 
KW_NO_EXPLAIN = KW_NOT_PROVIDED + 'EXPLANATION...' 
KW_NO_HINT = KW_NOT_PROVIDED + 'HINT...' 
KW_NO_INTRO = KW_NOT_PROVIDED + 'INTRO...' 
KW_NO_INFO = KW_NOT_PROVIDED + 'INFO...'
KW_NO_PARENTS = KW_NOT_PROVIDED + 'PARENTS...' 
KW_NO_PICTURE = KW_NOT_PROVIDED + 'PICTURE...' 
#KW_NO_POINTS = KW_NOT_PROVIDED + '0'
KW_NO_POINTS = '0'  # Still has to be a number
#KW_POINTS_CORRECT = KW_NOT_PROVIDED + '1'  
KW_POINTS_CORRECT = '1'  # Still has to be a number
KW_NO_PROMPT = KW_NOT_PROVIDED + 'PROMPT...' 

KW_NO_SEL = KW_NOT_PROVIDED + 'SELECT...' 
KW_NO_SELECT = KW_NOT_PROVIDED + 'SELECT...' 
KW_NO_SELECTS = KW_NOT_PROVIDED + 'SELECTS...' 
KW_NO_SEMESTER = KW_NOT_PROVIDED + 'UNKNOWN.SEMESTER...' 
KW_NO_SOLUTION = KW_NOT_PROVIDED + 'UNKOWN.SOLUTION...' 
KW_UNLABELED = KW_NOT_PROVIDED + 'LABEL...' 
KW_UNTITLED = KW_NOT_PROVIDED + 'TITLE...' 
KW_NO_TOPICS = KW_NOT_PROVIDED + 'TOPICS...'
KW_NO_UNIQUE = KW_NOT_PROVIDED + 'UNIQUE...' 
KW_NO_WEBSITE = KW_NOT_PROVIDED + 'WEBSITE...' 

# These are handled specially
KW_NO_UNIQUE = '0'  # has to be a number still 
KW_NO_NO = '0'  # has to be a number still 
KW_NO_CHAPTER_PROVISION = '1'   # This is the initial for chapter counter
KW_NO_SECTION_PROVISION = '1'   # This is the initial for section counter
KW_NO_SUBSECTION_PROVISION = '1'   # This is the initial for subsection counter
KW_NO_ASSIGNMENT_PROVISION = '1' # ditto



# commands, start with BACKSLASH
COM_BEGIN = BACKSLASH + r'begin'
COM_END = BACKSLASH + r'end'
COM_LABEL = BACKSLASH + r'label'
COM_NO =  BACKSLASH + r'no'
COM_PARENT = BACKSLASH + r'parent'
COM_PARENTS = BACKSLASH + r'parents'
COM_UNIQUE =  BACKSLASH + r'unique'

## Keywords, don't start with a backshlash
KW_ATOM_TITLE_PREFIX = 'Atom'
KW_CHAPTER_TITLE_PREFIX = 'Chapter'
KW_QUESTION_TITLE_PREFIX = 'Question'
KW_CHECKPOINT_TITLE_PREFIX = 'Checkpoint'
KW_SECTION_TITLE_PREFIX = 'Section'
KW_SUBSECTION_TITLE_PREFIX = 'Subsection'

# Label prefixes
KW_LABEL_PREFIX_ANSWER = 'answer'
KW_LABEL_PREFIX_ASSIGNMENT = 'assignment'
KW_LABEL_PREFIX_ASSTPROBLEM = 'asstproblem'
KW_LABEL_PREFIX_ATOM = 'atom'
KW_LABEL_PREFIX_BOOK = 'book'
KW_LABEL_PREFIX_CHAPTER = 'chapter'
KW_LABEL_PREFIX_CHOICE = 'choice'
KW_LABEL_PREFIX_COURSE = 'course'
KW_LABEL_PREFIX_GRAM = 'gram'
KW_LABEL_PREFIX_GROUP = 'group'
KW_LABEL_PREFIX_CHECKPOINT = 'checkpoint'
KW_LABEL_PREFIX_PROBLEM = 'problem'
KW_LABEL_PREFIX_PROBLEM_GROUP = 'problem_group'
KW_LABEL_PREFIX_PROBLEM_SET = 'problem_set'
KW_LABEL_PREFIX_PROBLEM_FR = 'problem_fr'
KW_LABEL_PREFIX_PROBLEM_MA = 'problem_ma'
KW_LABEL_PREFIX_PROBLEM_MC = 'problem_mc'
KW_LABEL_PREFIX_QUESTION_FR = 'question_fr'
KW_LABEL_PREFIX_QUESTION_MA = 'question_ma'
KW_LABEL_PREFIX_QUESTION_MC = 'question_mc'
KW_LABEL_PREFIX_SECTION = 'section'
KW_LABEL_PREFIX_SELECT = 'select'
KW_LABEL_PREFIX_SEMESTER = 'semester'
KW_LABEL_PREFIX_SUBSECTION = 'subsection'

## Keywords, don't start with a backshlash
KW_BOOK_NO = r'bookno'
KW_COURSE_NO = r'courseno'
KW_CHAPTER_NO = r'chapterno'
KW_FALSE = r'False'
KW_SECTION_NO = r'sectionno'
KW_ATOM_NO = r'atomno'
KW_QUESTION_NO = r'problemno'
KW_CHECKPOINT_NO = r'checkpointno'
KW_PARENT = r'parent'
KW_PROMPT = r'prompt'
KW_POINTS = r'points'
KW_TRUE = r'True'
KW_SUBSECTION_NO = r'subsectionno'



######################################################################
## BEGIN: Utilities

## join strings with newlines
#def join_str (xs, connector): 
#  return reduce (lambda x, y: x + connector + y, xs)
 # just use connector.join 


# Capitalize every word of a title
def mk_str_cap_title(s):
  result = ''
  words = s.split(' ')
  for w in words:

    # Be careful with multiple spaces, and empty strings
    # for empty words w[0] would cause an index error, 
    # but with w[:1] we get an empty string as desired
    w_out = w
    if w not in ENGLISH_CONNECTIVES:  
      w_out = w[:1].upper() + w[1:]

    result = result + ' ' + w_out    
  return result 

## latex style optarg
def mk_str_opt_arg(arg): 
  result = SQUARE_BRACKET_OPEN +  arg + SQUARE_BRACKET_CLOSE
  return result

## utility: latex style arg
def mk_str_arg(arg): 
#  print 'mk_str_arg:', arg
  result = CURLY_BRACKET_OPEN +  arg + CURLY_BRACKET_CLOSE 
  return result

## What to do with special characters inside the label?
## currently we replace them all
def sanitize_title (title): 
  s = title.lower()
  s = s.replace(' ', '_')
  for x in PHRASE_SYMBOLS: 
    s = s.replace(x, '_')
  s = s.replace('__', '_')
  return s

## END: Utilities
######################################################################

def missing_field(field_body):
#  print 'missing_field: KW_NOT_PROVIDED = ', KW_NOT_PROVIDED
#  print 'missing_field:', field_body[0:50]
  field_body = field_body.strip()
  if KW_NOT_PROVIDED in field_body:
#    print 'missing_field: True'
    return True
  else: 
#    print 'missing_field: False'
    return False

def valid_answer(answer):
  # explain could have <p> tags and other formatting.
  if KW_NO_ANSWER in answer:
    return False
  else:
    return True


def valid_explain(explain):
  explain = explain.strip()
  # explain could have <p> tags and other formatting.
  if KW_NO_EXPLAIN in explain:
    return False
  else:
    return True

def valid_intro(intro):
  intro = intro.strip()
  # explain could have <p> tags and other formatting.
  if KW_NO_INTRO in intro:
    return False
  else:
    return True


def valid_label(label):
  label = label.strip()
  if label == KW_UNLABELED: 
    return False
  else:
    return True

def valid_title(title):
  title = title.strip()
  if title == KW_UNTITLED:
    return False
  else:
    return True

######################################################################
## BEGIN: keyword makers 

def mk_str_atom_title (atom_name, title):   
  atom_name = cap_title(atom_name)
  title_out = KW_ATOM_TITLE_PREFIX +  COLON + atom_name + COLON +  cap_title(title)
#  print 'mk: title_out:', title_out
  return title_out

def mk_str_begin(s):
  result = COM_BEGIN + mk_str_arg(s) 
#  print "mk_begin: ", result
  return result 

def mk_str_chapter_title (title):   
  title_out = KW_CHAPTER_TITLE_PREFIX + COLON + cap_title(title)
#  print 'mk: title_out:', title_out
  return title_out

def mk_str_end(s): 
  result = COM_END + mk_str_arg(s) 
#  print "mk_end: ", result
  return result

def mk_str_label(name): 
  return COM_LABEL + mk_str_arg(name) 

## What to do with special characters inside the label?
## currently we replace them all
def mk_str_latex_atom_label(atom_name, title):
#  print 'mk_latex_atom_label:', s
  s = title.lower()
  s = s.replace(' ', '_')
  for x in PHRASE_SYMBOLS: 
    s = s.replace(x, '_')
  s = s.replace('__', '_')
#  print 'mk_latex_atom_label:', s
  result = 'atom:' + atom_name + ':' + s 
#  result = r'\label{atom:' + atom_name + ':' + s + '}'
  return result 
#####################

def mk_str_latex_chapter_label(title):
#  print 'mk_latex_atom_label:', s
  s = sanitize_title(title)
  result = 'chapter:' + s 
  return result 

def mk_str_latex_section_label(title):
#  print 'mk_latex_atom_label:', s
  s = sanitize_title(title)
  result = 'section:' + s 
  return result 

def mk_str_latex_subsection_label(title):
  s = sanitize_title(title)
  result = 'subsection:' + s 
  return result 

def mk_str_unique (un):
  result = COM_UNIQUE + mk_str_arg(un) 
  return result 

def mk_str_question_title (title):   
  title_out = KW_QUESTION_TITLE_PREFIX +  COLON + cap_title(title)
  return title_out

def mk_str_checkpoint_title (title):   
  title_out = KW_CHECKPOINT_TITLE_PREFIX + COLON + cap_title(title)
#  print 'mk: title_out:', title_out
  return title_out

def mk_str_section_title (title):   
  title_out = KW_SECTION_TITLE_PREFIX + COLON + cap_title(title)
#  print 'mk: title_out:', title_out
  return title_out

def mk_str_subsection_title (title):   
  title_out = KW_SUBSECTION_TITLE_PREFIX + COLON + cap_title(title)
#  print 'mk: title_out:', title_out
  return title_out

## END: keyword makers 








