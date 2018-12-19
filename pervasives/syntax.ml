######################################################################
## pervasives/syntax.py
######################################################################

let empty_string = ''

(* Some symbols *)
str_backslash = '\\'
str_bar = '|'
str_comma = r','
str_colon = r':'
str_curly_open = r'{'
str_curly_close = r'}'
str_dash = r'-'
str_dollar = r'$'
str_double_quote = '\"'
str_equal = r'='
str_exclamation = r'!'
str_greater = '>'
str_hash ='#'
str_hat = '^'
str_less = '<'
str_minus = '-'
str_newline = '\n'
str_paren_open = '('
str_paren_close = ')'
str_percentage = '%'
str_question_mark = '?'
str_quote = '\''
str_period = '.'
str_plus = '+'
str_slash = '/'
str_space = ' '
str_star = '*'
str_semi_colon = r';'
str_square_bracket_open = r'['
str_square_bracket_close = r']'
str_tilde = r'~'
str_underscore = r'_'


## TEX STUFF
str_tex_separator = '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' 



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

def missing_field_unique(field_body):
#  print 'missing_field: KW_NOT_PROVIDED = ', KW_NOT_PROVIDED
#  print 'missing_field:', field_body[0:50]
  field_body = field_body.strip()
  if KW_NO_UNIQUE == field_body:
#    print 'missing_field: True'
    return True
  else: 
#    print 'missing_field: False'
    return False

def missing_field_rank(field_body):
#  print 'missing_field: KW_NOT_PROVIDED = ', KW_NOT_PROVIDED
#  print 'missing_field:', field_body[0:50]
  field_body = field_body.strip()
  if KW_NO_RANK == field_body:
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








