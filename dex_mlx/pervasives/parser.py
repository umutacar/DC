######################################################################
## pervasives/parser.py
######################################################################

## Utilities shared by all parsers

import re
import pyparsing as pp 
from pervasives.syntax import *


######################################################################
## BEGIN: Pyparser static method updates

## Set the default characters for keywords to include * and to exclude $
## Can't include BACKSLASH BECAUSE OTHERWISE IT gets confused!!!!
## Keywords are used for \choice and \choice*

KEYWORD_CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_*'
pp.Keyword.setDefaultKeywordChars(KEYWORD_CHARS)

## END: Pyparser static method updates
######################################################################



######################################################################
## BEGIN: Blocks
class Block:
  ALGO = 0
  ANSWER = 1
  BOOK = 2
  CHAPTER = 3
  CHOICE = 4
  SECTION = 5
  CHECKPOINT = 6
  UNIT = 7
  GROUP = 8
  ATOM = 9
  PROBLEM = 10
  PROBLEMSET = 11
  QUESTION = 12
  ASSIGNMENT = 13
  ASSTPROBLEM = 14


## END: Blocks
######################################################################

##############################
## BEGIN: Parsers 

## TODO: What is a phrase? 

# All phrase symbors minus curly, minus square brackets, which are special
PHRASE_SYMBOLS_LATEX = AND + AT + BAR + BACKSLASH + COLON + COMMA + DASH + DOLLAR + DOUBLE_QUOTE + EQUAL + EXCLAMATION + GREATER + HASH + HAT + LESS + MINUS + PERIOD + PLUS + QUOTE + SLASH + STAR + TILDE + PAREN_OPEN + PAREN_CLOSE + PERCENTAGE + QUESTION_MARK

## For parser phrases
PHRASE_SYMBOLS = PHRASE_SYMBOLS_LATEX + CURLY_BRACKET_OPEN + CURLY_BRACKET_CLOSE

# Labels
LABEL_SYMBOLS = UNDERSCORE+COLON+PERIOD+DASH

## Various Symbols 
kw_dollar = pp.Literal(DOLLAR).suppress()
kw_sq_open = pp.Literal(SQUARE_BRACKET_OPEN).suppress()
kw_sq_close = pp.Literal(SQUARE_BRACKET_CLOSE).suppress()
kw_curly_open = pp.Literal(CURLY_BRACKET_OPEN).suppress()
kw_curly_close = pp.Literal(CURLY_BRACKET_CLOSE).suppress()
kw_curly_open_ = pp.Literal(CURLY_BRACKET_OPEN)
kw_curly_close_ = pp.Literal(CURLY_BRACKET_CLOSE)
kw_comma = pp.Literal(COMMA).suppress()
kw_semicolon = pp.Literal(SEMI_COLON).suppress()
kw_string_end = pp.StringEnd()

# commands
kw_begin = pp.Literal(COM_BEGIN)
kw_end = pp.Literal(COM_END)

## Constructors
def con_exp_arg(arg):
  SQUARE_BRACKET_OPEN + ' ' + arg  + ' ' + SQUARE_BRACKET_CLOSE 

######################################################################
## BEGIN: RE (regular expressions)
##
## These are currently unused I believe but 
## keep them for future reference.
##
## IMPORTANT: by using *? we are matching Non-greedily.
# match a label definition
pattern_label = re.compile(r'(\\label{(?P<name>(.)*?)})')

# match a ref
pattern_ref = re.compile(r'(\\ref{(?P<name>(.)*?)})')

## END: RE (regular expressions
######################################################################


######################################################################
## BEGIN: parsers


# comment
latex_comment =  (pp.NotAny(r'\%') + pp.Literal(r'%')) + pp.restOfLine() + pp.lineEnd()

# Matches a line
exp_line = pp.restOfLine() + pp.lineEnd()

# argument makers
def mk_parser_arg (parser):
  result = kw_curly_open + parser + kw_curly_close
#  result = result.setDebug()
  return result

def mk_parser_opt_arg (parser):
  result = kw_sq_open + parser + kw_sq_close
#  result = result.setDebug()
  return result


# latex label names
label_name = pp.Word(pp.alphanums+LABEL_SYMBOLS)
com_label = pp.Literal(COM_LABEL).suppress()
#com_label = com_label.setDebug()
exp_label = com_label + mk_parser_arg(label_name)
#label = label.setParseAction(lambda x: x[0] + CURLY_BRACKET_OPEN + x[1] + CURLY_BRACKET_CLOSE)

# parent
com_parent = pp.Literal(COM_PARENT).suppress()
#com_parent.setDebug()
exp_parent = com_parent + mk_parser_arg(label_name)

# author names
exp_authors = pp.OneOrMore(pp.Word(pp.alphas + PERIOD))
exp_authors = exp_authors.setParseAction(lambda x: ' '.join(x.asList()))


# phrases: a phrase is zero or more words and commas
phrase_gram = pp.Word(pp.alphanums+PHRASE_SYMBOLS)
# this does not work, don't understand why
#phrase_unit = pp.Combine(pp.OneOrMore(~kw_semicolon + ~kw_sq_close + ~kw_end + pp.printables))
#phrase = pp.OneOrMore(pp.Word(pp.alphanums+COMMA)).setName('phrase').setResultsName('phrase')
phrase = pp.OneOrMore(phrase_gram)
phrase = phrase.setParseAction(lambda x: ' '.join(x.asList()))

# A list of phrases is phrases separeted by semicolons
list_phrases = pp.Forward()
list_phrases << phrase + pp.Optional(kw_semicolon + list_phrases)

# Title phrases, clauses
title_phrases = list_phrases
title_phrases = title_phrases.setParseAction(lambda x:", ".join(x.asList()))

#exp_title = mk_parser_arg_sq(title_phrases)
exp_title = title_phrases

## Latex suitable phrases, exclude curly
# phrases: a phrase is zero or more words and commas
phrase_gram_latex = pp.Word(pp.alphanums+PHRASE_SYMBOLS_LATEX)
exp_phrase_gram_latex = phrase_gram_latex

phrase_latex = pp.OneOrMore(phrase_gram_latex)
phrase_latex = phrase_latex.setParseAction(lambda x: ' '.join(x.asList()))
exp_phrase_latex = phrase_latex

# A list of phrases is phrases separeted by semicolons
list_phrases_latex = pp.Forward()
list_phrases_latex << phrase_latex + pp.Optional(kw_semicolon + list_phrases_latex)

title_phrases_latex = list_phrases_latex
title_phrases_latex = title_phrases_latex.setParseAction(lambda x:", ".join(x.asList()))
exp_title_latex = title_phrases_latex


# Course number, drop dash
exp_dash_separated_number = pp.Combine(pp.Word(pp.nums) + \
                            pp.Literal(DASH).suppress() + \
                            pp.Word(pp.nums))
# numbers
#number_exp = pp.Word(pp.nums).setName('number_exp').setResultsName('number_exp')
exp_number = pp.Word(pp.nums).setName('exp_number').setResultsName('exp_number')

# includes minus sign
exp_integer_number = pp.Combine(pp.Optional(MINUS) + pp.Word(pp.nums)).setName('exp_number').setResultsName('exp_number')

# Parent phrases, clauses
parents_phrases =  list_phrases
# you don't want to join parents phrases, because they need to be separate
# so that you can treat each as ane item
# so convert it into a list.
parents_phrases = parents_phrases.setParseAction(lambda x:x.asList())
exp_parents = mk_parser_opt_arg(parents_phrases)
#parents_clause = parents_clause.setName('parents_clause').setResultsName('parents_clause')


# parser for \begin{kw}
def mk_parser_begin (kw):
  com_begin = kw_begin + kw_curly_open_ + pp.Literal(kw) + kw_curly_close_
  com_begin = com_begin.setParseAction(lambda x: ''.join(x.asList()))
#  com_begin = set_key_begin(com_begin)
#  com_begin = com_begin.setDebug()
  # Wrap it inside a group so you can use another parseAction

  com_begin = pp.Group(com_begin)
  return com_begin

# parser for \end{kw}
def mk_parser_end (kw):
  com_end = kw_end + kw_curly_open_ + pp.Literal(kw) + kw_curly_close_
  com_end = com_end.setParseAction(lambda x: ''.join(x.asList()))
#  com_end = set_key_end(com_end)
#  com_end = com_end.setDebug()

  # Wrap it inside a group so you can use another parseAction
  com_end = pp.Group(com_end)

  return com_end


## END: parsers
######################################################################

