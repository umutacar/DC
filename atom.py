from __future__ import unicode_literals

import datetime
import en_core_web_sm

import sys
import re
import os
import spacy
from itertools import combinations
reload(sys)
sys.setdefaultencoding('utf8')

# local imports
import constants


nlp = spacy.load('en')
nlpL = spacy.load('en', disable=['ner', 'parser'])
nlpL.add_pipe(nlpL.create_pipe('sentencizer'))


stopwords = constants.POSTGRES_STOP_WORDS

# constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")
GROUPING_ARGS = ['cluster', 'flex']
MATH_ARGS = ['math', 'equation', 'align', 'array', 'table', 'tabular']


class Atom(object):

	def __init__(self, string):

		assert(len(string) <= BEGIN_LEN)
		assert(string[:BEGIN_LEN] == BEGIN)
		begin_list = re.findall(r'\\begin{[ \w-]*}', string)
		begin_clause = begin_list[0]

		end_list = re.findall(r'\\end{[ \w-]*}', string)
		end_clause = end_list[-1]
		assert(begin_clause[BEGIN_LEN:-1] == end_clause[END_LEN:-1])
		self.text = string
		self.arg = begin_clause[BEGIN_LEN:-1]
		self.body = string[len(begin_clause):len(end_clause)]
		self.clean_body = re.sub(r"\\def{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", self.body)

text = "\\begin{example}[$\\{00\\}^*$] \\label{example:00-star}\
If $L = \\{\s{00}\\}$, then $L^*$ is the language consisting of all words containing an even number of $\\s{0}$'s and no other symbol. \
\\end{example}"

doc = nlp(text)

for token in doc:
	print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_, token.shape_, token.is_alpha, token.is_stop)


