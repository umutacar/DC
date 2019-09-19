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

def lemmatize(text):
	# Extracts roots of words
	lemma = " "
	for w in text:
		if(not w.lemma_ in stopwords):
			lemma+= (w.lemma_) + " "
	return lemma

def check_similarity(phrase1, phrase2):
	return phrase1.similarity(phrase2)

# removes stopwords before the first non-stopword 
# input: string
# returns a list of the words starting at the first non-stopword in text
def strip_before(text):
    i = 0
    textList = text.split()
    for w in textList:
       
        if(w.lower() not in stopwords):
            break
        i+=1
    return textList[i:]

# removes stopwords after the last non-stopword 
# input: list of the strings
# returns a list of the words ending at the last non-stopword
def strip_after(textList):
    i = 0
    last_not_stop = -1
    
    for w in textList:
        if(w.lower() not in stopwords):
            last_not_stop = i
        i+=1
    
    return textList[:last_not_stop+1]

# removes stopwords before the first non-stopword and after the last non-stopword 
# input: string
# returns string
def strip(text):
    stripped_b = strip_before(text)
    stripped_a = strip_after(stripped_b)
    return " ".join(stripped_a)

# given a dictionary of key phrases mapped to their labels, write them to file_name
def write_to_file(dictionary, file_name):
	new_list = dictionary.items()
	# if file_name we are writing to already exists, then we want to extract the existing labels
	if (os.path.exists(file_name)):
		read_file = open(file_name, "r")
		read_str = read_file.read()
		read_file.close()
		stored_lines = list(filter(lambda x: x, read_str.split("\n")))
		stored_list = list(map(lambda x: x.split('\\label{'), stored_lines))
		stored_list = [(keyphrase, "\\label{" + label) for (keyphrase, label) in stored_list]
		stored_list += new_list
	# if file_name doesn't exist, then we just want to create and write to file_name
	else:
		stored_list = new_list

	stored_list.sort(key=lambda x: len(x[0]))
	result_str = ""
	file_obj = open(file_name, "w+")
	for keyphrase, label in stored_list:
		result_str += keyphrase + "\t"
		result_str += label + "\n"
	file_obj.write(result_str)
	file_obj.close()

def file_to_dictionary_list(dict_filename):
	f = open(dict_filename, 'r')
	dict_str = f.read()
	lines = [d.split("\\label{") for d in dict_str.strip().split("\n")]
	lines = list(filter(lambda x: x, lines))
	# last element of dict_list is an empty list
	list_tuples = list(map(lambda x: (x[0].strip(), x[1].strip()[:-1]), lines))
	return list_tuples

def parser(tex_str):
	label_to_atom = {}
	defn_list = re.findall(r'\\defn{[ \w-]*}', tex_str)
	def_list = re.findall(r'\\def{[ \w-]*}', tex_str)
	defn_keyphrase = list(set([defn[6:-1] for defn in defn_list] + [defn[5:-1] for defn in defn_list]))
	begin_index = 0
	begin_definition = re.search(r'\\begin{definition*}', tex_str)
	while (tex_str and begin_definition):
		end_definition = re.search(r'\\end{definition*}', tex_str)
		atom = tex_str[begin_definition.start(): end_definition.end()]
		def_list, defn_list = re.findall(r'\\def{[ \w-]*}', atom), re.findall(r'\\defn{[ \w-]*}', atom)
		definitions = [defn[6:-1] for defn in defn_list] + [defn[5:-1] for defn in def_list]
		label = re.findall(r'\\label{.*}', atom)
		tex_str = tex_str[end_definition.end():]
		begin_definition = re.search(r'\\begin{definition*}', tex_str)
		if (len(label) != 1):
			print("Error: there exists multiple or no labels for an atom")
			break
		for defn in definitions:
			label_to_atom[defn] = label[0]
	return label_to_atom

# return true if end of string[:index] contains an argument to a command
def is_argument(string, index):
	last_open_brace = string.rfind("{", 0, index)
	last_close_brace = string.rfind("}", 0, index)
	last_literal_brace = string.rfind("\\{", 0, index)
	# if most recent open brace is after the most recent close brace, 
	# the index is probably an argument
	if (last_open_brace > last_close_brace):
		if (last_literal_brace == -1 or last_literal_brace + 1 != last_open_brace):
			return True

	# TODO: make sure to check \[ and \]
	last_open_bracket = string.rfind("[", 0, index)
	last_close_bracket = string.rfind("]", 0, index)
	if (last_open_bracket > last_close_bracket):
		return True
	return False

# return true if end of string[:index] is a command
def is_command(string, index):
	backslash_index = string.rfind("\\", 0, index)
	substr = string[backslash_index + 1: index]
	return (len(substr.split()) < 2)

# return true if string[index] is within a a math expression
def is_math_expression(string, index):
	num_literal_dollar_signs = string.count("\$", 0, index)
	if (string.count("$$", 0, index) % 2):
		return True
	elif ((string.count("$", 0, index) - num_literal_dollar_signs) % 2):
		return True
	elif (string.count("\[", 0, index) - string.count("\]", 0, index) == 1):
		return True
	for arg in MATH_ARGS:
		begin_arg = string.rfind('\\begin{' + arg + '}', 0, index)
		if (begin_arg != -1):
			end_arg = string.rfind('\\end{' + arg + '}', begin_arg, index)
			if (end_arg == -1):
				return True
	return False

# given string and map of keyphrases to labels, identify the key pheases
# and insert the labels as necessary
def insert_label(string, def_to_label):
	lowercase_string = string.lower()
	str_iref = ""
	for definition, label in def_to_label:
		prev_end = 0
		# only use lowercase_string for finding occurrence case-insensitive
		index = lowercase_string.find(definition.lower())
		str_iref = ""
		while (index != -1):
			# if string[index: index+len(definition)] is valid key phrase
			if (is_math_expression(string, index)):
				print('math expression' + '\t' + string[index - 10: index + 10])
			elif (is_argument(string, index)):
				print("argument" + '\t' + string[index - 10: index + 10])
			elif (is_command(string, index)):
				print("command" + '\t' + string[index - 10: index + 10])
			else:
				print("is_valid" + '\t' + string[index - 10: index + 10])
				iref = "\\iref{" + label + "}{" + definition + "}"
				str_iref += string[prev_end:index]
				str_iref += iref
				# if keyphrase is replaced by label, update prev_start to be last updated index
				prev_end = index + len(definition)
			index = lowercase_string.find(definition.lower(), index+len(definition))
		str_iref += string[prev_end:]
		string = str_iref
		lowercase_string = string.lower()
	return string

def identify_implicit_refs(tex_str, keyphrase_to_label):
	# input: nlp output, docs
	def getPhrases(docs):
		id_phrases = {}

		phrases_numbered = []
		phrases = []
		sentences = docs.text.split('.')
		i = 0
		stripped_phrases = set([])
		for sentence in sentences:
			lst = sentence.split()

			for start, end in combinations(range(len(lst)+1), 2):
				i+=1
				curr_phrase = " ".join(lst[start:end])
				
				curr_phrase = strip(curr_phrase)
				id_phrases[curr_phrase] = start
				if(len(curr_phrase)>0):
					phrases.append(curr_phrase)
		for phrase in phrases:
			phrases_numbered.append((id_phrases[phrase], phrase, lemmatize(nlpL(phrase.decode()))))
		return phrases_numbered

	text =  re.sub(r"\\def{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	doc = nlp(text.decode())
	src_textL = getPhrases(doc)
	print "Finished creating doc."
	print src_textL
	for keyphrase, label in keyphrase_to_label:
		print "looking at " + keyphrase + " with label " + label
		keyphrase_lemma = lemmatize(nlpL(keyphrase.decode()))
		nlp_keyphrase_lemma = nlp(keyphrase_lemma.decode())
		max_sim = 0
		phrases_seen = set([])

		for (pos, phrase, token_s) in src_textL:
			if (abs(len(nlp_keyphrase_lemma.text) - len(token_s)) >= 3):
				continue
			curr_sim = check_similarity(nlp(token_s.decode()), nlp_keyphrase_lemma)
			if (curr_sim == 1):
				print("New Match. Matched phrase '%s' to keyphrase '%s'" % (phrase, keyphrase))


def testcase_mathexpr():
	def_string = "\\begin{definition}\\label{definition:keyphrase}\n\defn{keyphrase} is a phrase that's important.\n\\end{definition}\n"
	math_string = "\\begin{math}\n I'm looking for a keyphrase. \n\\end{math}"
	test_string = def_string + math_string
	phrase_to_label = parser(test_string)
	assert(phrase_to_label == {"keyphrase" : "\\label{definition:keyphrase}"})
	assert(is_math_expression(test_string, len(def_string) + 20))
	result = insert_label(test_string, phrase_to_label)
	assert(result == test_string)

if __name__=='__main__':
	# test cases
	# testcase_mathexpr()
	tex_file = sys.argv[1]
	f = open(tex_file, 'r')
	tex_str = f.read()
	def_to_label = file_to_dictionary_list("dictionary.txt")
	identify_implicit_refs(tex_str, def_to_label)



