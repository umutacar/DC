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

GLOBAL_INDEX = {}

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
def strip_stopwords(text):
	stripped_b = strip_before(text)
	stripped_a = strip_after(stripped_b)
	return " ".join(stripped_a)

# given a string, create a list that maps each word to its index
def word_indices(string):
	result = []
	index = 0
	if not string[0].isspace():
		result.append(index)
	index += 1
	while (index < len(string)):
		if (not string[index].isspace() and string[index-1].isspace()):
			result.append(index)
		index += 1
	return result

# given a list of sentences, create a list that maps each sentence to its index
def sentence_indices(sentences):
	sentences_length = list(map(lambda x: len(x) + 1, sentences)) # +1 for the '.'
	cumulative_sentence_length_list = []
	cumulative_length = 0
	for length in sentences_length:
		cumulative_sentence_length_list.append(cumulative_length)
		cumulative_length += length
	return cumulative_sentence_length_list

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

def file_to_keyphrase_indices(filename):
	g = open(filename, "r")
	txt_str = g.read()
	g.close()
	keyphrase_to_indices = {}
	keyphrase_lines = txt_str.decode().split("\n")
	for i in range(1, len(keyphrase_lines), 2):
		keyphrase = keyphrase_lines[i].decode()
		indices = keyphrase_lines[i+1].decode()
		if (indices):
			indices_list_of_strs = indices.split("\t")
			indices_list = list(map(lambda lst: (lst.split()[0], lst.split()[1]), indices_list_of_strs))
			indices_list_nums = list(map(lambda (x, y): (int(x), int(y)), indices_list))
			keyphrase_to_indices[keyphrase] = indices_list_nums
	return keyphrase_to_indices

def keyphrase_indices_to_file(filename, keyphrase_indices):
	g = open(filename, "w+")
	txt_str = ""
	for (keyphrase, index_list) in keyphrase_indices:
		txt_str += "\n" + keyphrase + "\n"
		index_list_str = list(map(lambda x: str(x[0]) + " " + str(x[1]), index_list))
		txt_str += "\t".join(index_list_str)
	g.write(txt_str)
	g.close()

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

# given string, label, and map of keyphrases to indices, insert the labels as necessary
def insert_label_for_keyphrase(string, keyphrase, label, keyphrases_to_indices):
	sentences = string.split(".")
	sentences_length = list(map(lambda x: len(x) + 1, sentences)) # +1 for the '.'
	cumulative_slen_list = sentence_indices(sentences)

	str_iref = ""
	iref = "\\iref{" + label + "}{" + keyphrase + "}"

	sentences_indices = keyphrases_to_indices[keyphrase]
	prev_end = 0
	for (sentence_id, sentence_index, phrase_len) in sentences_indices:
		sentence = sentences[sentence_id]
		# find sentence index from word_id
		index = cumulative_slen_list[sentence_id] + sentence_index

		# check if the keyphrase is in the right environment
		if (is_math_expression(string, index)):
			print('math expression' + '\t' + string[index - 10: index + 10])
		elif (is_argument(string, index)):
			print("argument" + '\t' + string[index - 10: index + 10])
		elif (is_command(string, index)):
			print("command" + '\t' + string[index - 10: index + 10])
		else:
			print("is_valid" + '\t' + string[index - 10: index + 10])

			str_iref += string[prev_end:index]
			str_iref += iref
			prev_end = index + phrase_len

			# update other indices list in keyphrases_to_indices
			for (new_keyphrase, indices_list) in keyphrases_to_indices.items():
				if (len(new_keyphrase) > len(keyphrase)):
					continue
				for i in range(len(indices_list)):
					# new_string_id, new_string_index,
					(new_index, new_phrase_len) = indices_list[i]
					# new_index = cumulative_slen_list[new_string_id] + new_string_index
					increment = len(iref) - phrase_len



					# only look at indices after current index
					if (new_index > index):
						new_index += increment
						indices_list[i] = (new_index, new_phrase_len)
				keyphrase_to_indices[new_keyphrase] = indices_list

	str_iref += string[prev_end:]
	return str_iref, keyphrase_to_indices

# given string, map of keyphrases to labels, and map of keyphrases to indices, insert the labels as necessary
def insert_label(string, def_to_label, keyphrase_to_indices):
	sentences = string.split(".")
	str_iref = string

	for definition, label in def_to_label[:12]:
		if (definition in keyphrase_to_indices):
			str_iref, keyphrases_to_indices = insert_label_for_keyphrase(str_iref, definition, label, keyphrase_to_indices)
	return str_iref, keyphrase_to_indices

def identify_implicit_refs(tex_str, keyphrase_to_label):
	og_tex_str = tex_str
	# input: nlp output, docs
	def getPhrases(docs):
		id_phrases = {}

		phrases_numbered = []
		phrases = []
		sentences = docs.text.split('.')
		assert(tex_str == docs.text)
		# testing sentences 
		cumulative_slen_list = sentence_indices(sentences)

		for i in range(len(sentences)):
			sentence = sentences[i]
			word_index_list = word_indices(sentence)
			lst = sentence.split()

			for start, end in combinations(range(len(lst)+1), 2):
				curr_phrase = " ".join(lst[start:end])
				
				stripped_phrase = strip_stopwords(curr_phrase)
				# id_phrases[curr_phrase] = word_index_list[start]
				if(len(curr_phrase)>0):
					index = cumulative_slen_list[i] + word_index_list[start]
					# GLOBAL_INDEX[curr_phrase] = index
					# print "curr_phrase: " + curr_phrase + ", test phase: " + tex_str[index:index+len(curr_phrase)]
					# print(curr_phrase[:2], tex_str[index:index+2], index, start, end, lst)
					phrases_numbered.append((index, curr_phrase, lemmatize(nlpL(stripped_phrase.decode()))))
		return phrases_numbered

	# text =  re.sub(r"\\def{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	text = tex_str

	sentences = text.split(".")
	cumulative_slen = sentence_indices(sentences)
	
	doc = nlp(text.decode())
	print "Finished creating doc."
	src_textL = getPhrases(doc)
	print "Finished collecting all phrases."
	implicit_ref_to_index = []
	for keyphrase, label in keyphrase_to_label:
		print "looking at " + keyphrase + " with label " + label
		implicit_ref_keyphrase = []
		keyphrase_lemma = lemmatize(nlpL(keyphrase.decode()))
		nlp_keyphrase_lemma = nlp(keyphrase_lemma.decode())
		max_sim = 0
		phrases_seen = set([])

	
		for (index, phrase, token_s) in src_textL:

			# index = cumulative_slen[sentence_id] + string_index
			if (abs(len(nlp_keyphrase_lemma.text) - len(token_s)) >= 3):
				continue
			curr_sim = check_similarity(nlp(token_s.decode()), nlp_keyphrase_lemma)
			if (curr_sim == 1):
				print("New Match. Matched phrase '%s' to keyphrase '%s'" % (phrase, keyphrase))
				print phrase + " " + tex_str[index: index + len(phrase)]
				assert(phrase == tex_str[index: index + len(phrase)])
				implicit_ref_keyphrase.append( (index, len(phrase)) )

		no_dup_keyphrases = list(set(implicit_ref_keyphrase))
		no_dup_keyphrases.sort(key=lambda x: x[0])
		implicit_ref_to_index.append( (keyphrase, no_dup_keyphrases) )
	return implicit_ref_to_index

if __name__=='__main__':
	# test cases
	# testcase_mathexpr()
	
	tex_file = sys.argv[1]
	f = open(tex_file, 'r')
	tex_str = f.read()
	def_to_label = file_to_dictionary_list("dictionary.txt")

	# print "Finding implicit references..."
	# result = identify_implicit_refs(tex_str, def_to_label)
	# print "Writing implicit references to file..."
	# g = keyphrase_indices_to_file("chapter1_implicit_references.txt", result)

	print "Getting keyphrases and irefs from file..."
	keyphrase_to_indices = file_to_keyphrase_indices("chapter1_implicit_references.txt")
	print(keyphrase_to_indices)
	print "building scanned list of index for sentences..."
	sentences = tex_str.split(".")

	for (keyphrase, indices_list) in keyphrase_to_indices.items():
		print "KEYPHRASE: " + keyphrase
		for (index, phrase_len) in indices_list:
			if (not is_math_expression(tex_str.decode(), index) and not is_argument(tex_str.decode(), index) \
				and not is_command(tex_str.decode(), index)):
				print("\ti: " + str(index) + ", " + str(phrase_len) + " str: " + tex_str[index - 10: index + phrase_len])

	# print "inserting labels..."
	# new_str, keyphrases_to_indices = insert_label(tex_str, def_to_label, keyphrase_to_indices)
	# # print(new_str)
	# f = open('19_09_26_results.txt', 'w+')
	# f.write(new_str)
	# f.close()
