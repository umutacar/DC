from __future__ import unicode_literals

import datetime
import en_core_web_sm
import en_core_web_md

import sys
import re
import os
import spacy
from itertools import combinations
reload(sys)
sys.setdefaultencoding('utf8')

# local imports
import constants


nlp = spacy.load('en_core_web_md')
nlpL = spacy.load('en_core_web_md', disable=['ner', 'parser'])
nlpL.add_pipe(nlpL.create_pipe('sentencizer'))


stopwords = constants.POSTGRES_STOP_WORDS

# constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")
GROUPING_ARGS = ['cluster', 'flex']
MATH_ARGS = ['math', 'equation', 'align', 'array', 'table', 'tabular']
GRAPHIC_ARGS = ['center', 'image']

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

# return true if curr_phrase is a super string of any strings in phrases_seen
def superString(phrases_seen, curr_phrase):
	index, length = curr_phrase
	overlapping_phrases = list(filter(lambda x: index <= x[0] and index+length >= x[0]+x[1], phrases_seen))
	return (overlapping_phrases != [])

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

def file_to_atom_keyphrase_indices(filename):
	g = open(filename, "r")
	txt_str = g.read()
	g.close()
	atoms = []
	atom_lines = txt_str.split("atom ")
	for atom_line in atom_lines:
		atom_line = atom_line[2:].strip()
		keyphrase_to_indices = {}
		keyphrase_lines = atom_line.decode().split("\n")
		if (len(keyphrase_lines) < 2):
			continue
		for i in range(0, len(keyphrase_lines), 2):
			keyphrase = keyphrase_lines[i].decode()
			indices = keyphrase_lines[i+1].decode()
			if (indices):
				print(keyphrase, indices)
				indices_list_of_strs = indices.split("\t")
				indices_list = list(map(lambda lst: (lst.split()[0], lst.split()[1]), indices_list_of_strs))
				indices_list_nums = list(map(lambda (x, y): (int(x), int(y)), indices_list))
				keyphrase_to_indices[keyphrase] = indices_list_nums
		atoms.append(keyphrase_to_indices)
	return atoms

def atoms_keyphrase_indices_to_file(filename, atom_indices):
	g = open(filename, "w+")
	txt_str = ""
	for atom in range(len(atom_indices)):
		txt_str += "atom " + str(atom) + "\n"
		for (keyphrase, index_list) in atom_indices[atom]:
			if (index_list == []):
				continue
			txt_str += "\n" + keyphrase + "\n"
			index_list_str = list(map(lambda x: str(x[0]) + " " + str(x[1]), index_list))
			txt_str += "\t".join(index_list_str)
		txt_str += "\n"
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
	for (index, phrase_len) in sentences_indices:
		# sentence = sentences[sentence_id]
		# find sentence index from word_id
		# index = cumulative_slen_list[sentence_id] + sentence_index

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
				keyphrases_to_indices[new_keyphrase] = indices_list

	str_iref += string[prev_end:]
	return str_iref, keyphrases_to_indices

# given string, map of keyphrases to labels, and map of keyphrases to indices, insert the labels as necessary
def insert_label(string, def_to_label, atom_keyphrase_to_indices):
	atoms = []
	for i in range(len(atom_keyphrase_to_indices)):
		str_iref = string[i]
		keyphrase_to_indices = atom_keyphrase_to_indices[i]
		for definition, label in def_to_label:
			if (definition in keyphrase_to_indices):
				str_iref, keyphrase_to_indices = insert_label_for_keyphrase(str_iref, definition, label, keyphrase_to_indices)
	atoms.append( (str_iref, keyphrase_to_indices) )	
	return atoms

def checkPOSMatch(doc1, doc2):
	assert(len(doc1) == len(doc2))
	for i in range(len(doc1)):
		if doc1[i].pos_ != doc2[i].pos_:
			return False
	return True
	

def identify_implicit_refs(tex_str, keyphrase_to_label):
	def parseAtoms(tex):
		atoms = []
		tex = tex.decode().strip()
		# remove flex
		flex_instance = tex.find("\\begin{flex}")
		while (flex_instance != -1):
			label_index = tex.find("\\label{", flex_instance, -1)
			label_end_index = tex.find("}", label_index, -1)
			tex = tex[:flex_instance] + tex[label_end_index+1:]
			flex_end_index = tex.find("\\end{flex}")
			tex = tex[:flex_end_index] + tex[flex_end_index + len("\\end{flex}"):]
			flex_instance = tex.find("\\begin{flex}")

		# remove gram
		gram_instance = tex.find("\\begin{gram}")
		while (gram_instance != -1):
			label_index = tex.find("\\label{", gram_instance, -1)
			label_end_index = tex.find("}", label_index, -1)
			tex = tex[:gram_instance] + tex[label_end_index+1:]
			gram_end_index = tex.find("\\end{gram}")
			tex = tex[:gram_end_index] + tex[gram_end_index + len("\\end{gram}"):]
			gram_instance = tex.find("\\begin{gram}")

		while (tex):
			tex = tex.strip()
			begin_index = tex.find("\\begin{")
			arg = tex[begin_index + BEGIN_LEN: begin_index + tex[begin_index:].index("}")]
			end = tex.index("\\end{" + arg + "}")
			end_len = len("\\end{" + arg + "}")
			atom = tex[begin_index:end+end_len]
			atom = atom.split('\n')[1:-1] # remove begin and end
			atom = "\n".join(atom)
			atoms.append(atom)
			tex = tex[end+end_len:]
		return atoms

	# input: nlp output, docs
	def getPhrases(docs):
		phrases_numbered = []
		phrases = []
		parsed_string = re.sub(r"``", "  ", docs.text)
		parsed_string = re.sub(r"''", "  ", parsed_string)
		# parsed_string = re.sub(r"/", " ", parsed_string)
		parsed_string = re.sub(r",", " ", parsed_string)
		cleaned_sentences = parsed_string.split('.')

		sentences = docs.text.split('.')
		cumulative_slen_list = sentence_indices(sentences)

		# testing sentences 
		for i in range(len(cleaned_sentences)):
			clean_sentence = cleaned_sentences[i]
			if clean_sentence == '':
				continue
			# have the word_index_list be for the non parsed sentences because otherwise the indexing is off
			word_index_list = word_indices(sentences[i])
			lst = clean_sentence.split()

			# next step: change for loop to use word_indices
			for start, end in combinations(word_index_list + [len(clean_sentence)], 2):
				curr_phrase = clean_sentence[start:end]
				
				stripped_phrase = strip_stopwords(curr_phrase)
				if(len(stripped_phrase)>0):
					try:
						index = cumulative_slen_list[i] + start
					except:
						print("Indexing error in sentence, " + sentences[i], lst, " for word index " + str(start))
					phrases_numbered.append((index, curr_phrase, stripped_phrase, lemmatize(nlpL(stripped_phrase.decode()))))
		return phrases_numbered

	# text =  re.sub(r"\\def{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	# text =  re.sub(r"\\\[[^\\\]].*?\\\]|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	tex_str = re.sub(r"-", " ", tex_str)
	tex_str = re.sub(r"/", " ", tex_str)
	atoms = parseAtoms(tex_str)

	# text = tex_str.decode()
	# sentences = text.split(".")
	# cumulative_slen = sentence_indices(sentences)
	atom_to_iref_list = []
	for i in range(1):
		atom = atoms[54]
		doc = nlp(atom.decode())
		print "Finished creating doc for atom " + str(i)
		# src_textL: [(index, str, str, lemmatize phrase)]
		src_textL = getPhrases(doc)

		implicit_ref_to_index = []
		for keyphrase, label in keyphrase_to_label:
			implicit_ref_keyphrase = []
			keyphrase_lemma = lemmatize(nlpL(keyphrase.decode()))
			nlp_keyphrase_lemma = nlp(keyphrase_lemma.decode())
			max_sim = 0
			phrases_seen = []

			for (index, curr_phrase, stripped_phrase, token_s) in src_textL:
				nlp_token_s = nlp(token_s.decode())
				if (abs(len(nlp_keyphrase_lemma.text) - len(token_s)) >= 3):
					continue
				# print "keyphrase: " + nlp_keyphrase_lemma.text + ", phrase: " + token_s.decode()
				curr_sim = check_similarity(nlp_token_s, nlp_keyphrase_lemma)
				if (curr_sim >= 1 and not superString(phrases_seen, (index, len(curr_phrase)))):
					
					# checking for POS
					if (nlp_token_s.text == nlp_keyphrase_lemma.text and not checkPOSMatch(nlp_token_s, nlp_keyphrase_lemma)):
						print("Whoops! Bad Match", nlp_token_s.text)
						continue
					print("New Match. Matched phrase '%s', or stripped, '%s' to keyphrase '%s'" % (curr_phrase, stripped_phrase, keyphrase))
					
					implicit_ref_keyphrase.append( (index, len(curr_phrase)) )
					phrases_seen.append( (index, len(curr_phrase)) )

			no_dup_keyphrases = list(set(implicit_ref_keyphrase))
			no_dup_keyphrases.sort(key=lambda x: x[0])
			implicit_ref_to_index.append( (keyphrase, no_dup_keyphrases) )
		atom_to_iref_list.append(implicit_ref_to_index)
	return atom_to_iref_list

if __name__=='__main__':
	# test cases
	# testcase_mathexpr()
	
	tex_file = sys.argv[1]
	f = open(tex_file, 'r')
	tex_str = f.read()
	def_to_label = file_to_dictionary_list("dictionary.txt")

	print "Finding implicit references..."
	# result is a list of lists of (keyphrase, implicit reference substitutes)
	result = identify_implicit_refs(tex_str, def_to_label)
	print(result)
	print "Writing implicit references to file..."
	g = atoms_keyphrase_indices_to_file("dummy.txt", result)

	# print "Getting keyphrases and irefs from file..."
	# result = file_to_atom_keyphrase_indices("chapter1_atom_implicit_references.txt")
	# print(result)
	# print "building scanned list of index for sentences..."
	# sentences = tex_str.split(".")

	# index = 0
	# for atom in result:
	# 	print "ATOM " + str(index)
	# 	index += 1
	# 	for (keyphrase, indices_list) in atom.items():
	# 		print "KEYPHRASE: " + keyphrase
	# 		for (index, phrase_len) in indices_list:
	# 			if (not is_math_expression(tex_str.decode(), index) and not is_argument(tex_str.decode(), index) \
	# 				and not is_command(tex_str.decode(), index)):
	# 		 		print("\ti: " + str(index) + ", " + str(phrase_len) + " str: " + tex_str[index - 10: index + phrase_len])

	# print "inserting labels..."
	# atoms = insert_label(tex_str, def_to_label, result)
	# print(atoms)
	# f = open('19_10_11_results.txt', 'w+')
	# iref_str = "\n".join(list(map(lambda (x, y): x, atoms)))
	# print(iref_str)
	# f.write(iref_str)
	# f.close()
