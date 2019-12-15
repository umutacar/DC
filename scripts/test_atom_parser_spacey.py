from __future__ import unicode_literals

import datetime
import en_core_web_sm
import en_core_web_md

import sys
import re
import os
import spacy
import json
from itertools import combinations
reload(sys)
sys.setdefaultencoding('utf8')

# local imports
import constants


nlp = spacy.load('en_core_web_md', disable=['ner'])
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


# given a string, create a list of (start, end) indices that correspond to non-stopwords
def nonstopword_indices(string):
	indices = word_indices(string) + [len(string)]
	result = []
	for i in range(len(indices)):
		index = indices[i]
		if (i < len(indices) - 1):
			next_index = indices[i+1]
			word = string[index:next_index].strip()
		else:
			word = string[index:].strip()

		if (word.lower() not in stopwords):
			result.append( (index, len(word)) )
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

def find_sentence_index(atom, index):
	sentence_length = sentence_indices(atom.decode().strip().split('.'))
	if (index == 0):
		return 0
	elif (index >= sentence_length[-1]):
		return len(sentence_length) - 1
	i = 0
	while (sentence_length[i] <= index):
		i+=1
	return i - 1

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

def nested_dictionary_list(keyphrases):
	keyphrase_dict = {}
	for keyphrase in keyphrases:
		if (' of ' in keyphrase):
			of_index = keyphrase.rfind(' of ')
			prefix = keyphrase[:of_index]
			suffix = keyphrase[of_index:]
			if (prefix in keyphrase_dict):
				keyphrase_dict[prefix] += [suffix]
			else:
				keyphrase_dict[prefix] = [suffix]
		else:
			if (keyphrase in keyphrase_dict):
				keyphrase_dict[keyphrase] += ['']
			else:
				keyphrase_dict[keyphrase] = ['']
	return keyphrase_dict

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
				# print(keyphrase, indices)
				indices_list_of_strs = indices.split("\t")
				indices_list = list(map(lambda lst: (lst.split()[0], lst.split()[1]), indices_list_of_strs))
				indices_list_nums = list(map(lambda (x, y): (int(x), int(y)), indices_list))
				keyphrase_to_indices[keyphrase] = indices_list_nums
		atoms.append(keyphrase_to_indices)
	return atoms

def atoms_keyphrase_indices_to_file(filename, atom_indices):
	# filter out results that are empty
	# for i in range(len(atom_indices)):
	# 	atom = atom_indices[i]
	# 	for (keyphrase, indices) in atom.items():
	# 		if indices == []:
	# 			del atom[keyphrase]
	# 	atom_indices[i] = atom

	with open(filename, "w+") as write_file:
		json_str = json.dumps(atom_indices, indent=2)
		write_file.write(json_str)
		write_file.close()

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
		if ("\\label{" in atom[0]):
			atom = atom[1:]
		atom = "\n".join(atom)
		atoms.append(atom)
		tex = tex[end+end_len:]
	return atoms

def identify_implicit_refs(tex_str, keyphrase_dict):
	# input: atom text
	def getPhrases(text):
		print(text)
		phrases_numbered = []
		phrases = []
		parsed_string = re.sub(r"``", "  ", text)
		parsed_string = re.sub(r"''", "  ", parsed_string)
		parsed_string = re.sub(r",", " ", parsed_string)
		cleaned_sentences = parsed_string.split('.')
		sentences = text.split('.')
		cumulative_slen_list = sentence_indices(sentences)
		# print("getPhrases, cumulative slen list: ", cumulative_slen_list)
		# testing sentences 
		for i in range(len(cleaned_sentences)):
			clean_sentence = cleaned_sentences[i]
			# clean_sentence_doc = nlpL(clean_sentence)
			if clean_sentence == '':
				continue
			nonstopword_index_list = nonstopword_indices(clean_sentence)
			# print(clean_sentence, nonstopword_index_list)
			for (s, length) in nonstopword_index_list:
				curr_phrase = clean_sentence[s:s+length]
				# print(curr_phrase)
				stripped_phrase = curr_phrase.strip()
				try:
					index = cumulative_slen_list[i] + s
				except:
					print("Indexing error in sentence, " + sentences[i], " for word index " + str(s))
				if (not is_math_expression(text, index)):
					# print('works', stripped_phrase)
					phrases_numbered.append( (cumulative_slen_list[i], s, curr_phrase.strip(), lemmatize(nlpL(stripped_phrase.decode()))) )
				# else:
				# 	print('doesnt work', stripped_phrase)
			for ((s1, len1), (s2, len2)) in combinations(nonstopword_index_list, 2):
				curr_phrase = clean_sentence[s1:s2 + len2]
				stripped_phrase = curr_phrase.strip()
				try:
					index = cumulative_slen_list[i] + s1
				except:
					print("Indexing error in sentence, " + sentences[i], " for word index " + str(s1))
				if (not is_math_expression(text, index)):
					# print "works" + stripped_phrase
					phrases_numbered.append( (cumulative_slen_list[i], s1, curr_phrase.strip(), lemmatize(nlpL(stripped_phrase.decode()))) )
				# else:
				# 	print "doesnt work " + stripped_phrase
		return phrases_numbered

	# text =  re.sub(r"\\def{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	# text =  re.sub(r"\\\[[^\\\]].*?\\\]|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", tex_str)
	tex_str = re.sub(r"-", " ", tex_str)
	tex_str = re.sub(r"/", " ", tex_str)
	tex_str = re.sub(r"~", " ", tex_str)
	tex_str = re.sub(r";", " ", tex_str)
	tex_str = re.sub(r":", " ", tex_str)
	# tex_str = re.sub("?", " ", tex_str)

	# input: 
	# return true if keyphrase is not being used as an adjective and false otherwise
	def checkMatch(atom, index, curr_phrase):
		print(curr_phrase, index)
		# check that the phrase is 1 word
		if (len(curr_phrase.strip().split()) > 1):
			return True
		# determine which sentence index belongs to
		sentences = atom.decode().strip().split('.')
		cumulative_slen = sentence_indices(sentences)

		sentence_index = find_sentence_index(atom, index)
		
		sentence = sentences[sentence_index]
		sentence_doc = nlp(sentence)
		# finding corresponding nlp token
		token_index = 0
		while (len(sentence_doc[:token_index].text) <= index - cumulative_slen[sentence_index]):
			token_index += 1
		token = sentence_doc[token_index - 1]
		if (token.text.strip() != curr_phrase.strip()):
			return True
		assert(token.text.strip() == curr_phrase.strip())
		if (token.pos_ == 'NOUN' or token.pos_ == 'VERB'):
			# make sure a phrase that is supposedly a noun or a verb is NOT an adjective
			# if (token.head.pos_ == 'NOUN' and token.dep_ != 'ROOT'): # valid match if head is a verb or if phrase is the root
			# 	return False
			# make sure the word is not part of a noun chunk that represents something else.
			# ex: programming language, bipartite graph, etc
			if (token.pos_ == 'NOUN' and token_index > 1):
				prev_token = sentence_doc[token_index - 2]
				# print(token.text, token.pos_, prev_token.text, prev_token.pos_)
				return not (prev_token.head.text.strip() == token.text.strip() and prev_token.pos_ == 'NOUN')
		return True


	atoms = parseAtoms(tex_str)
	atom_to_iref_list = []
	for i in range(13, 22):#len(atoms)):
		atom = atoms[i]
		sentences = atom.decode().strip().split('.')
		# src_textL: [(index, str, str, lemmatize nlpL phrase)]
		src_textL = getPhrases(atom.decode())
		# print(src_textL)
		print "Finished extracting phrases for atom " + str(i)
		implicit_ref_to_index = {}
		for (prefix, suffixes) in keyphrase_dict.items():
			implicit_ref_for_prefix = {}
			for suffix in suffixes:
				keyphrase = prefix + suffix
				print 'keyphrase ' + keyphrase
				if (' of '  in suffix):
					
					threshold = 0.90
				else:
					threshold = 1.0

				implicit_ref_per_suffix = []
				prefix_lemma = lemmatize(nlpL(prefix.decode()))
				nlp_prefix_lemma = nlp(prefix_lemma.decode())
				phrases_seen = set([])
			
				# print(nlp_keyphrase_lemma[0].text, nlp_keyphrase_lemma[0].pos_)
				# find phrases that have high similarity score to the prefix + suffix
				for (index0, index1, curr_phrase, token_s) in src_textL:
					nlp_token_s = nlp(token_s.decode())
					# if (index0 == 0 and index1 == 45 and curr_phrase.decode() == 'DFA'):
					# 	print('HELLO')
					if (abs(len(nlp_prefix_lemma.text) - len(token_s)) >= 3):
						continue
					# if (index0 == 0 and index1 == 45 and curr_phrase.decode() == 'DFA'):
					# 	print('GOODBYE')
					curr_sim = check_similarity(nlp_token_s, nlp_prefix_lemma)
					# if (keyphrase == 'DFA'):
						# print nlp_token_s.text + " " + nlp_prefix_lemma.text + " " + str(curr_sim)
					# print "keyphrase: " + nlp_prefix_lemma.text + ", phrase: " + token_s.decode() + " sim: " + str(curr_sim)
					if (curr_sim >= threshold and not superString(phrases_seen, (index0+index1, len(curr_phrase)))):
						# sentence_index = find_sentence_index(atom, index)
						# sentence = sentences[sentence_index]
						# if (checkMatch(atom, index0+index1, curr_phrase)):
							# print("Sentence index: '%s' and word index: '%s'" % (index0, index1))
						print("New Match. Matched phrase '%s' to keyphrase '%s'" % (curr_phrase, keyphrase))
						implicit_ref_per_suffix.append( (index0+index1, len(curr_phrase), curr_phrase) )
						phrases_seen.add( (index0+index1, len(curr_phrase)) )
					# elif (curr_sim >= 0.9 and not superString(phrases_seen, (index0+index1, len(curr_phrase)))):

				no_dup_suffix = list(set(implicit_ref_per_suffix))
				no_dup_suffix.sort(key=lambda x: x[0])

				if (no_dup_suffix != []):
					implicit_ref_for_prefix[suffix] = no_dup_suffix
			if (implicit_ref_for_prefix != {}):
				implicit_ref_to_index[prefix] = implicit_ref_for_prefix
		atom_to_iref_list.append(implicit_ref_to_index)
	return atom_to_iref_list

if __name__=='__main__':
	tex_file = sys.argv[1]
	f = open(tex_file, 'r')
	tex_str = f.read()
	def_to_label = file_to_dictionary_list("dictionary.txt")

	keyphrase_dict = nested_dictionary_list(list(map(lambda x: x[0], def_to_label)))

	print "Finding implicit references..."
	# result is a list of lists of (keyphrase, implicit reference substitutes)
	result = identify_implicit_refs(tex_str, keyphrase_dict)
	print(result)
	print "Writing implicit references to file..."
	g = atoms_keyphrase_indices_to_file("label_detection_results/chapter3_12_6_19/test_12_6_19_ch3_spacy.json", result)

	# print "Getting keyphrases and irefs from file..."
	# result = file_to_atom_keyphrase_indices("chapter1_atom_implicit_references.txt")
	# print(result)
	# print "building scanned list of index for sentences..."
	# sentences = tex_str.split(".")

	# print "inserting labels..."
	# atoms = insert_label(tex_str, def_to_label, result)
	# print(atoms)
	# f = open('19_10_11_results.txt', 'w+')
	# iref_str = "\n".join(list(map(lambda (x, y): x, atoms)))
	# print(iref_str)
	# f.write(iref_str)
	# f.close()
