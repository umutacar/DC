from __future__ import unicode_literals

import en_core_web_md

import sys
import re
import os
import spacy
import json
import argparse
from itertools import combinations

reload(sys)
sys.setdefaultencoding('utf8')

# local imports
import constants


nlp = spacy.load('en_core_web_md', disable=['ner'])
nlpL = spacy.load('en_core_web_md', disable=['ner', 'parser'])
nlpL.add_pipe(nlpL.create_pipe('sentencizer'))

print(os.getcwd())
sys.path.append(os.getcwd())
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
def overlapping_substring(phrases_seen, curr_phrase):
	index, length = curr_phrase
	non_overlapping_phrases = list(filter(lambda x: index + length < x[0] or x[0] + x[1] < index, phrases_seen))
	# overlapping_phrases = list(filter(lambda x: index <= x[0] and index+length >= x[0]+x[1], phrases_seen))
	return (len(phrases_seen) != len(non_overlapping_phrases))

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

def extract_definitions_from_chapter(chapter_file):
	def_to_labels = []
	with open(chapter_file, "r") as read_file:
		chapter = read_file.read()
		# defn_list = re.findall(r'\\defn{[ \w-]*}', chapter)
		# def_list = re.findall(r'\\def{[ \w-]*}', chapter)
		# defn_keyphrase = list(set([defn[6:-1] for defn in defn_list] + [defn[5:-1] for defn in defn_list]))
		
		begin_index = 0
		begin_definition = re.search(r'\\begin{definition*}', chapter)
		while (chapter and begin_definition):
			end_definition = re.search(r'\\end{definition*}', chapter)
			# finds \\defn{} per atom
			atom = chapter[begin_definition.start(): end_definition.end()]

			def_list, defn_list = re.findall(r'\\def{[ \w-]*}', atom), re.findall(r'\\defn{[ \w-]*}', atom)
			definitions = [defn[6:-1] for defn in defn_list] + [defn[5:-1] for defn in def_list]
	
			label = re.findall(r'\\label{.*}', atom)
			if (len(label) != 1):
				print("Error: there exists multiple or no labels for definition ", definitions)
			else:
				for defn in definitions:
					def_to_labels.append( [defn, label[0]] )

			chapter = chapter[end_definition.end():]
			begin_definition = re.search(r'\\begin{definition*}', chapter)
	return def_to_labels

# given: no dictionar.txt is declared
# return: list of [keyphrase, label] pairs
def create_dictionary(chapter_files):
	dictionary = []
	for file in chapter_files:
		chapter_keyphrases = extract_definitions_from_chapter(file)
		dictionary += chapter_keyphrases
	dictionary.sort(key=lambda x: len(x[0]), reverse=True)
	return dictionary

def read_from_json(filename):
	with open(filename, "r") as read_file:
		data = json.load(read_file)
		return data

def write_to_json(filename, atom_indices):
	# filter out results that are empty
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

def is_valid_expression(string, index):
	return not (is_math_expression(string, index) \
				or is_command(string, index) \
				or is_argument(string, index))

# given string, label, and map of keyphrases to indices, insert the labels as necessary
# def insert_label_for_keyphrase(string, keyphrase, label, keyphrases_to_indices):
# 	sentences = string.split(".")
# 	sentences_length = list(map(lambda x: len(x) + 1, sentences)) # +1 for the '.'
# 	cumulative_slen_list = sentence_indices(sentences)

# 	str_iref = ""
# 	iref = "\\iref{" + label + "}{" + keyphrase + "}"

# 	sentences_indices = keyphrases_to_indices[keyphrase]
# 	prev_end = 0
# 	for (index, phrase_len) in sentences_indices:
# 		# sentence = sentences[sentence_id]
# 		# find sentence index from word_id
# 		# index = cumulative_slen_list[sentence_id] + sentence_index

# 		# check if the keyphrase is in the right environment
# 		if (is_math_expression(string, index)):
# 			print('math expression' + '\t' + string[index - 10: index + 10])
# 		elif (is_argument(string, index)):
# 			print("argument" + '\t' + string[index - 10: index + 10])
# 		elif (is_command(string, index)):
# 			print("command" + '\t' + string[index - 10: index + 10])
# 		else:
# 			print("is_valid" + '\t' + string[index - 10: index + 10])

# 			str_iref += string[prev_end:index]
# 			str_iref += iref
# 			prev_end = index + phrase_len

# 			# update other indices list in keyphrases_to_indices
# 			for (new_keyphrase, indices_list) in keyphrases_to_indices.items():
# 				if (len(new_keyphrase) > len(keyphrase)):
# 					continue
# 				for i in range(len(indices_list)):
# 					# new_string_id, new_string_index,
# 					(new_index, new_phrase_len) = indices_list[i]
# 					# new_index = cumulative_slen_list[new_string_id] + new_string_index
# 					increment = len(iref) - phrase_len



# 					# only look at indices after current index
# 					if (new_index > index):
# 						new_index += increment
# 						indices_list[i] = (new_index, new_phrase_len)
# 				keyphrases_to_indices[new_keyphrase] = indices_list

# 	str_iref += string[prev_end:]
# 	return str_iref, keyphrases_to_indices

# given string, map of keyphrases to labels, and map of keyphrases to indices, insert the labels as necessary
# def insert_label(string, def_to_label, atom_keyphrase_to_indices):
# 	atoms = []
# 	for i in range(len(atom_keyphrase_to_indices)):
# 		str_iref = string[i]
# 		keyphrase_to_indices = atom_keyphrase_to_indices[i]
# 		for definition, label in def_to_label:
# 			if (definition in keyphrase_to_indices):
# 				str_iref, keyphrase_to_indices = insert_label_for_keyphrase(str_iref, definition, label, keyphrase_to_indices)
# 	atoms.append( (str_iref, keyphrase_to_indices) )	
# 	return atoms

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
			if ("\\label{" in atom[0]):
				atom = atom[1:]
			atom = "\n".join(atom)
			atoms.append(atom)
			tex = tex[end+end_len:]
		return atoms

	# input: atom text
	def getPhrases(text):
		phrases_numbered = []
		phrases = []
		parsed_string = re.sub(r"``", "  ", text)
		parsed_string = re.sub(r"''", "  ", parsed_string)
		parsed_string = re.sub(r",", " ", parsed_string)
		cleaned_sentences = parsed_string.split('.')
		sentences = text.split('.')
		cumulative_slen_list = sentence_indices(sentences)
		# testing sentences 
		for i in range(len(cleaned_sentences)):
			clean_sentence = cleaned_sentences[i]
			# clean_sentence_doc = nlpL(clean_sentence)
			if clean_sentence == '':
				continue
			nonstopword_index_list = nonstopword_indices(clean_sentence)
			for (s, length) in nonstopword_index_list:
				curr_phrase = clean_sentence[s:s+length]
				stripped_phrase = curr_phrase.strip()
				try:
					index = cumulative_slen_list[i] + s
					if (is_valid_expression(text, index)):
						phrases_numbered.append( (index, curr_phrase.strip(), lemmatize(nlpL(stripped_phrase.decode()))) )
				except:
					print("Indexing error in sentence, " + sentences[i], " for word index " + str(s))

			for ((s1, len1), (s2, len2)) in combinations(nonstopword_index_list, 2):
				curr_phrase = clean_sentence[s1:s2 + len2]
				stripped_phrase = curr_phrase.strip()
				try:
					index = cumulative_slen_list[i] + s1
					if (is_valid_expression(text, index)):
						phrases_numbered.append( (index, curr_phrase.strip(), lemmatize(nlpL(stripped_phrase.decode()))) )
				except:
					print("Indexing error in sentence, " + sentences[i], " for word index " + str(s1))
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
	for i in range(1):# len(atoms)):
		atom = atoms[6]
		sentences = atom.decode().strip().split('.')
		# src_textL: [(index, str, str, lemmatize phrase)]
		src_textL = getPhrases(atom.decode())
		print "Finished extracting phrases for atom " + str(i)
		implicit_ref_to_index = {}
		phrases_seen = set([])
		for keyphrase, label in keyphrase_to_label:
			implicit_ref_keyphrase = []
			keyphrase_lemma = lemmatize(nlpL(keyphrase.decode()))
			nlp_keyphrase_lemma = nlp(keyphrase_lemma.decode())

			for (index, curr_phrase, token_s) in src_textL:
				nlp_token_s = nlp(token_s.decode())
				if (abs(len(nlp_keyphrase_lemma.text) - len(token_s)) >= 5):
					continue
				curr_sim = check_similarity(nlp_token_s, nlp_keyphrase_lemma)
				if (curr_sim >= 1.0 and not overlapping_substring(phrases_seen, (index, len(curr_phrase)) )):
					# sentence_index = find_sentence_index(atom, index)
					# sentence = sentences[sentence_index]
					# if (checkMatch(atom, index, curr_phrase)):
					print("New Match. Matched phrase '%s' to keyphrase '%s'" % (curr_phrase, keyphrase))
					implicit_ref_keyphrase.append( (index, len(curr_phrase), curr_phrase) )
					phrases_seen.add( (index, len(curr_phrase)) )
			no_dup_keyphrases = list(set(implicit_ref_keyphrase))
			no_dup_keyphrases.sort(key=lambda x: x[0])
			implicit_ref_to_index[keyphrase] = no_dup_keyphrases
		atom_to_iref_list.append(implicit_ref_to_index)
	return atom_to_iref_list

if __name__=='__main__':

	parser = argparse.ArgumentParser()
	parser.add_argument('--tex_file', type=str)
	parser.add_argument('--output_file', type=str)
	args = parser.parse_args()

	f = open(args.tex_file, 'r')
	tex_str = f.read()
	files = [f for f in os.listdir('.') if os.path.isfile(f)]

	if ("dictionary.json" not in files):
		tex_sources = [f for f in os.listdir('./tex_sources/') if 'Chapter' in f]
		dictionary = create_dictionary(list(map(lambda x: "./tex_sources/" + x, tex_sources)))
		write_to_json("dictionary.json", dictionary)
	# TODO: if a new file is being labeled, need to add keyphrases to dictionary

	def_to_label = read_from_json("dictionary.json")
	print("def to labels: ", def_to_label)
	print "Finding implicit references..."
	# result is a list of lists of (keyphrase, implicit reference substitutes)
	result = identify_implicit_refs(tex_str, def_to_label)
	print(result)
	print "Writing implicit references to file..."
	for i in range(len(result)):
		atom = result[i]
		for (keyphrase, indices) in atom.items():
			if indices == []:
				del atom[keyphrase]
		result[i] = atom
	g = write_to_json(args.output_file, result)

	# print "Getting keyphrases and irefs from file..."