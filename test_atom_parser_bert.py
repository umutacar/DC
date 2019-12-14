from __future__ import unicode_literals

import sys
import re
import os
import json
from bert_embedding import BertEmbedding
import numpy as np

bert_embedding = BertEmbedding()

# reload(sys)
# sys.setdefaultencoding('utf8')

# local imports
import constants

# constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")
TEX_FILE_DIRECTORY = 'tex_sources/'
TEX_FILES = ['test_ch1.tex', 'test_ch3.tex', 'test_ch4.tex'] # , 'test_ch5.tex', 'test_ch6.tex', 'test_ch7.tex', 'test_ch8.tex']

def sentence_indices(sentences):
	sentences_length = list(map(lambda x: len(x) + 1, sentences)) # +1 for the '.'
	cumulative_sentence_length_list = []
	cumulative_length = 0
	for length in sentences_length:
		cumulative_sentence_length_list.append(cumulative_length)
		cumulative_length += length
	return cumulative_sentence_length_list

def find_sentence_index(atom, index):
	sentence_length = sentence_indices(atom.strip().split('.'))
	if (index == 0):
		return 0
	elif (index >= sentence_length[-1]):
		return len(sentence_length) - 1
	i = 0
	while (sentence_length[i] <= index):
		i+=1
	return i - 1

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

def atoms_keyphrase_indices_to_file(filename, atom_indices):

	with open(filename, "w+") as write_file:
		json_str = json.dumps(atom_indices, indent=2)
		write_file.write(json_str)
		write_file.close()

def parseAtoms(tex):
	atoms = []
	tex = tex.strip()
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

def extract_keyphrase_definitions(tex_string, keyphrases, definitions):
	for keyphrase in keyphrases:
		# print('keyphrase ', keyphrase)
		defn_index = tex_string.find('\\defn{' + keyphrase + '}')
		def_index = tex_string.find('\\def{' + keyphrase + '}')
		if (defn_index == -1 and def_index == -1):
			continue
		if (defn_index != -1):
			index = defn_index
		else:
			index = def_index
		# print(index)
		prev_sentence_index = tex_string[:index].rfind('.')
		prev_newline_index = tex_string[:index].rfind('\n')
		# print(prev_sentence_index, prev_newline_index)
		next_sentence_index = tex_string[index:].find('.')
		# print(next_sentence_index)
		definition = tex_string[max(prev_sentence_index, prev_newline_index) + 1:index+next_sentence_index]
		definitions[keyphrase] = definition
	return definitions

def find_embedding_index(embedding_sentence, keyphrase):
	keyphrase_split = keyphrase.lower().strip().split()
	length_of_keyphrase = len(keyphrase_split)
	# print(length_of_keyphrase)
	for i in range(len(embedding_sentence) - length_of_keyphrase + 1):
		if (embedding_sentence[i:i+length_of_keyphrase] == keyphrase_split):
			return i
	return -1
		
# if definition label doesn't show up in the embedding, that means it was cut off by bert
def reextract_definition_embeddings(keyphrase, clean_definition, embedding):
	keyphrase_split = keyphrase.lower().split()
	length_of_keyphrase = len(keyphrase_split)
	definition_words_list = clean_definition.lower().strip().split()
	index = find_embedding_index(definition_words_list)


	# return desired bert embedding

def create_definition_embeddings(definition_dict):
	definition_list = definition_dict.items()
	keyphrases = list(map(lambda x: x[0], definition_list))
	definitions = list(map(lambda x: x[1], definition_list))
	definitions_clean = list(map(lambda x: re.sub(r"\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", x), definitions))
	# definitions_clean = list(map(lambda x: re.sub(r"\\defn{[^\}].*?}|\{|\}|\\\[[^\\\]].*?\\\]|\(|\)|\\|\"|\'|\`|,|\\\w+\b|\$[^\$].*?\$", " ", x), definitions_clean))
	# print(definitions_clean[-10:])
	embeddings = bert_embedding(definitions_clean)
	embedding_dict = {}
	for i in range(len(keyphrases)):
		keyphrase = keyphrases[i]
		keyphrase_split = keyphrase.lower().split()
		length_of_keyphrase = len(keyphrase_split)

		definition_clean_list = definitions_clean[i].strip().lower().split()


		embedding_sentence = embeddings[i][0]
		embedding_vectors = embeddings[i][1]

		current_index = find_embedding_index(embedding_sentence, keyphrase)
		back_up_index = find_embedding_index(definition_clean_list, keyphrase)
		index = -1
		if (back_up_index == -1):
			exit()
		elif (current_index == -1):
			end_index = back_up_index + length_of_keyphrase
			begin_index = end_index - 20
			new_definition_sentence = " ".join(definition_clean_list[begin_index:end_index])
			print(keyphrase, " had issues with finding embeddings: ", back_up_index)
			print(new_definition_sentence)
			def_embeddings = bert_embedding([new_definition_sentence])

			embedding_sentence = def_embeddings[0][0]
			embedding_vectors = def_embeddings[0][1]
			index = find_embedding_index(embedding_sentence, keyphrase)


		else:
			index = current_index
		avg_embedding = np.mean(embedding_vectors[index: index + length_of_keyphrase], axis=0)
		avg_embedding /= np.linalg.norm(avg_embedding)
		embedding_dict[keyphrase] = avg_embedding
	return embedding_dict

def filter_false_positives(definition_embedding, atoms, json_data):
	results = []
	for i in range(13, 22):#len(atoms)): # json_dict : dict(keyphrase) => list of indices
		print("looking at atom ", i)
		json_dict = json_data[i-13]
		atom = atoms[i]
		filtered_dict = filter_false_positives_per_atom(atom, json_dict, definition_embedding)
		results.append(filtered_dict)
	return results

def remove_math_env(sentence):
	temp = list(sentence)
	flag = 0 #  0 is copy, 1 is for $, and 2 is for $$, and 3 is for \[ \]
	for i in range(len(temp)):
		if (flag == 0):
			if (temp[i] == '$'):
				if (i > 0 and temp[i-1] == '\\'):
					flag = 0
				elif (i < len(temp) - 1 and temp[i+1] == "$"):
					flag = 2
				else:
					flag = 1
		elif (flag == 1):
			if (temp[i] == '$' and temp[i-1] != '\\'):
				flag = 0
				temp[i] = ' '
		elif (flag == 2):
			if (temp[i] == '$' and temp[i+1] == '$'):
				flag = 0
				temp[i] = ' '
				temp[i+1] = ' '
		if (flag != 0):
			temp[i] = " "
	return ''.join(temp)

def find_phrase_embedding(sentence, index_tuple):
	index, length, phrase = index_tuple[0], index_tuple[1], index_tuple[2]
	prefix = sentence[:index]
	suffix = sentence[index + length:]

	prefix = prefix.split()
	suffix = suffix.split()
	assert(sentence[index:index+length] == phrase)
	num_words = len(phrase.split())
	embedding_size = 10
	embedding_size -= num_words
	if (embedding_size // 2 < len(prefix)):
		prefix = prefix[-1*(embedding_size // 2):]
	if (embedding_size // 2 < len(suffix)):
		suffix = suffix[:embedding_size // 2]
	new_sentence = ' '.join(prefix) + ' ' + phrase + ' ' + ' '.join(suffix)
	embedding = bert_embedding([new_sentence])
	return embedding


def filter_false_positives_per_atom(atom, json_dict, definition_embeddings):
	atom = re.sub(r"``", "  ", atom)
	atom = re.sub(r"''", "  ", atom)
	atom = re.sub(r",", " ", atom)
	atom = re.sub(r"\?", " ", atom)
	dirty_sentences = atom.split('.')
	sentences = list(map(lambda x: remove_math_env(x), dirty_sentences))
	for i in range(len(sentences)):
		assert(len(sentences[i]) == len(dirty_sentences[i]))
	s_indices = sentence_indices(sentences)
	sentence_embeddings = bert_embedding(sentences)
	final_dict = {}
	for (prefix, suffix_dict) in json_dict.items():
		for (suffix, indices_list) in suffix_dict.items():
			keyphrase = prefix + suffix
	# for (keyphrase, indices) in json_dict.items():
			definition_embedding = definition_embeddings[keyphrase]
			results = []
			for index_pair in indices_list:
				index, length, phrase = index_pair[0], index_pair[1], index_pair[2]
				num_of_words = len(phrase.strip().split())
				sentence_index = find_sentence_index(atom, index)
				new_index = index - s_indices[sentence_index]
				new_index_pair = (new_index, length, phrase)
				
				sentence = sentences[sentence_index]
				assert(sentence.find(phrase) != -1)
				sentence_embedding = sentence_embeddings[sentence_index][0]
				vector_embedding = sentence_embeddings[sentence_index][1]
				# find embeddings of phrase within the sentence
				phrase_index = find_embedding_index(sentence_embedding, phrase)

				if (phrase_index == -1):
					print("BUG: COULDN'T FIND CORRECT PHRASE EMBEDDING INDEX FOR ", keyphrase, index_pair)
					embedding = find_phrase_embedding(sentence, new_index_pair)
					assert(len(embedding) == 1)
					sentence_embedding = embedding[0][0]
					vector_embedding = embedding[0][1]
					phrase_index = find_embedding_index(sentence_embedding, phrase)
					# results.append(index_pair)
				phrase_embedding = vector_embedding[phrase_index: phrase_index + num_of_words]
				avg_phrase_embedding = np.mean(phrase_embedding, axis=0)
				avg_phrase_embedding /= np.linalg.norm(avg_phrase_embedding)

				similarity = np.dot(definition_embedding, avg_phrase_embedding)
				if (similarity >= 0.8):
					print("success!", keyphrase, phrase, similarity)
					results.append(index_pair)
			final_dict[keyphrase] = results
	return final_dict

if __name__=='__main__':
	json_file = sys.argv[1] # label_detection_results/11_20_21_ch1.json

	# extracting atoms from latex file
	tex_file = sys.argv[2]
	output_file = sys.argv[3]
	f = open(tex_file, 'r')
	latex_chapter = f.read()
	latex_atoms = parseAtoms(latex_chapter)

	# extracting bert embedding of definitions
	def_to_label = file_to_dictionary_list("dictionary.txt")
	# keyphrase_dict = nested_dictionary_list(def_to_label)
	definitions = list(map(lambda x: x[0], def_to_label))
 
	definition_sentences = {}
	for chapter in TEX_FILES:
		path = TEX_FILE_DIRECTORY + chapter
		print('looking at chapter ', path)
		with open(path, 'r') as file:
			tex_string = file.read()
			definition_sentences = extract_keyphrase_definitions(tex_string, definitions, definition_sentences)
	# print("definition sentences: ", definition_sentences)
	embedding_definitions = create_definition_embeddings(definition_sentences)
	with open(json_file, "r") as read_file:
		data = json.load(read_file)
		results = filter_false_positives(embedding_definitions, latex_atoms, data)
		print(results)
		atoms_keyphrase_indices_to_file(output_file, results)
