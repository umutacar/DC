from parse import *
import Queue
import sys
import re

# constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")
GROUPING_ARGS = ['cluster', 'flex']
MATH_ARGS = ['math', 'equation', 'align', 'array', 'table', 'tabular']

# given a dictionary of key phrases mapped to their labels, write them to file_name
def write_to_file(dictionary, file_name):
	output = open(file_name, "a+")
	result_str = ""
	for keyphrase, label in dictionary.items():
		result_str += keyphrase + "\t"
		result_str += label + "\n"
	print result_str
	output.write(result_str)
	output.close()

def file_to_dictionary(dict_str):
	lines = [d.split("\\label{") for d in dict_str.strip().split("\n")]
	print(lines)
	# last element of dict_list is an empty list
	dict_tuples = list(map(lambda x: (x[0].strip(), (x[1][:-1]).strip()), lines))
	dictionary = dict(dict_tuples)
	return dictionary

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
		# print(atom, definitions, label)
		tex_str = tex_str[end_definition.end():]
		begin_definition = re.search(r'\\begin{definition*}', tex_str)
		if (len(label) != 1):
			print("Error: there exists multiple or no labels for an atom")
			break
		for defn in definitions:
			label_to_atom[defn] = label[0]
	return label_to_atom

	# while (len(tex_str)):
	# 	token_arg = find_atom_word(tex_str)
	# 	(begin_index, end_index) = find_atom(tex_str)
		
	# 	if (begin_index == -1 and end_index == -1):
	# 		tex_str = ""
	# 	# elif (not (token_arg.strip() in GROUPING_ARGS) and token_arg != "example"):
	# 	elif (token_arg.strip() in ["definition", "theorem"]):
	# 		atom = tex_str[begin_index:end_index]
	# 		tex_str = tex_str[end_index:]
	# 		label_index = atom.find("\\label{")

	# 		keyphrases = find_keyphrases(atom)
	# 		if (label_index != -1 and keyphrases != []):
	# 			end_label_index = atom.find("}", label_index)
	# 			label = atom[label_index + BEGIN_LEN:end_label_index]
	# 			for keyphrase in keyphrases:
	# 				label_to_atom[keyphrase] = label
	# 	else:
	# 		tmp = tex_str.strip().split('\n')
	# 		tex_str = "\n".join(tmp[1:])

# return true if end of string[:index] contains an argument to a command
def is_argument(string, index):
	last_open_brace = string.rfind("{", 0, index)
	last_close_brace = string.rfind("}", 0, index)
	last_literal_brace = string.rfind("\\{", 0, index)
	# if most recent open brace is after the most recent close brace, 
	# the index is probably an argument
	# print(string[index:index+30])
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
		begin_arg = string.find('\\begin{' + arg + '}', 0, index)
		if (begin_arg != -1):
			end_arg = string.rfind(r'\\end{' + arg + r'}', 0, index)
			if (end_arg == -1):
				return True
	return False

# given string and map of keyphrases to labels, identify the key pheases
# and insert the labels as necessary
def insert_label(string, def_to_label):
	lowercase_string = string.lower()
	str_iref = ""
	for definition, label in def_to_label.items():
		prev_end = 0
		# only use lowercase_string for finding occurrence case-insensitive
		index = lowercase_string.find(definition.lower())
		str_iref = ""
		while (index != -1):
			# if string[index: index+len(definition)] is valid key phrase
			if (not is_math_expression(string, index) 
				and not is_argument(string, index)
				and not is_command(string, index)):
				iref = "\\iref{" + label + "}{" + definition + "}"
				str_iref += string[prev_end:index]
				str_iref += iref
				# if keyphrase is replaced by label, update prev_start to be last updated index
				prev_end = index + len(definition)
			index = lowercase_string.find(definition, index+len(definition))
		str_iref += string[prev_end:]
		string = str_iref
		lowercase_string = string.lower()
	return str_iref

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
	testcase_mathexpr()

	if (len(sys.argv) > 3):
		if (sys.argv[1] == 'extract_label'):
			tex_file = sys.argv[2]
			f = open(tex_file, "r")
			tex_str = f.read()
			dictionary = parser(tex_str)
			write_output = sys.argv[3]
			write_to_file(dictionary, write_output)
		elif (sys.argv[1] == 'insert_label'):
			tex_file = sys.argv[3]
			f = open(tex_file, "r")
			tex_str = f.read()

			dictionary_file = sys.argv[2]
			d = open(dictionary_file, "r")
			dictionary_str = d.read()
			dictionary = file_to_dictionary(dictionary_str)

			output_file = sys.argv[4]
			new_latex = insert_label(tex_str, dictionary)
			output = open(output_file, "w+")
			output.write(new_latex)
			output.close()
		else:
			print("wrong mode. Mode should either be `extract_label` or `insert_label`")
	else:
		print("input must be of form `[extract_label/insert_label] <file_name>`. See README.md for more details")

