from parse import *
import Queue
import sys
import re
import os

# constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")
GROUPING_ARGS = ['cluster', 'flex']
MATH_ARGS = ['math', 'equation', 'align', 'array', 'table', 'tabular']

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
	stored_list.reverse()
	result_str = ""
	file_obj = open(file_name, "w+")
	for keyphrase, label in stored_list:
		result_str += keyphrase + "\t"
		result_str += label + "\n"
	file_obj.write(result_str)
	file_obj.close()

def file_to_dictionary_list(dict_str):
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
	# return label_to_atom

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
		print("1")
		return True
	elif ((string.count("$", 0, index) - num_literal_dollar_signs) % 2):
		print('2')
		return True
	elif (string.count("\[", 0, index) - string.count("\]", 0, index) == 1):
		print('3')
		return True
		begin_arg = re.search(string)
	return False

# given string and map of keyphrases to labels, identify the key pheases
# and insert the labels as necessary
def insert_label(string, def_to_Label):
	lowercase_copy = string.lower()
	str_iref = ""
	for definition, label in def_to_Label.items():
		prev_end = 0
		# only use lowercase_copy for finding occurrence case-insensitive
		index = lowercase_copy.find(definition.lower())
		str_iref = ""
		while (index != -1):
			# if string[index: index+len(definition)] is valid key phrase
			if (is_math_expression(string, index)):
				print 'math expression' + '\t' + string[index - 10: index + 10]
			elif (is_argument(string, index)):
				print "argument" + '\t' + string[index - 10: index + 10]
			elif (is_command(string, index)):
				print "command" + '\t' + string[index - 10: index + 10]
			else:
				print "instance2" + '\t' + string[index - 10: index + 10]
				iref = "\\iref{" + label + "}{" + definition + "}"

				str_iref += string[prev_start:start]
				str_iref += iref
				# if keyphrase is replaced by label, update prev_start to be last updated index
				prev_end = index + len(definition)
			index = lowercase_string.find(definition.lower(), index+len(definition))
		str_iref += string[prev_end:]
		string = str_iref
		lowercase_string = string.lower()
	return string

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
			def_list = file_to_dictionary_list(dictionary_str)
			output_file = sys.argv[4]
			new_latex = insert_label(tex_str, def_list)
			output = open(output_file, "w+")
			output.write(new_latex)
			output.close()
		else:
			print("wrong mode. Mode should either be `extract_label` or `insert_label`")
	else:
		print("no argument was provided. Please provide a latex file you'd like to parse")
