from __future__ import unicode_literals

import sys
import os
import json
import argparse
from docx import Document

reload(sys)
sys.setdefaultencoding('utf8')

# local imports
import constants
BEGIN = "\\begin{"
END = "\\end{"
BEGIN_LEN = len("\\begin{")
END_LEN = len("\\end{")

def open_json_file(filename):
	with open(filename, "r") as read_file:
		print(read_file)
		data = json.load(read_file)
		return data

def parse_atoms(tex):
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
		# if (begin_index != -1):
		close_bracket = tex[begin_index:].index('}')
		arg = tex[begin_index + BEGIN_LEN: begin_index + close_bracket]
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


def iref_atoms_to_tex(original_atoms, inserted_atoms, tex_string):
	assert(len(original_atoms) == len(inserted_atoms))

	tex = tex_string.decode().strip()
	new_string = ""
	og_atom_last_index = 0
	length = 0
	while (inserted_atoms):
		# popping off first elt from inserted_atoms
		curr_atom = inserted_atoms.pop(0)
		og_atom = original_atoms.pop(0)
		
		# find corresponding index of curr_atom in tex_string
		curr_index = tex.find(og_atom)
		wrapper = tex[og_atom_last_index + length :curr_index]
		og_atom_last_index = curr_index
		length = len(og_atom)
		
		new_string += wrapper
		new_string += curr_atom

	# add last wrapper command
	print(tex[og_atom_last_index+len(og_atom):])
	new_string += tex[og_atom_last_index + length:]
	return new_string

# given string, map of keyphrases to labels, and map of keyphrases to indices, insert the labels as necessary
def insert_iref_per_atom(atom_string, definition_to_labels, keyphrase_to_indices):

	# NOTE: keyphrase_to_indices is in decreasing order based on index.
	for (keyphrase, index, length) in keyphrase_to_indices:
		label = definition_to_labels[keyphrase]
		iref = "\\iref{" + keyphrase + "}{" + label + "}"

		begin_index = index
		end_index = begin_index + length

		atom_string_before = atom_string[:begin_index]
		atom_string_after = atom_string[end_index:]
		atom_string = atom_string_before + iref + atom_string_after

	return atom_string


if __name__=='__main__':
	all_true_pos, all_false_pos, all_false_neg = 0, 0, 0
	parser = argparse.ArgumentParser()
	parser.add_argument('--label_file', type=str)
	parser.add_argument('--input_tex_file', type=str)
	parser.add_argument('--output_tex_file', type=str)
	args = parser.parse_args()
	
	f = open(args.input_tex_file, 'r')
	tex_str = f.read()
	atoms = parse_atoms(tex_str)
	labels = open_json_file(args.label_file)

	dictionary = open_json_file('dictionary.json')
	def_to_labels = {}
	for (keyphrase, label) in dictionary:
		def_to_labels[keyphrase] = label
	inserted_atoms = []
	for i in range(len(atoms)):
		print("ATOM ", i)

		# combining dict: keyphrase -> indices into a comprehensive ordered list
		keyphrases_indices = []
		print "labels " + str(i), labels[i]
		for (keyphrase, indices) in labels[i].items():
			for (index, length, curr_phrase) in indices:
				keyphrases_indices.append( (keyphrase, index, length) )
		keyphrases_indices.sort(key = lambda x: x[1], reverse = True)		
		print("post", keyphrases_indices)

		inserted_iref_atom = insert_iref_per_atom(atoms[i], def_to_labels, keyphrases_indices)
		inserted_atoms.append(inserted_iref_atom)

	inserted_string = iref_atoms_to_tex(atoms, inserted_atoms, tex_str)
	f = open(args.output_tex_file, "w+")
	f.write(inserted_string)
	f.close()