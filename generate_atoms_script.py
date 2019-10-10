from __future__ import unicode_literals

import sys
import re
import os

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

def write_to_file(string, filename):
	f = open(filename, 'w+')
	f.write(string)
	f.close()

if __name__=='__main__':

	tex_file = sys.argv[1]
	f = open(tex_file, 'r')
	tex_str = f.read()
	begin_objects = re.findall(r"\\begin{[ \w-]*}", tex_str)
	end_objects = re.findall(r"\\end{[ \w-]*}", tex_str)

	i = 0
	j = 0
	# while (i < len(begin_objects) and j < len(end_objects)):
	# 	begin_s, begin_e = begin_objects[i].start(), begin_objects[i].end()
	# 	end_s, end_e = end_objects[j].start(), end_objects[j].end()
	# 	begin = begin_objects[i]
	# 	if (i+1 < len(begin_objects) and begin_objects[i+1].start() > :


	# begin_object = re.search(r"\\begin{[ \w-]*}", tex_str)

	while(begin_object):
		s, e = begin_object.start(), begin_object.end()
		# arg = tex_str[s+len("\\begin{"):e-1]
		tex_str = tex_str[e:]
		end_object = re.search(r"\\end{[ \w-]*}", tex_str)
		next_begin_object = re.search(r"\\begin{[ \w-]*}")




	