from __future__ import unicode_literals

import sys
import re
import os



tex_file = sys.argv[1]
f = open(tex_file, 'r')
tex_str = f.read()
atoms_string = ""
while(tex_str != ""):
	end_obj = re.search(r"\\end{[ \w-]*}", tex_str)
	# there's no end left
	if (not end_obj):
		break
	prefix = tex_str[:end_obj.end()]
	tex_str = tex_str[end_obj.end():]
	# begin_obj = re.search(r"begin{[ \w-]*}", prefix)
	last_begin_before_end_obj = prefix.rfind("\\begin{")
	if (last_begin_before_end_obj == -1):
		end_obj = re.search(r"\\end{[ \w-]*}", tex_str)
	else:
		atoms_string += prefix[last_begin_before_end_obj:]
		atoms_string += "\n"

print(atoms_string)

f = open("test.txt", 'w+')
f.write(atoms_string)
f.close()