import sys

def convert(s):
  s = s.replace("\t", "  ")
  s = s.replace("@", "")
  s = s.replace("=>", "$\Rightarrow$")
  s = s.replace("\cdm", "\cd")
  lines = s.split("\n")
#  print(lines)

  out = "\\[\n"
  out += ("\\begin{array}{ll}\n")

  index = 1
  for l in lines:
    tildes = len(l) - (len(l.lstrip(" ")))
    l = l.lstrip(" ")
    if len(l) != 0:
      build = str(index) + " & "
      build += "~"*tildes
      build += ("\\cd{")
      build += (l)
      build += ("}\n")
      build += ("\\\\\n")

      out += build
      index += 1


  out += ("\\end{array}\n")
  out += ("\\]\n")

  return out

f = open(sys.argv[1])
print(convert(f.read()))
