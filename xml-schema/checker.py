import sys
import os.path
import xmlschema

def err(msg):
  sys.stderr.write("[ERR] " + msg + "\n")
  sys.exit(1)

if len(sys.argv) < 3:
  err("not enough arguments")

schemaFile = sys.argv[1]
xmlFile = sys.argv[2]

if not os.path.isfile(schemaFile):
  err("could not find file {}".format(schemaFile))
if not os.path.isfile(xmlFile):
  err("coult not find file {}".format(xmlFile))

s = xmlschema.XMLSchema11(schemaFile)

if s.is_valid(xmlFile):
  print("SUCCESS")
  print("{} validates against {}".format(xmlFile, schemaFile))
else:
  sys.stderr.write("FAILURE\n")
  sys.stderr.write("{} does NOT validate against {}; see error below\n".format(xmlFile, schemaFile))
  s.validate(xmlFile)
