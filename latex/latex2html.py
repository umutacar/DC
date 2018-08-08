######################################################################
## latex/latex2html.py
##
######################################################################

import os
import re

import pervasives.os_utils as os_utils
import pervasives.syntax  as syntax
# import pypandoc

######################################################################
## BEGIN: Globals

# prep string for conversion
def text_prep(s):
  # Replace NEWLINE with SPACE + NEWLINE
  # This prevents some math conversion problems by making sure that 
  # operators have a space after them in case they had a NEWLINE
  result = s.replace(syntax.NEWLINE, syntax.SPACE + syntax.NEWLINE)
  return result 



## generate standalone html files
##
# You can use Mathml or mathjax but 
# mathml is not supported natively by many browsers

#PANDOC_STANDALONE = r'pandoc --mathml -s'
PANDOC_STANDALONE = r'pandoc --from latex+smart  --mathjax -s'
PANDOC_STANDALONE = r'pandoc  --mathjax -s'


# generate non-standalone html files
PANDOC_MINOR = r'pandoc --from latex+smart  --mathjax'
PANDOC_MINOR = r'pandoc --mathjax'


# command to be used 
#PANDOC = 'echo ' + PANDOC_MINOR
PANDOC =  PANDOC_MINOR
#PANDOC =  PANDOC_STANDALONE

LATEXML = 'latexml'
LATEXMLPOST = 'latexmlpost -pmml --javascript=LaTeXML-maybeMathJax.js'

PATTERN_HTML_PARAGRAPH = re.compile(r'<p>(?P<contents>.*)</p>\n*')

LATEX_BEGIN_DOCUMENT = r'\begin{document}'
LATEX_END_DOCUMENT = r'\end{document}'

## END: Globals
######################################################################

def extract_html_body(file):
  inRecordingMode = False
  for line in file:
    if not inRecordingMode:
      if line.startswith(r'<body>'):
        print('caught <body>')
        inRecordingMode = True  
    elif line.startswith(r'</body>'):
      print('caught </body>')
      inRecodingMode = False
    else:
      yield line

def do_latex_to_html_pandoc (latex_file_name, html_file_name):
    ## Beware: pandoc converts everything to unicode
    ## HTML is therefore unicode string.
    ## This matters when printing to terminal which is ASCII
    command = PANDOC + syntax.SPACE + syntax.SPACE + latex_file_name + syntax.SPACE +  '-o' + html_file_name 
    os_utils.run_command(command)
    print('LaTex code is in file:', latex_file_name)
    print('HTML code is in file:', html_file_name)

def do_latex_to_html_latexml(latex_file_name, html_file_name):

#    (name_first, name_ext) = latex_file_name.split (syntax.PERIOD) 
#    xml_file_name = name_first + os_utils.XML_EXTENSION
    xml_file_name = os_utils.mk_file_name_ext (latex_file_name, os_utils.XML_EXTENSION)
  
    # Convert to XML    
    command = LATEXML + syntax.SPACE + \
              '--dest' + syntax.SPACE + xml_file_name + syntax.SPACE + \
              latex_file_name 
    os_utils.run_command(command)
    
    # Convert to HTML
    html_file_name_tmp = html_file_name + syntax.PERIOD + 'html'
    command = LATEXMLPOST + syntax.SPACE + \
              '--dest' + syntax.SPACE + html_file_name_tmp + syntax.SPACE + \
              xml_file_name
    os_utils.run_command(command)

    # Extract body
    html_file_tmp = open(html_file_name_tmp, 'r')
    body_file = extract_html_body (html_file_tmp)
    html_file = open(html_file_name, 'w')
    for line in body_file:
      html_file.write(line) 
    html_file_tmp.close()    
    html_file.close()    
    
    print('LaTex code is in file:', latex_file_name)
    print('HTML code is in file:', html_file_name)


## END: Globals
######################################################################

######################################################################
## Translate latex (contents) to html
## tmp_dir is /tmp/ or similar
## unique is a unique name that can be used for translation files
## preamble is the preamble file
## contents is the contents to be translated
## match specifies that what is expected is a single paragraph
##
def latex_to_html (tmp_dir,  unique, preamble, contents, match_single_paragraph):
  # prep for translation
  contents = text_prep(contents)

  latex_file_name = tmp_dir + r'/' + unique + syntax.PERIOD + os_utils.LATEX_EXTENSION 
  latex_file = open(latex_file_name, 'w')

  latex_file.write(preamble + '\n')
  latex_file.write(LATEX_BEGIN_DOCUMENT + '\n')
  latex_file.write(contents + '\n')
  latex_file.write(LATEX_END_DOCUMENT + '\n')
  latex_file.close()

  ## translate to html
  html_file_name = tmp_dir + r'/' + unique + syntax.PERIOD + os_utils.HTML_EXTENSION

  do_latex_to_html_pandoc (latex_file_name, html_file_name)     
  ##do_latex_to_html_latexml (latex_file_name, html_file_name)     

  html_file = open(html_file_name, 'r')  
  result = html_file.read ()
  if match_single_paragraph:
    m = PATTERN_HTML_PARAGRAPH.match(result)
    if m: 
#        print 'latex2html.translate: pattern matched'
      result = m.group('contents')
#        print 'matched contents:', result
    else:
      print('FATAL ERROR: latex2html.translate: pattern did not match') 

 #   print 'latex2html.translate:', result
  return result
######################################################################

######################################################################
## BEGIN: Class Latex2Html
class Latex2Html:
  def __init__ (self, tmp_dir, preamble):
    self.tmp_dir = tmp_dir
    self.preamble = preamble

    # Create tmp dir
    command = 'mkdir '+ self.tmp_dir
    os_utils.run_command_no_check(command)

  ## Translatex contents (latex) to html
  def translate (self,  unique, contents, match_single_paragraph):

    # prep for translation
    contents = text_prep(contents)

    # TODO: get a wierd ascii error when trying to use this
    # text = self.preamble + '\n' + contents + '\n' + self.postamble + '\n'
    # output = pypandoc.convert_text(text, "html", format="tex", extra_args=['--mathjax'])
    # print(output)
    # return output
    result = latex_to_html(self.tmp_dir, unique, self.preamble, contents, match_single_paragraph)
    return result

## END: Class Latex2Html
######################################################################

## Older version based on preamble_file instead of preamble string
# ## BEGIN: Class Latex2Html
# class Latex2Html:
#   def __init__ (self, tmp_dir, preamble_file_name, postamble):
#     self.tmp_dir = tmp_dir
#     self.preamble_file_name = preamble_file_name
#     self.postamble = postamble

#     # Create tmp dir
#     command = 'mkdir '+ self.tmp_dir
#     os_utils.run_command_no_check(command)

#     preamble_file = open(self.preamble_file_name, 'r')
#     self.preamble = preamble_file.read ()

#   ## Translatex contents (latex) to html
#   def translate (self,  unique, contents, match_single_paragraph):

#     # prep for translation
#     contents = text_prep(contents)

#     # TODO: get a wierd ascii error when trying to use this
#     # text = self.preamble + '\n' + contents + '\n' + self.postamble + '\n'
#     # output = pypandoc.convert_text(text, "html", format="tex", extra_args=['--mathjax'])
#     # print(output)
#     # return output

#     latex_file_name = self.tmp_dir + r'/' + unique + syntax.PERIOD + os_utils.LATEX_EXTENSION 
#     latex_file = open(latex_file_name, 'w')

#     latex_file.write(self.preamble + '\n')
#     latex_file.write(contents + '\n')
#     latex_file.write(self.postamble + '\n')
#     latex_file.close()

#     ## translate to html
#     html_file_name = self.tmp_dir + r'/' + unique + syntax.PERIOD + os_utils.HTML_EXTENSION

#     do_latex_to_html_pandoc (latex_file_name, html_file_name)     
#     ##do_latex_to_html_latexml (latex_file_name, html_file_name)     

#     html_file = open(html_file_name, 'r')  
#     result = html_file.read ()
#     if match_single_paragraph:
#       m = PATTERN_HTML_PARAGRAPH.match(result)
#       if m: 
# #        print 'latex2html.translate: pattern matched'
#         result = m.group('contents')
# #        print 'matched contents:', result
#       else:
#         print 'FATAL ERROR: latex2html.translate: pattern did not match' 

#  #   print 'latex2html.translate:', result
#     return result

# ## END: Class Latex2Html
# ######################################################################


# #  Create an Latex2Html object
# Latex2Html = Latex2Html(\
#                TMP_DIR, \
#                LATEX_PREAMBLE_FILE, \
#                LATEX_POSTAMBLE)

