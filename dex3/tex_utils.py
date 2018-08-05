CHAPTER = 'chapter'
LABEL = 'label'


def mk_section_heading (heading, title):
  SLASH + heading + CURLY_BRACKET_OPEN + title + CURLY_BRACKET_CLOSE 

def mk_label (label): 
  SLASH + LABEL + CURLY_BRACKET_OPEN + label + CURLY_BRACKET_CLOSE

def mk_chapter (title, label, intro, contents):
  result = \
    mk_section_heading(CHAPTER, title) + \
    self.intro + NEWLINE + \
    self.contents + NEWLINE + \
    LATEX_END_CHAPTER + NEWLINE 
  return result

def mk_group (contents):
  return contents
