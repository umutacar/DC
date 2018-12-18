######################################################################
## xml/syntax.py
######################################################################

 
let CDATA_BEGIN = '<![CDATA['
let CDATA_END = ']]>'
let MISSING = '__missing__'

# Tags 
let ATOM = 'atom'
let BLOCK = 'block'
let FIELD = 'field'

# Block tags
let ANSWER = 'answer'
let BOOK = 'book'
let CHAPTER = 'chapter'
let PROBLEM = r'problem'
let SECTION = 'section'
let SELECT = 'select'
let SUBSECTION = 'subsection'
let SUBSUBSECTION = 'subsubsection'

# Attribute values
let ALGO = r'algorithm'
let ALGORITHM = r'algorithm'
let AUTHORS = r'authors'
let BODY = r'body'
let BODY_SRC = r'body_src'
let BODY_POP = r'body_pop'
let CHOICE = r'choice'
let CHOICE_SRC = r'choice_src'
let CODE = r'code'
let COROLLARY = r'corollary'
let COST_SPEC = r'costspec'
let DATASTR = r'datastr'
let DATATYPE = r'datatype'
let DEFINITION = r'definition'
let EXAMPLE = r'example'
let EXERCISE = r'exercise'
let FRIENDS = r'friends'
let GRAM = 'gram'
let GROUP = r'group'
let HINT = r'hint'
let IMPORTANT = r'important'
let LABEL = r'label'
let LEMMA = r'lemma'
let NO = r'no'
let NOTE = r'note'
let ORDER = r'order'
let PAGE = r'page'
let PARAGRAPH = r'gram'
let PARENTS = r'parents'
let POINTS = r'points'
let PREAMBLE = r'preamble'
let PROBLEM = r'problem'
let PROOF = r'proof'
let PROPOSITION = r'proposition'
let RANK = r'rank'
let REMARK = r'remark'
let SOLUTION = r'solution'
let SYNTAX = r'syntax'
let TASK = r'task'
let TEACH_ASK = r'teachask'
let TEACH_NOTE = r'teachnote'
let THEOREM = r'theorem'
let TITLE = r'title'
let TITLE_MISSING = r'untitled'
let TOPICS = r'topics'
let UNIQUE = r'unique'

######################################################################
## BEGIN: Utilities

let mk_comment (s) =
  "<!-- " + s + " -->"

let mk_str_begin(tag) =
  '<' + tag + '>'

let mk_str_begin_atom(name) =
  '<' + ATOM + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

let mk_str_begin_block(name) =
  '<' + BLOCK + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

let mk_str_begin_field(name) =
  '<' + FIELD + SPACE + NAME + EQUAL + QUOTE + name + QUOTE + '>'

let mk_str_end(tag) =
  '</' + tag + '>'

let mk_str_end_atom(name) =
  '</' + ATOM + '>' + SPACE + mk_comment(name)

let mk_str_end_block(name) =
  '</' + BLOCK + '>' + SPACE + mk_comment(name)

let mk_str_end_field(name) =
  '</' + FIELD + '>'+ SPACE + mk_comment(name)

## TODO = used?
let mk_xml_node(tag, text) = 
  result = \
    mk_str_begin(tag) + NEWLINE + \
    text.strip() + NEWLINE + \
    mk_str_end(tag)
#  print 'mk_xml_node:\n', result
  result

let mk_cdata(body) =
  CDATA_BEGIN + NEWLINE + body.strip() + NEWLINE + CDATA_END

let mk_str_block_atom(name, fields) =
  print 'mk_str_block_atom:', name
#  print 'mk_str_block_generic:', fields
  begin = mk_str_begin_atom(name)
  end = mk_str_end_atom(name)  
  result = reduce (lambda x,y: x + NEWLINE + y, fields)
  begin + NEWLINE + result + NEWLINE + end

let mk_str_block_generic(name, fields) =
  print 'mk_str_block_generic:', name
#  print 'mk_str_block_generic:', fields
  begin = mk_str_begin_block(name)
  end = mk_str_end_block(name)  
  result = reduce (lambda x,y: x + NEWLINE + y, fields)
  begin + NEWLINE + result + NEWLINE + end

let mk_str_field_generic(name, contents) =
  begin = mk_str_begin_field(name)
  end = mk_str_end_field(name)
  result = begin + NEWLINE + contents + NEWLINE + end
  result


## END: Utilities
######################################################################

######## 
## BEGIN: Block makers

## book maker
let mk_str_book (fields) =
  mk_str_block_generic(BOOK, fields)

## chapter maker
let mk_str_chapter (fields) = 
  mk_str_block_generic(CHAPTER, fields)

## course maker
let mk_str_course (fields) =
  mk_str_block_generic(COURSE, fields)

## course maker
let mk_str_group (fields) =
  mk_str_block_generic(GROUP, fields)

let mk_str_problem_group (fields) =
  mk_str_block_generic(PROBLEM_GROUP, fields)

let mk_str_problem_set (fields) =
  mk_str_block_generic(PROBLEM_SET, fields)

## section maker
let mk_str_section (fields) =
  mk_str_block_generic(SECTION, fields)

## subsection maker
let mk_str_subsection (fields) =
  mk_str_block_generic(SUBSECTION, fields)

## subsubsection maker
let mk_str_subsubsection (fields) =
  mk_str_block_generic(SUBSUBSECTION, fields)

## un maker
let mk_str_checkpoint (fields) =
  mk_str_block_generic(CHECKPOINT, fields)


## assignment maker
let mk_str_assignment (fields) =
  mk_str_block_generic(ASSIGNMENT, fields)

## atom maker
let mk_str_atom (atom_name, fields) =
  mk_str_block_generic(atom_name, fields)

## atom maker
let mk_str_atom (atom_name, fields) =
  mk_str_block_atom(atom_name, fields)

## algo maker
let mk_str_algo (fields) =
  mk_str_block_generic(ALGO, fields)

## problem_ff maker
let mk_str_problem_fr (fields) =
  mk_str_block_generic(PROBLEM_FR, fields)

## problem_mc maker
let mk_str_problem_ma (fields) =
  mk_str_block_generic(PROBLEM_MA, fields)

## problem_mc maker
let mk_str_problem_mc (fields) =
  mk_str_block_generic(PROBLEM_MC, fields)

## question_ff maker
let mk_str_question_fr (fields) =
  mk_str_block_generic(QUESTION_FR, fields)

## question_mc maker
let mk_str_question_ma (fields) =
  mk_str_block_generic(QUESTION_MA, fields)

## question_mc maker
let mk_str_question_mc (fields) =
  mk_str_block_generic(QUESTION_MC, fields)

## answer maker
let mk_str_answer (fields) =
  mk_str_block_generic(ANSWER, fields)

## choice maker
let mk_str_choice (fields) =
  mk_str_block_generic(CHOICE, fields)

## select maker
let mk_str_select (fields) =
  mk_str_block_generic(SELECT, fields)


## END: Block makers
######## 

######## 
## BEGIN: Field makers
let mk_str_authors(authors) = 
  mk_str_field_generic(AUTHORS, authors)

let mk_str_body (body) = 
  mk_str_field_generic(BODY, mk_cdata(body))

let mk_str_body_src (body) = 
  mk_str_field_generic(BODY_SRC, mk_cdata(body))

let mk_str_body_pop (body) = 
  mk_str_field_generic(BODY_POP, body)

let mk_str_correctness(correctness) = 
  mk_str_field_generic(CORRECTNESS, correctness)

let mk_str_course_number(number) = 
  mk_str_field_generic(COURSE_NUMBER, number)

let mk_str_duedate (duedate) =
  mk_str_field_generic(DUEDATE, duedate)

let mk_str_explain (explain) = 
  mk_str_field_generic(EXPLAIN, explain)

let mk_str_explain_src (explain) = 
  mk_str_field_generic(EXPLAIN_SRC, explain)

let mk_str_folder (folder) = 
  mk_str_field_generic(FOLDER, folder)

let mk_str_hint (hint) = 
  mk_str_field_generic(HINT, hint)

let mk_str_hint_src (hint) = 
  mk_str_field_generic(HINT_SRC, hint)

let mk_str_info(info) =
  mk_str_field_generic(INFO, info)

let mk_str_info_src(info) =
  mk_str_field_generic(INFO_SRC, info)

let mk_str_instance(info) =
  mk_str_field_generic(INSTANCE, info)

let mk_str_intro(intro) = 
  mk_str_field_generic(INTRO, mk_cdata(intro))

let mk_str_intro_src(intro) = 
  mk_str_field_generic(INTRO_SRC, mk_cdata(intro))

let mk_str_label(label) = 
#  print 'label:', label
  mk_str_field_generic(LABEL, label)

let mk_str_no(no) = 
#  print 'no =', no
  mk_str_field_generic(NO, no)

let mk_str_parents(parents): 
#  print 'mk_str_field = parents = ', parents
  parents = ', '.join(parents)
  mk_str_field_generic(PARENTS, parents)

let mk_str_picture(picture) = 
  mk_str_field_generic(PICTURE, picture)

let mk_str_points(points) = 
  mk_str_field_generic(POINTS, points)

let mk_str_prompt (prompt) = 
  mk_str_field_generic(PROMPT, prompt)

let mk_str_prompt_src (prompt) = 
  mk_str_field_generic(PROMPT_SRC, prompt)

let mk_str_provides_book (pb) = 
  mk_str_field_generic(PROVIDES_BOOK, pb)

let mk_str_provides_chapter (pc) = 
  mk_str_field_generic(PROVIDES_CHAPTER, pc)

let mk_str_provides_section (pc) = 
  mk_str_field_generic(PROVIDES_SECTION, pc)

let mk_str_provides_subsection (pc) = 
  mk_str_field_generic(PROVIDES_SUBSECTION, pc)

let mk_str_provides_assignment (pc) =
  mk_str_field_generic(PROVIDES_ASSIGNMENT, pc)

let mk_str_semester(semester) = 
  mk_str_field_generic(SEMESTER, semester)

let mk_str_solution (solution) = 
  mk_str_field_generic(SOLUTION, solution)

let mk_str_solution_src (solution) = 
  mk_str_field_generic(SOLUTION_SRC, solution)

# let mk_str_title(title): 
#   if title:
#     mk_str_field_generic(FIELD_TITLE, title)
#   else:
#     KW_UNTITLED

let mk_str_title(title) = 
  mk_str_field_generic(TITLE, mk_cdata(title))

let mk_str_title_src(title) = 
  mk_str_field_generic(TITLE_SRC, mk_cdata(title))

let mk_str_topics (topics) = 
  mk_str_field_generic(TOPICS, topics)

let mk_str_unique(unique) = 
  mk_str_field_generic(UNIQUE, unique)

let mk_str_website(website) = 
  mk_str_field_generic(WEBSITE, website)

## END: Field makers
######## 
