## Error conditions that can occur as tokens
kw_not_provided = '...NOT.PROVIDED.'
kw_no_answer = KW_NOT_PROVIDED + 'ANSWER...' 
kw_no_answers = KW_NOT_PROVIDED + 'ANSWERS...' 
kw_no_checkpoint = KW_NOT_PROVIDED + 'CHECKPOINT...' 
kw_no_explain = KW_NOT_PROVIDED + 'EXPLANATION...' 
kw_no_hint = KW_NOT_PROVIDED + 'HINT...' 
kw_no_intro = KW_NOT_PROVIDED + 'INTRO...' 
kw_no_info = KW_NOT_PROVIDED + 'INFO...'
kw_no_parents = KW_NOT_PROVIDED + 'PARENTS...' 
kw_no_points = '0'  # Still has to be a number
kw_points_correct = '1'  # Still has to be a number
kw_no_prompt = KW_NOT_PROVIDED + 'PROMPT...' 

kw_no_solution = KW_NOT_PROVIDED + 'UNKOWN.SOLUTION...' 
kw_no_label = KW_NOT_PROVIDED + 'LABEL...' 
kw_no_title = KW_NOT_PROVIDED + 'TITLE...' 
kw_no_topics = KW_NOT_PROVIDED + 'TOPICS...'
kw_no_unique = KW_NOT_PROVIDED + 'UNIQUE...' 

# These are handled specially
kw_no_unique = '0'  # has to be a number still 
kw_no_no = '0'  # has to be a number still 
kw_no_rank = '0'  # has to be a number still 

# commands, start with BACKSLASH
COM_BEGIN = BACKSLASH + r'begin'
COM_END = BACKSLASH + r'end'
COM_LABEL = BACKSLASH + r'label'
COM_NO =  BACKSLASH + r'no'
COM_PARENT = BACKSLASH + r'parent'
COM_PARENTS = BACKSLASH + r'parents'
COM_UNIQUE =  BACKSLASH + r'unique'

## Keywords, don't start with a backshlash
KW_ATOM_TITLE_PREFIX = 'Atom'
KW_CHAPTER_TITLE_PREFIX = 'Chapter'
KW_QUESTION_TITLE_PREFIX = 'Question'
KW_CHECKPOINT_TITLE_PREFIX = 'Checkpoint'
KW_SECTION_TITLE_PREFIX = 'Section'
KW_SUBSECTION_TITLE_PREFIX = 'Subsection'

# Label prefixes
KW_LABEL_PREFIX_ANSWER = 'answer'
KW_LABEL_PREFIX_ASSIGNMENT = 'assignment'
KW_LABEL_PREFIX_ASSTPROBLEM = 'asstproblem'
KW_LABEL_PREFIX_ATOM = 'at'
KW_LABEL_PREFIX_BOOK = 'book'
KW_LABEL_PREFIX_CHAPTER = 'ch'
KW_LABEL_PREFIX_CHOICE = 'choice'
KW_LABEL_PREFIX_COURSE = 'course'
KW_LABEL_PREFIX_GRAM = 'gram'
KW_LABEL_PREFIX_GROUP = 'gr'
KW_LABEL_PREFIX_CHECKPOINT = 'checkpoint'
KW_LABEL_PREFIX_PROBLEM = 'problem'
KW_LABEL_PREFIX_PROBLEM_GROUP = 'problem_group'
KW_LABEL_PREFIX_PROBLEM_SET = 'problem_set'
KW_LABEL_PREFIX_PROBLEM_FR = 'problem_fr'
KW_LABEL_PREFIX_PROBLEM_MA = 'problem_ma'
KW_LABEL_PREFIX_PROBLEM_MC = 'problem_mc'
KW_LABEL_PREFIX_QUESTION_FR = 'question_fr'
KW_LABEL_PREFIX_QUESTION_MA = 'question_ma'
KW_LABEL_PREFIX_QUESTION_MC = 'question_mc'
KW_LABEL_PREFIX_SECTION = 'sec'
KW_LABEL_PREFIX_SELECT = 'select'
KW_LABEL_PREFIX_SEMESTER = 'semester'
KW_LABEL_PREFIX_SUBSECTION = 'subsec'
KW_LABEL_PREFIX_SUBSUBSECTION = 'subsubsec'

## Keywords, don't start with a backshlash
KW_BOOK_NO = r'bookno'
KW_COURSE_NO = r'courseno'
KW_CHAPTER_NO = r'chapterno'
KW_FALSE = r'False'
KW_SECTION_NO = r'sectionno'
KW_ATOM_NO = r'atomno'
KW_QUESTION_NO = r'problemno'
KW_CHECKPOINT_NO = r'checkpointno'
KW_PARENT = r'parent'
KW_PROMPT = r'prompt'
KW_POINTS = r'points'
KW_TRUE = r'True'
KW_SUBSECTION_NO = r'subsectionno'

