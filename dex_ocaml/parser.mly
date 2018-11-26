%{
open Printf
let parse_error s = printf "Parse Error: %s" s
%}	


%token EOF
%token SPACE TAB NEWLINE
%token CRITICAL

%token <string> WORD

%token O_CURLY C_CURLY	
%token O_SQ_BRACKET C_SQ_BRACKET

	
%token HEADING_CHAPTER
%token HEADING_SECTION
%token HEADING_SUBSECTION
%token HEADING_SUBSUBSECTION	
%token HEADING_PARAGRAPH	
%token HEADING_SUBPARAGRAPH

%token ENV_B_GROUP ENV_E_GROUP
%token ENV_B_DEFINITION ENV_E_DEFINITION
%token ENV_B_EXAMPLE ENV_E_EXAMPLE

	
%start chapter
%type <unit> chapter
%%


/* a box is the basic unit of composition */
box:
  WORD  {}
| O_CURLY boxes C_CURLY {}
| O_SQ_BRACKET boxes C_SQ_BRACKET {}
;	

boxes:
  {}
| boxes box { }
;

/* paragraph */
paragraph:
  box boxes CRITICAL
;		

paragraphs:
| paragraph	{}
| paragraphs CRITICAL paragraph {}
;

chapter_heading:
 HEADING_CHAPTER O_CURLY boxes C_CURLY {}
;

section_heading:
 HEADING_SECTION O_CURLY boxes C_CURLY {}
;

env_b_definition:
 ENV_B_DEFINITION O_SQ_BRACKET boxes C_SQ_BRACKET {}
;

env_e_definition:
  ENV_E_DEFINITION { }

env_b_example:
 ENV_B_EXAMPLE O_SQ_BRACKET boxes C_SQ_BRACKET {}
;

env_e_example:
  ENV_E_EXAMPLE { }
;

env_b_group:
 ENV_B_GROUP O_SQ_BRACKET boxes C_SQ_BRACKET {}
;

env_e_group:
  ENV_E_GROUP { }

chapter:
  chapter_heading CRITICAL blocks CRITICAL sections EOF {}
|	chapter_heading nnewlines sections newlines EOF {}	
;		

section:
  section_heading nnewlines blocks {}		
	
sections:
| section {}
| sections newlines section {}
;

blocks:
|	block {}
| blocks nnewlines block {  }
;

block:
	atom {}
| group {}
;			
			
group:
  env_b_group nnewlines atoms nnewlines env_e_group {}
;

atoms:
 {}		
|	atom {}
| atoms newlines atom {  }
;
	
atom:
| paragraph		 {}
|	env_b_definition nnewlines paragraphs newlines  env_e_definition    { }
|	env_b_definition nnewlines error newlines  env_e_definition    { }
| env_b_example nnewlines paragraphs newlines  env_e_example    { }
;

