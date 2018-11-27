%{
open Printf
let parse_error s = printf "Parse Error: %s" s
%}	


%token EOF
%token SPACE TAB NEWLINE
%token SEPARATOR

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

curly_box:
  O_CURLY boxes C_CURLY {}
;
sq_box:
  O_SQ_BRACKET boxes C_SQ_BRACKET {}
;

word: 
  WORD {}
;

/* a box is the basic unit of composition */
box:
  word {}
| curly_box {}
| sq_box {}
;	

boxes:
  {}
| boxes box { }
;

sq_boxes:
  {}
|	word boxes { }
|	sq_box boxes { }
;

curly_boxes:
  {}
|	word boxes { }
|	curly_box boxes { }
;


chapter_heading:
 HEADING_CHAPTER curly_box {}
;

section_heading:
 HEADING_SECTION curly_box {}
;

env_b_definition:
 ENV_B_DEFINITION sq_box {}
;

env_e_definition:
  ENV_E_DEFINITION { }

env_b_example:
 ENV_B_EXAMPLE sq_box {}
;

env_e_example:
  ENV_E_EXAMPLE { }
;

env_b_group:
 ENV_B_GROUP sq_box {}
;

env_e_group:
  ENV_E_GROUP { }

chapter:
  chapter_heading blocks sections EOF {}
|	chapter_heading sections EOF {}	
;		

section:
  section_heading blocks {}		
	
sections:
| section {}
| sections section {}
;

blocks:
|	block {}
| blocks block {  }
;

block:
	atom {}
| group {}
;			
			
group:
  env_b_group atoms env_e_group {}
;

atoms:
 {}		
|	atom {}
| atoms atom {  }
;
	
atom:
	env_b_definition curly_boxes env_e_definition   { }
| env_b_example curly_boxes  env_e_example    { }
;

