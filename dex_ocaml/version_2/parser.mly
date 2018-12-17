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
  box boxes SEPARATOR {}
;		

paragraphs:
| paragraph	{}
| paragraphs SEPARATOR paragraph {}
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
  chapter_heading SEPARATOR blocks SEPARATOR sections EOF {}
|	chapter_heading SEPARATOR sections EOF {}	
;		

section:
  section_heading SEPARATOR blocks {}		
	
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
| atoms SEPARATOR atom {  }		
;
	
atom:
| paragraph		 {}
|	env_b_definition paragraphs env_e_definition   { }
| env_b_example paragraphs  env_e_example    { }
;

