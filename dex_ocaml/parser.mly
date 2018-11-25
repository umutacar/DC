%{
open Printf
let parse_error s = printf "Parse Error: %s" s
%}	

%token EOF
%token SPACE TAB NEWLINE

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

/* begin spaces = whitespaces without NEWLINE */
benign_space:
	SPACE {}
|	TAB {}
;
benign_spaces:
  {}
| benign_space {}
| benign_space benign_spaces {}		
;		

/* white space */
ws:
	SPACE {}
|	TAB {}
| NEWLINE {}		
;

wss:
  {}
| ws {}
| wss ws {}		
;		
	
/* a box is the basic unit of composition */
box:
	WORD  {}
| O_CURLY wss box wss C_CURLY {}
| O_SQ_BRACKET wss box wss C_SQ_BRACKET {}
;	

boxes:
  box { }
| boxes benign_spaces box { }
;

line:
  benign_spaces boxes benign_spaces NEWLINE {}
;		
		
/* paragraph */
paragraph:
| line { } 
| paragraph line { } 		
;		

paragraphs:
| paragraph	{}
| paragraphs benign_spaces NEWLINE wss paragraph {}
;

chapter_heading:
  HEADING_CHAPTER O_CURLY C_CURLY { }
| HEADING_CHAPTER O_CURLY boxes C_CURLY {}
;

section_heading:
  HEADING_SECTION O_CURLY C_CURLY {}
| HEADING_SECTION O_CURLY boxes C_CURLY {}
;

env_b_definition:
  ENV_B_DEFINITION { }
| ENV_B_DEFINITION O_SQ_BRACKET wss boxes wss C_SQ_BRACKET {}
;

env_e_definition:
  ENV_E_DEFINITION { }

env_b_example:
  ENV_B_EXAMPLE { }
| ENV_B_EXAMPLE O_SQ_BRACKET wss boxes wss C_SQ_BRACKET {}
;

env_e_example:
  ENV_E_EXAMPLE { }
;

env_b_group:
  ENV_B_GROUP { }
| ENV_B_GROUP O_SQ_BRACKET wss boxes wss C_SQ_BRACKET {}
;

env_e_group:
  ENV_E_GROUP { }

		
chapter:
| chapter_heading  EOF {}		
| chapter_heading wss blocks wss EOF {}
| chapter_heading wss blocks sections EOF {}
;		
		
sections:
  section {}
| section sections {}
;

section:
  section_heading {}
| section_heading blocks {}		
  

blocks:
	block {}
| blocks wss block {  }
;

block:
	atom {}
| group {}
;			
			
group:
  env_b_group wss atoms wss env_e_group {}
;

atoms:
	atom {}
| atoms wss atom {  }
;
	
atom:
| paragraph		 {}
|	env_b_definition wss paragraphs wss  env_e_definition    { }
|	env_b_definition wss error wss  env_e_definition    { }
| env_b_example wss paragraphs wss  env_e_example    { }
;

