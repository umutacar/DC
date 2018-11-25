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
white_space:
	SPACE {}
|	TAB {}
| NEWLINE {}		
;
white_spaces:
  {}
| white_space {}
| white_spaces white_space {}		
;		
	
/* a box is the basic unit of composition */
box:
	WORD  {}
| O_CURLY white_spaces box white_spaces C_CURLY {}
| O_SQ_BRACKET white_spaces box white_spaces C_SQ_BRACKET {}
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
| paragraphs benign_spaces NEWLINE white_spaces paragraph {}
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
| ENV_B_DEFINITION O_SQ_BRACKET white_spaces boxes white_spaces O_SQ_BRACKET {}
;

env_e_definition:
  ENV_E_DEFINITION { }

env_b_example:
  ENV_B_EXAMPLE { }
| ENV_B_EXAMPLE O_SQ_BRACKET white_spaces boxes white_spaces O_SQ_BRACKET {}
;

env_e_example:
  ENV_E_EXAMPLE { }
;

env_b_group:
  ENV_B_GROUP { }
| ENV_B_GROUP O_SQ_BRACKET white_spaces boxes white_spaces O_SQ_BRACKET {}
;

env_e_group:
  ENV_E_GROUP { }

		
chapter:
| chapter_heading  EOF {}		
| chapter_heading white_spaces blocks white_spaces EOF {}
| chapter_heading white_spaces blocks sections EOF {}
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
| blocks white_spaces block {  }
;

block:
	atom {}
| group {}
;			
			
group:
  env_b_group white_spaces atoms env_e_group {}
;

atoms:
	atom {}
| atoms white_spaces atom {  }
;
	
atom:
| paragraph		 {}
|	env_b_definition white_spaces paragraphs  env_e_definition    { }
| env_b_example paragraphs  env_e_example    { }
;

