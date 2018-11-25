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
	
/* word is a word also possibly bracketed */
/* todo: allow for space within brackets*/
word:
	WORD  {}
| O_CURLY white_spaces word white_spaces C_CURLY {}
| O_SQ_BRACKET white_spaces word white_spaces C_SQ_BRACKET {}
;	

words:
  word { }
| words benign_spaces word { }
;

line:
  benign_spaces words benign_spaces NEWLINE {}
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
| HEADING_CHAPTER O_CURLY words C_CURLY {}
;

section_heading:
  HEADING_SECTION O_CURLY C_CURLY {}
| HEADING_SECTION O_CURLY words C_CURLY {}
;

env_b_definition:
  ENV_B_DEFINITION { }
| ENV_B_DEFINITION O_SQ_BRACKET white_spaces words white_spaces O_SQ_BRACKET {}
;

	
	

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
| paragraph		 {}
|	env_b_definition white_spaces paragraphs  ENV_E_DEFINITION    { }
| ENV_B_EXAMPLE paragraphs  ENV_E_EXAMPLE    { }
;

