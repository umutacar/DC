%{
open Printf
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
  benign_space {}
| benign_space benign_spaces {}		
;		

/* white space */
white_space:
	SPACE {}
|	TAB {}
| NEWLINE {}		
;
white_spaces:
  white_space {}
| white_space white_spaces {}		
;		
	
/* word is a word also possibly bracketed */
/* todo: allow for space within brackets*/
word:
	WORD {}
| O_CURLY word C_CURLY {}
| O_CURLY error C_CURLY {}		
| O_SQ_BRACKET word C_SQ_BRACKET {}
| O_SQ_BRACKET error C_SQ_BRACKET {}
;	

words:
  word { }
| word words { }		
| word benign_spaces words { }
;
		
/* A plain paragraph */
plain_paragraph:
| words NEWLINE NEWLINE { }
| words NEWLINE plain_paragraph { } 		
;		

plain_paragraphs:
| plain_paragraph   		{}
| plain_paragraph plain_paragraphs {}
;

chapter_heading:
  HEADING_CHAPTER O_CURLY C_CURLY { }
| HEADING_CHAPTER O_CURLY words C_CURLY {}
;

section_heading:
  HEADING_SECTION O_CURLY C_CURLY {}
| HEADING_SECTION O_CURLY words C_CURLY {}
;

chapter:
| chapter_heading  EOF {}		
| chapter_heading blocks EOF {}
| chapter_heading error EOF {}		
| chapter_heading blocks sections EOF {}
;		
		
sections:
  section {}
| section sections {}
;

section:
  section_heading {}
| section_heading blocks {}		
  

blocks:
  block blocks {  }
;

block:
| plain_paragraph		 {}
|	ENV_B_DEFINITION plain_paragraphs  ENV_E_DEFINITION    { }
| ENV_B_EXAMPLE plain_paragraphs  ENV_E_EXAMPLE    { }
;

