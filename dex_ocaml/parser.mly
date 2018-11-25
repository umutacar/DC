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

chapter:
         {}
| blocks {}
| blocks sections {}
;		
		
sections:
         {}   	
| b = block bs = blocks {  }
;

blocks:
         {}   	
| b = block bs = blocks {  }
;

block:
  ENV_B_DEFINITION c = contents  ENV_E_DEFINITION    { }
| ENV_B_EXAMPLE c = contents  ENV_E_EXAMPLE    { }
;

contents:
     {}
| w = WORD c = contents {}
