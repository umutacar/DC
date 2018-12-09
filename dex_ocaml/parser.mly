%{
open Printf
let parse_error s = printf "Parse Error: %s" s
%}	


%token EOF

%token <string> WORD

%token <string> BACKSLASH
%token <string> O_CURLY C_CURLY	
%token <string> O_SQ_BRACKET C_SQ_BRACKET

	
%token <string> HEADING_CHAPTER
%token <string> HEADING_SECTION
%token <string> HEADING_SUBSECTION
%token <string> HEADING_SUBSUBSECTION	
%token <string> HEADING_PARAGRAPH	
%token <string> HEADING_SUBPARAGRAPH

%token <string> ENV_B_GROUP ENV_E_GROUP
%token <string> ENV_B_DEFINITION ENV_E_DEFINITION
%token <string> ENV_B_EXAMPLE ENV_E_EXAMPLE

	
%start chapter
%type <string> chapter
%%

/* A curly box looks like
   { word } 
   { [ word ] { word } ] 
   etc.  
*/
curly_box:
  o = O_CURLY;  b = boxes;  c = C_CURLY 
  {o ^ b ^ c}


sq_box:
  o = O_SQ_BRACKET; b = boxes;  c = C_SQ_BRACKET 
  {o ^ b ^ c}

word: 
  w = WORD 
  {w}
| b = BACKSLASH w = WORD
  {b ^ w}

/* a box is the basic unit of composition */
box:
  w = word 
  {w}
| b = curly_box 
  {b}
| b = sq_box 
  {b}


boxes:
  {""}
| bs = boxes; b = box
  {bs ^ b }


boxes_start_no_sq:
  {""}
|	w = word; bs = boxes
  {w ^ bs}
|	b = curly_box; bs = boxes
  {b ^ bs }


chapter_heading:
  hc = HEADING_CHAPTER; b = curly_box 
  {hc ^ b}


section_heading:
  hs = HEADING_SECTION; b = curly_box 
  {hs ^ b}


env_b_definition_sq:
  hb = ENV_B_DEFINITION; b = sq_box
  {hb ^ b}


env_b_definition:
  hb = ENV_B_DEFINITION; 
  {hb}


env_e_definition:
  he = ENV_E_DEFINITION
  {he}


env_b_example:
  hb = ENV_B_EXAMPLE; b = sq_box
  {hb ^ b}


env_b_example_sq:
  hb = ENV_B_EXAMPLE; s = sq_box
  {hb ^ s}


env_e_example:
  he = ENV_E_EXAMPLE
  {he}


env_b_group:
  hb = ENV_B_GROUP
  {hb}


env_b_group_sq:
  hb = ENV_B_GROUP
  b = sq_box
  {hb ^ b}


env_e_group:
  he = ENV_E_GROUP
  {he}

chapter:
  hc = chapter_heading; bs = blocks; ss = sections; EOF 
  {hc ^ bs ^ ss}
|	hc = chapter_heading; ss = sections; EOF
  {hc ^ ss};		

section: 
  hc = section_heading; bs = blocks
  {hc ^ bs}		
	
sections:
| s = section; {s}
| ss = sections; s = section
  {ss ^ s}


blocks:
	b = block 
  {b}
| bs = blocks; b = block
  {bs ^ b}


block:
	a = atom
  {a}
| g = group
  {g}

			
group:
  hb = env_b_group; ats = atoms; he = env_e_group
  {hb ^ ats ^ he}
| hb = env_b_group_sq; ats = atoms; he = env_e_group
  {hb ^ ats ^ he}


atoms:
 {""}		
|	a = atom 
  {a}
| ats = atoms; a = atom
  {ats ^ a}

	
atom:
	hb = env_b_definition; bs = boxes_start_no_sq; he = env_e_definition
  {hb ^ bs ^ he }
|	hb = env_b_definition_sq; bs = boxes; he = env_e_definition
  {hb ^ bs ^ he}
| hb = env_b_example; bs = boxes_start_no_sq;  he = env_e_example
  {hb ^ bs ^ he}
| hb = env_b_example_sq; bs = boxes; he = env_e_example
  {hb ^ bs ^ he}

