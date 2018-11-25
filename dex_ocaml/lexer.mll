{
open Core
open Printf
open Parser

}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'				
let p_o_curly = '{'
let p_c_curly = '}'
let p_o_sq_bracket = '['
let p_c_sq_bracket = ']' 													
												 
let p_chapter = '\\' "chapter"
let p_section = '\\' "section"
let p_subsection = '\\' "subsection"
let p_subsubsection = '\\' "subsubsection"
let p_paragraph = '\\' "paragraph"												
let p_subparagraph = '\\' "subparagraph"												
		
let p_b_definition = '\\' "begin{definition}"				
let p_e_definition = '\\' "end{definition}"

let p_b_example = '\\' "begin{example}"				
let p_e_example = '\\' "end{example}"		

let p_b_group = '\\' "begin{group}"				
let p_e_group = '\\' "end{group}"		


let p_word = [^ '\n' '{' '}' '[' ']']+
(** END PATTERNS *)			

rule token = parse
| p_newline
		{printf "\n"; NEWLINE}

| p_o_curly
		{printf "{"; O_CURLY}				
| p_c_curly
		{printf "}"; C_CURLY}				


| p_o_sq_bracket
		{printf "["; O_SQ_BRACKET}				
| p_c_sq_bracket
		{printf "]"; C_SQ_BRACKET}				
		
		
| p_chapter as heading
  	{printf "%s" heading; HEADING_CHAPTER}		
| p_section as heading
  	{printf "%s" heading; HEADING_SECTION}		
| p_subsection as heading
  	{printf "%s" heading; HEADING_SUBSECTION}
| p_subsubsection as heading
  	{printf "%s" heading; HEADING_SUBSUBSECTION}
| p_paragraph as heading
  	{printf "%s" heading; HEADING_PARAGRAPH}				
| p_subparagraph as heading
  	{printf "%s" heading; HEADING_SUBPARAGRAPH}
		
| p_b_definition as begin_atom
  	{printf "%s" begin_atom; ENV_B_DEFINITION}		
| p_e_definition as end_atom
  	{printf "%s" end_atom; ENV_E_DEFINITION}


| p_b_example as begin_atom
  	{printf "%s" begin_atom; ENV_B_EXAMPLE}		
| p_e_example as end_atom
  	{printf "%s" end_atom; ENV_E_EXAMPLE}

| p_b_group as begin_group
  	{printf "%s" begin_group; ENV_B_GROUP}		
| p_e_group as end_group
  	{printf "%s" end_group; ENV_E_GROUP}

| p_word as word
		{printf "_%s_" word; WORD (word)}
		
| eof
		{EOF} 		
		
{
}
