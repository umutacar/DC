{
open Core
open Printf
open Parser

}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n']*	
			
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_ws
let p_o_sq_bracket = '[' p_ws
let p_c_sq_bracket = ']' p_ws											
												 
let p_chapter = '\\' "chapter" p_ws
let p_section = '\\' "section" p_ws
let p_subsection = '\\' "subsection" p_ws
let p_subsubsection = p_ws '\\' "subsubsection" p_ws
let p_paragraph = p_ws '\\' "paragraph" p_ws												
let p_subparagraph = p_ws '\\' "subparagraph" p_ws												
		
let p_b_definition = p_ws '\\' "begin{definition}" p_ws				
let p_e_definition = p_ws '\\' "end{definition}" p_ws
		

let p_b_example = p_ws '\\' "begin{example}" p_ws
let p_e_example = p_ws '\\' "end{example}" p_ws

let p_b_group = p_ws '\\' "begin{group}" p_ws	
let p_e_group = p_ws '\\' "end{group}" p_ws


let p_word = [^ '\\' '{' '}' '[' ']']+ 
(** END PATTERNS *)			

rule token = parse

| p_o_curly
		{printf "matched: {."; O_CURLY}				
| p_c_curly
		{printf "matched: }\n"; C_CURLY}				


| p_o_sq_bracket
		{printf "matched: ["; O_SQ_BRACKET}				
| p_c_sq_bracket
		{printf "matched: ]\n"; C_SQ_BRACKET}				
		
		
| p_chapter as heading
  	{printf "matched %s." heading; HEADING_CHAPTER}		
| p_section as heading
  	{printf "matched: %s." heading; HEADING_SECTION}		
| p_subsection as heading
  	{printf "matched: %s." heading; HEADING_SUBSECTION}
| p_subsubsection as heading
  	{printf "matched: %s." heading; HEADING_SUBSUBSECTION}
| p_paragraph as heading
  	{printf "matched: %s." heading; HEADING_PARAGRAPH}				
| p_subparagraph as heading
  	{printf "matched: %s." heading; HEADING_SUBPARAGRAPH}
		
| p_b_definition as begin_atom
  	{printf "matched: %s. " begin_atom; ENV_B_DEFINITION}		
| p_e_definition as end_atom
  	{printf "matched: %s." end_atom; ENV_E_DEFINITION}


| p_b_example as begin_atom
  	{printf "matched: %s." begin_atom; ENV_B_EXAMPLE}		
| p_e_example as end_atom
  	{printf "matched: %s." end_atom; ENV_E_EXAMPLE}

| p_b_group as begin_group
  	{printf "matched: %s." begin_group; ENV_B_GROUP}		
| p_e_group as end_group
  	{printf "matched: %s." end_group; ENV_E_GROUP}

| p_word as word
		{printf "matched: _%s_" word; WORD (word)}
		
| eof
		{EOF}
| _
    {token lexbuf}		
		
{
}
