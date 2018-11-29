{
open Core
open Printf
open Parser


let sections_list = 
  [
   ("chapter", HEADING_CHAPTER);
   ("section", HEADING_SECTION);
   ("subsection", HEADING_SUBSECTION);
   ("subsubsection", HEADING_SUBSUBSECTION);
   ("paragraph", HEADING_PARAGRAPH);
   ("subparagraph", HEADING_SUBPARAGRAPH)
  ]

let sections_table = String.Table.create () ~size:64 
let _ = List.iter ~f:(fun (key, data) -> Hashtbl.set sections_table ~key ~data) sections_list
}	

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
(* Critical space, two or more newlines *)
let p_separator = [' ' '\t']* '\n' ([' ' '\t']* '\n')+ 		
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
		
let p_begin = '\\' "begin"				
let p_end = '\\' "begin"				

let p_word = [^ ' ' '\t' '\n' '{' '}' '[' ']']+
(** END PATTERNS *)			

rule token = parse

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
		
| p_begin as keyword_begin
  	{printf "%s" keyword_begin; KW_BEGIN}		
| p_end as keyword_end
  	{printf "%s" keyword_end; KW_END}		

| p_word as word
		{printf "_%s_" word; WORD (word)}
		
| eof
		{EOF}
| _
    {token lexbuf}		
		
{
}
