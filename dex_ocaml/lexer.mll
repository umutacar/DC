{
open Core
open Printf
open Parser
open Atoms

let atom_table = String.Table.create () ~size:512 
let _ = List.iter ~f:(fun (key, data) -> Hashtbl.set atom_table ~key ~data) all_atoms

}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n']*	

let p_backslash = '\\' p_ws			
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_ws
let p_o_sq_bracket = '[' p_ws
let p_c_sq_bracket = ']' p_ws											

let p_label = '\\' "label" p_ws												 

let p_chapter = '\\' "chapter" p_ws
let p_section = '\\' "section" p_ws
let p_subsection = '\\' "subsection" p_ws
let p_subsubsection = '\\' "subsubsection" p_ws
let p_paragraph = '\\' "paragraph" p_ws												
let p_subparagraph = '\\' "subparagraph" p_ws												

let p_b_group = '\\' "begin{group}" p_ws	
let p_e_group = '\\' "end{group}" p_ws

let p_begin = '\\' "begin"				
let p_end = '\\' "end"				

let p_word = [^ '\\' '{' '}' '[' ']']+ 
(** END PATTERNS *)			

rule token = parse
| p_backslash as x
		{printf "!matched: \\."; BACKSLASH(x)}				
| p_o_curly as x
		{printf "!matched: {."; O_CURLY(x)}				
| p_c_curly as x
		{printf "!matched: }.\n"; C_CURLY(x)}				

| p_o_sq_bracket as x
		{printf "!matched: [."; O_SQ_BRACKET(x)}				
| p_c_sq_bracket as x
		{printf "!matched: ].\n"; C_SQ_BRACKET(x)}				

| p_label as x
  	{printf "!matched %s." x; KW_LABEL(x)}		
				
| p_chapter as x
  	{printf "!matched %s." x; KW_CHAPTER(x)}		
| p_section as x
  	{printf "!matched: %s." x; KW_SECTION(x)}		
| p_subsection as x
  	{printf "!matched: %s." x; KW_SUBSECTION(x)}
| p_subsubsection as x
  	{printf "!matched: %s." x; KW_SUBSUBSECTION(x)}
| p_paragraph as x
  	{printf "!matched: %s." x; KW_PARAGRAPH(x)}				
| p_subparagraph as x
  	{printf "!matched: %s." x; KW_SUBPARAGRAPH(x)}		

| p_begin as x
  	{printf "%s" x; KW_BEGIN(x)}		
| p_end as x
  	{printf "%s" x; KW_END(x)}		

| p_b_group as x
  	{printf "!matched: %s." x; ENV_B_GROUP(x)}		
| p_e_group as x
  	{printf "!matched: %s." x; ENV_E_GROUP(x)}

| p_word as x
		{printf "!found word: %s." x;
     match (Hashtbl.find atom_table x) with 
     | None -> printf "!matched word: %s." x; WORD (x)
     | Some y ->  printf "!found atom: %s." y; ATOM(y)
    }
		
| eof
		{EOF}
| _
    {token lexbuf}		
		
{
}
