{
open Core
open Printf
open Parser

}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
(* Critical space, two newlines *)
let p_critical = [' ' '\t']* '\n' [' ' '\t']* '\n' 		
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
| p_critical as x
		{printf "%s" x; token lexbuf}

| p_o_curly
		{printf "{"; token lexbuf}
| p_c_curly
		{printf "}"; token lexbuf}

| p_o_sq_bracket
		{printf "["; token lexbuf}				
| p_c_sq_bracket
		{printf "]"; token lexbuf}				
		
		
| p_chapter as heading
  	{printf "%s" heading; token lexbuf}		
| p_section as heading
  	{printf "%s" heading; token lexbuf}		
| p_subsection as heading
  	{printf "%s" heading; token lexbuf}
| p_subsubsection as heading
  	{printf "%s" heading; token lexbuf}
| p_paragraph as heading
  	{printf "%s" heading; token lexbuf}
| p_subparagraph as heading
  	{printf "%s" heading; token lexbuf}
		
| p_b_definition as begin_atom
  	{printf "%s" begin_atom; token lexbuf}
| p_e_definition as end_atom
  	{printf "%s" end_atom; token lexbuf}


| p_b_example as begin_atom
  	{printf "%s" begin_atom; token lexbuf}
| p_e_example as end_atom
  	{printf "%s" end_atom; token lexbuf}

| p_b_group as begin_group
  	{printf "%s" begin_group; token lexbuf}
| p_e_group as end_group
  	{printf "%s" end_group; token lexbuf}

| p_word as word
		{printf "_%s_" word; token lexbuf}
		
| eof
		{}

| _
    {token lexbuf}		
		
{
let main () =
	let args = Sys.argv in
    if Array.length args = 2 then
      let filename = Sys.argv.(1) in
			(* let ic = open_in filename in *)
			let ic = In_channel.create filename in
      let lexbuf = Lexing.from_channel ic in
      token lexbuf;
  		printf "Completed lexing\n"
    else
      printf "Usage: lexer <filename>\n";;
}
