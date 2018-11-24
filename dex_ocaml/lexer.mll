{
open Core
open Printf

let n_lines = ref 0
let n_chars = ref 0

}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'				

let p_b_definition = '\\' "begin{definition}"				
let p_e_definition = '\\' "end{definition}"

let p_b_example = '\\' "begin{example}"				
let p_e_example = '\\' "end{example}"		

let p_b_group = '\\' "begin{group}"				
let p_e_group = '\\' "end{group}"		

let p_word = [^ ' ' '\n' '\t']+
(** END PATTERNS *)			
rule count  = parse
| '\n'    {incr n_lines ; printf "line\n"; count lexbuf}
| _       {incr n_chars ; count lexbuf}
| eof     {()}


and token = parse
| p_space
		{printf " "; token lexbuf}		
| p_newline
		{printf "\n"; token lexbuf}
| p_tab
		{printf "\t"; token lexbuf}		

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
		{printf "word(%s)" word; token lexbuf}

| eof
		{()} 		
		
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
