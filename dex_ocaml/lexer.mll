{
open Core
open Printf

let n_lines = ref 0
let n_chars = ref 0
	
}

rule count  = parse
| '\n'    {incr n_lines ; printf "line\n"; count lexbuf}
| _       {incr n_chars ; count lexbuf}
| eof     {()}


and token = parse
| ' '
		{printf " "; token lexbuf}		
| '\n'
		{printf "\n"; token lexbuf}
| '\t'
		{printf "\t"; token lexbuf}		
		
| '\\' "begin{definition}" as begin_atom
  	{printf "%s" begin_atom; token lexbuf}
| '\\' "end{definition}" as end_atom
  	{printf "%s" end_atom; token lexbuf}

| '\\' "begin{group}" as begin_group
  	{printf "%s" begin_group; token lexbuf}		
| '\\' "end{group}" as end_group
  	{printf "%s" end_group; token lexbuf}

| [^ ' ' '\n' '\t']+
		{printf "word"; token lexbuf}

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
