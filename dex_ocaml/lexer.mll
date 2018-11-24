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
| '\\' "begin{definition}"  {printf "atom definition\n"; token lexbuf}
| _                     {token lexbuf}
| eof     {()} 		
		
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
