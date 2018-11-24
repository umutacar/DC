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


{
let main () =
	let args = Sys.argv in
    if Array.length args = 2 then
      let filename = Sys.argv.(1) in
			let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      count lexbuf;
  		Printf.printf "# lines = %d, # chars = %d\n" !n_lines !n_chars			
    else
      print_string "Usage: lexer <filename>\n";;

let _ = main ()
}
