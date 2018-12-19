open Core
open Lexer
open Lexing


(* let filename = Sys.argv.(1) *)

let main () =
	let args = Sys.argv in
    if Array.length args = 2 then
      let filename = Sys.argv.(1) in
			(* let ic = open_in filename in *)
			let ic = In_channel.create filename in
			try 
        let lexbuf = Lexing.from_channel ic in
		  	let s = Parser.chapter Lexer.token lexbuf in
          printf "Parsed successfully chapter:\n%s" s
      with End_of_file -> exit 0
    else
      printf "Usage: top <filename>\n";;			
					
let _ = main ()
