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
				while true do
					Parser.chapter Lexer.token lexbuf
				done
					
      with End_of_file -> exit 0
let _ = Printexc.print main ()
