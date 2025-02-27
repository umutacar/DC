open Core
open Lexer
open Lexing


(*
let filename = Sys.argv.(1) 
*)

let main () =
	let args = Sys.argv in
    if Array.length args >= 2 then
      let filename = Sys.argv.(1) in
			let ic = In_channel.create filename in
      let chapter_tex =   
  			try 
          let lexbuf = Lexing.from_channel ic in
	  	  	let chapter = Parser.chapter Lexer.token lexbuf in
          let chapter_tex = Ast.chapterToTex chapter in
            printf "Parsed successfully chapter.\n";
            chapter_tex
        with End_of_file -> exit 0
      in 
        if Array.length args = 3 then    
          let outfile = Sys.argv.(2) in
            Out_channel.write_all outfile ~data:chapter_tex 
        else
          (printf "Parsing complete: parsed text: \n%s" chapter_tex)
    else
      printf "Usage: main <input latex file> [<output latex file>]\n";;			
					
let _ = main ()

