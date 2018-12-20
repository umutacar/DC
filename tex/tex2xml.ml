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
      let chapter_xml =   
  			try 
          let lexbuf = Lexing.from_channel ic in
	  	  	let chapter = Parser.chapter Lexer.token lexbuf in
          let chapter_xml = Ast.chapterToXml chapter in
            printf "Parsed successfully chapter.\n";
            chapter_xml
        with End_of_file -> exit 0
      in 
        if Array.length args = 3 then    
          let outfile = Sys.argv.(2) in
            Out_channel.write_all outfile ~data:chapter_xml 
        else
          (printf "Parsing complete: parsed text: \n%s" chapter_xml)
    else
      printf "Usage: main <input latex file> [<output xml file>]\n";;			
					
let _ = main ()

