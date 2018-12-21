open Core
open Lexer
open Lexing


let default_tmp_dir = "/tmp/"
let default_preamble_file = "/Users/umut/15210/algobook/latex_preamble/preamble.tex"
          
let mk_translator () = 
  let preamble = In_channel.read_all default_preamble_file in
    Tex2html.mk_translator (default_tmp_dir, preamble) 

let main () =
	let args = Sys.argv in
    if Array.length args >= 2 then
      let filename = Sys.argv.(1) in
			let ic = In_channel.create filename in
      let chapter_xml =   
  			try 
          let lexbuf = Lexing.from_channel ic in
	  	  	let ast_chapter = Parser.chapter Lexer.token lexbuf in
          let tex2html = mk_translator () in
          let chapter_xml = Ast.chapterToXml tex2html ast_chapter in
            printf "Parsed successfully chapter.\n";
            chapter_xml
        with End_of_file -> exit 0
      in 
        if Array.length args = 3 then    
          let outfile = Sys.argv.(2) in
            Out_channel.write_all outfile ~data:chapter_xml 
        else
          printf "Parsing complete."
    else
      printf "Usage: main <input latex file> [<output xml file>]\n";;			
					
let _ = main ()

