open Core
open Lexer
open Lexing

let default_tmp_dir = "/tmp"


let mk_translator preamble_filename = 
  let preamble = In_channel.read_all preamble_filename in
    Tex2html.mk_translator (default_tmp_dir, preamble) 

let tex2ast filename = 
	let ic = In_channel.create filename in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with End_of_file -> exit 0

let main () =
	let args = Sys.argv in
    if Array.length args == 4 then
      let filename = Sys.argv.(1) in
      let preamble_filename = Sys.argv.(2) in

      (* Make AST *)
	  	let ast = tex2ast filename in
      (* Elaborate AST *)
      let ast_elaborated = Ast.chapterEl ast in
      (* Make XML *)
      let chapter_xml =   
  			try 
          let tex2html = mk_translator preamble_filename in
          let chapter_xml = Ast.chapterToXml tex2html ast_elaborated in
            printf "Parsed successfully chapter.\n";
            chapter_xml
        with End_of_file -> exit 0
      in 
      let outfile = Sys.argv.(3) in
         Out_channel.write_all outfile ~data:chapter_xml 
    else
      printf "Usage: main <input latex file> <input preamble file> <output xml file>\n";;			
					
let _ = main ()

