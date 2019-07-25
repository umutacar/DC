open Core
open Lexer
open Lexing

let tex2ast filename = 
	let ic = In_channel.create filename in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with End_of_file -> exit 0


let ast2tex ast_chapter = 
  let chapter_tex = Ast.chapterToTex ast_chapter in
      chapter_tex

let main () =
	let args = Sys.argv in
     if Array.length args = 3 then    
        let filename = args.(1) in
        let outfile = args.(2) in
        let ast = tex2ast filename in
        let ast_new = Ast.chapterEl ast in
        let result = ast2tex ast_new in
        let _ = Out_channel.write_all outfile ~data:result in
        let _ = printf "Successfully tranlated chapter. Output in %s\n" outfile in
           ()
      else
         printf "Usage: main <input latex file> <output latex file>\n" ;;
					
let _ = main ()

