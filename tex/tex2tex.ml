open Core
open Lexer
open Lexing

let tex2tex filename = 
	let ic = In_channel.create filename in
  let chapter_tex =   
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let chapter = Parser.chapter Lexer.token lexbuf in
      let chapter_tex = Ast.chapterToTex chapter in
        chapter_tex
    with End_of_file -> exit 0
  in
    chapter_tex

let main () =
	let args = Sys.argv in
    if Array.length args = 2 then
      let filename = args.(1) in
      let result = tex2tex filename in
        printf "Parsed successfully chapter: %s\n" result 
      else if Array.length args = 3 then    
        let filename = args.(1) in
        let outfile = args.(2) in
        let result = tex2tex filename in
          Out_channel.write_all outfile ~data:result
      else
         printf "Usage: main <input latex file> [<output latex file>]\n" ;;
					
let _ = main ()

