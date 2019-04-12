open Core
open Lexer
open Lexing


let tex2ast infile = 
	let ic = In_channel.create infile in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with End_of_file -> exit 0


let ast2tex ast_chapter = 
  Ast.chapterToTex ast_chapter

let tex2tex infile = 
  (* Preprocessing I: Inline *)
  let infile_inlined = TexUtils.file_derivative infile Constants.ext_core in
  let () = TexUtils.inline infile infile_inlined in

  (* Make AST *)
  let ast = tex2ast infile_inlined in
  (* Elaborate AST *)
  let ast_elaborated = Ast.chapterEl ast in
  (* Make TeX *)
  let result = ast2tex ast_elaborated in
    result

let main () =
	let args = Sys.argv in
    if Array.length args = 2 then
      let infile = args.(1) in
      let result = tex2tex infile in
        printf "Successfully translater chapter to TeX: %s\n" result 
      else if Array.length args = 3 then    
        let infile = args.(1) in
        let outfile = args.(2) in
        let result = tex2tex infile in
        let _ = Out_channel.write_all outfile ~data:result in
        let _ = printf "Successfully tranlated chapter. Output in %s\n" outfile in
           ()
      else
         printf "Usage: main <input latex file> [<output latex file>]\n" ;;
					
let _ = main ()

