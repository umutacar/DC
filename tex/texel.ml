(* texel.ml 
 * Elaborator tool for LaTex.
 *) 
open Core
open Lexer
open Lexing

let verbose = ref false
let do_groups = ref false
let do_inline = ref false
let in_file = ref None
let out_file = ref None

(* Set string argument *)
let set_str_arg r v = r := Some v

(* Get value of string argument *)
let get_str_arg r v =
  match !r with 
  | None -> (printf "Fatal Error"; exit 1)
  | Some s -> s

(* Stub. *)
let handle_parser_error () = 
  printf ("Parse Error\n.")

let tex2ast infile = 
	let ic = In_channel.create infile in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with | End_of_file -> exit 0
         | Parser.Error as exn -> 
           handle_parser_error (); 
           raise exn


let tex2ast infile = 
	let ic = In_channel.create infile in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with End_of_file -> exit 0


let ast2tex ast_chapter = 
  Ast.chapterToTex ast_chapter


let elaborate do_inline do_groups infile = 

  let infile_inlined = 
    if do_inline then
      Preprocessor.inline_file infile 
    else
      infile
  in
  (* Make AST *)
  let ast = tex2ast infile_inlined in
  let ast =     
    if do_groups then
      Ast.chapterEl ast
    else 
      ast
  in
  (* Label AST *)
  let ast_labeled = Ast.labelChapter ast in

  (* Make TeX *)
  let result = ast2tex ast_labeled in
    result
					
let main () = 
  let spec = [
              ("-v", Arg.Set verbose, "Enables verbose mode; default is false.");
(*              ("-inline", Arg.Set do_inline, "Inline latex input directives; default is false."); *)
              ("-groups", Arg.Set do_groups, "Create groups.");
              ("-o", Arg.String (set_str_arg out_file), "Sets output file")
             ]
  in 

  let take_infile_name anon = 
    match !in_file with 
    | None -> in_file := Some anon
    | Some _ -> (printf "Warning: multiple input files specified, taking first.\n")
  in

  let usage_msg = "texel elaborates latex. \n Usage: texel <latex file>.\n Options available:" 
  in
  let _  = Arg.parse spec take_infile_name usage_msg in
  let in_file_name =  
    match !in_file with 
    | None -> (printf "Error: Missing input Latex file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | Some x -> x
  in
  let _ = printf "Executing command: texel %s\n" in_file_name in
  let result = elaborate !do_inline !do_groups in_file_name  in
    match !out_file with 
    | None -> 
        printf "Successfully translater chapter to TeX:\n %s\n" result 
    | Some outfile ->
      let _ = Out_channel.write_all outfile ~data:result in
      let _ = printf "Successfully translated chapter. Output in %s\n" outfile in
        ()

let _ = main ()


