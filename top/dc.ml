open Core
open Lexing

module Ast = Ast
module Lexer = Tex_lexer
module Parser = Tex_parser
module MDTranslator = Md2html
module TexTranslator = Tex2html

let file_extension_xml = ".xml"
let verbose = ref false
let do_inline = ref false
let default_pl = ref None
let meta_dir = ref "."  (* Current directory by default *)
let tmp_dir = ref "/tmp"
let bib_file = ref None
let preamble_file = ref None
let in_file = ref None
let out_file = ref None


(* Set string argument *)
let set_str_arg r v = r := Some v

(* Get value of string argument *)
let get_str_arg r v =
  match !r with 
  | None -> (printf "Fatal Error"; exit 1)
  | Some s -> s

let mk_tex_translator preamble_file default_pl = 
  let preamble = 
    match preamble with 
    | None -> ""
    | Some x -> In_channel.read_all x
  in
    TexTranslator.mk_translator 
		!verbose 
		!tmp_dir 
		!meta_dir
		default_pl
		preamble 

let mk_md_translator default_pl = 
  let preamble = "" in
  MDTranslator.mk_translator 
		!verbose 
		!tmp_dir 
		!meta_dir
    default_pl
		preamble 


let prep_input infile =
  let is_md = Utils.file_is_markdown infile in
  let contents = In_channel.read_all infile in

  (* Remove frontmatter if markdown *) 
  let contents = 
		if is_md then
			let tripledash = Str.regexp "---" in
			if Str.string_match tripledash contents 0 then
				try 
					let pos = Str.search_forward tripledash contents 3 in
					String.slice contents (pos + 3) (String.length contents)
				with Not_found -> contents
			else
				contents
		else
			contents
  let translator =  
		if is_md then
			mk_md_translator default_pl
		else
			mk_tex_translator preamble_file default_pl 
	in
	(contents, translator)


let ast_from_string contents = 
(*
  let _ = printf "**contents:\n%s" contents in 
  let _ = printf "**contents done\n" in 
*)
	let ast = 
   	try 
(*      let lexbuf = Lexing.from_channel ic in *)
      let lexbuf = Lexing.from_string contents in
	    let ast = Parser.top Lexer.lexer lexbuf in
			match ast with 
			| None -> (printf "Parse Error."; exit 1)
			| Some ast -> ast
    with End_of_file -> exit 0
	in
	let _ = Ast.validate ast in
  let _  = Ast.normalize ast in
  let _ = Ast.assign_labels ast in
		ast

let input_to_xml infile  outfile preamble_file default_pl = 
  let (contents, translator) = prep_input infile in
  let ast = ast_from_string contents in
  let xml = Ast.to_xml translator ast in

let main () = 
  let spec = [
              ("-v", Arg.Set verbose, "Enables verbose mode; default is false.");
(*              ("-inline", Arg.Set do_inline, "Inline latex input directives; default is false."); *)
              ("-o", Arg.String (set_str_arg out_file), "Sets output file");
              ("-meta", Arg.Set_string meta_dir, "Directory for meta informaiton, e.g., highlighting definitions, lua filters, etc.");
(*  Don't support default language.
              ("-lang", Arg.String (set_str_arg default_pl), "Sets the default programming language.");
*)
              ("-tmp", Arg.Set_string tmp_dir, "Sets the temporary directory, default is /tmp.");
              ("-preamble (LaTeX only)", Arg.String (set_str_arg preamble_file), "Sets LaTeX preamble, if any.");
              ("-bib (LaTeX only)", Arg.String (set_str_arg bib_file), "Sets bibliography (bib) file if any.");
             ]
  in 

  let take_infile_name anon = 
    match !in_file with 
    | None -> in_file := Some anon
    | Some _ -> (printf "Warning: multiple input files specified, taking first.\n")
  in

  let usage_msg = "dc translates LaTeX/Markdwon to Diderot XML. \n Usage: dc <file>.\n Options available:" 
  in
  let _  = Arg.parse spec take_infile_name usage_msg in
  let in_file_name =  
    match !in_file with 
    | None -> (printf "Error: Missing input file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | Some x -> x
  in
  let outfile_name = match !out_file with 
          | None -> 
            let x = Utils.mk_xml_filename in_file_name in
              (out_file := Some x; x)
          | Some x -> x
  in
  let _ = printf "Executing command: dc %s\n" in_file_name in
  let xml = input_to_xml in_file_name outfile_name !preamble_file  !verbose  !default_pl in       
  let _ = Out_channel.write_all outfile_name ~data:xml in
	let _ = 
		match !preamble_file with 
		| None -> printf "Warning: no LaTeX preamble was specified.\n"
    | _ -> ()
	in
     printf "Output written in %s\n" outfile_name 
		
  

let _ = main ()

