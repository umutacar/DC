open Core
open Lexing

module Ast = Ast
module Comment_lexer = Tex_comment_lexer
module Tex_Lexer = Tex_lexer
module Md_Lexer = Md_lexer
module Tex_parser = Tex_parser
module Md_parser = Md_parser
module Md_translator = Md2html
module Tex_translator = Tex2html

let file_extension_xml = ".xml"
let arg_na = ref false   (* Non-atomic mode: don't dump atomized input. *)
let arg_verbose = ref false

(* Set string argument *)
let set_str_arg r v = r := Some v

(* Get value of string argument *)
let get_str_arg r v =
  match !r with 
  | None -> (printf "Fatal Error"; exit 1)
  | Some s -> s

let mk_md_translator verbose tmp_dir meta_dir default_pl = 
  let preamble = "" in
  Md_translator.mk_translator 
		verbose 
		tmp_dir 
		meta_dir
    default_pl
		preamble  

(* Make Tex to XML/HTML translator *)
let mk_tex_translator verbose tmp_dir meta_dir default_pl (preamble_file: string option) = 
  let preamble = 
    match preamble_file with 
    | None -> ""
    | Some x -> In_channel.read_all x
  in
    Tex_translator.mk_translator 
		verbose 
		tmp_dir 
		meta_dir
		default_pl
		preamble 


let prep_input_md verbose tmp_dir meta_dir default_pl infile (preamble_file: string option) =
  let contents = In_channel.read_all infile in
  (* Remove frontmatter if markdown *) 
	let contents = 
		let tripledash = Str.regexp "---" in
		if Str.string_match tripledash contents 0 then
			try 
				let pos = Str.search_forward tripledash contents 3 in
				String.slice contents (pos + 3) (String.length contents)
			with Not_found -> contents
		else
			contents					
	in
  (* Add newline to allow for an immediate header, e.g.,
   * \n# Chapter
   *)  
  let contents = "\n" ^ contents in
	let translator = mk_md_translator verbose tmp_dir meta_dir default_pl in
	(contents, translator)


let prep_input_tex verbose tmp_dir meta_dir default_pl infile (preamble_file: string option) =
	let ic = In_channel.create infile in
  let lexbuf = Lexing.from_channel ic in
  (* Remove comments *)
  let contents = Comment_lexer.lexer lexbuf in
  let translator = mk_tex_translator verbose tmp_dir meta_dir default_pl preamble_file in
	(contents, translator)


let ast_from_string (lex, parse) contents = 
(*
  let _ = printf "**contents:\n%s" contents in 
  let _ = printf "**contents done\n" in 
*)
	let ast = 
   	try 
(*      let lexbuf = Lexing.from_channel ic in *)
      let lexbuf = Lexing.from_string contents in
	    let ast = parse lex lexbuf in
			match ast with 
			| None -> (printf "Parse Error."; exit 1)
			| Some ast -> ast
    with End_of_file -> exit 0
	in
	let _ = Ast.validate ast in
  let _ = Ast.normalize ast in
  let _ = Ast.assign_labels ast in
  let _ = Ast.propagate_point_values ast in 
		ast

let input_to_xml is_md verbose tmp_dir meta_dir default_pl  infile preamble_file = 
  let (contents, translator) = 
		if is_md then
			prep_input_md verbose tmp_dir meta_dir default_pl infile preamble_file 
		else
			prep_input_tex verbose tmp_dir meta_dir default_pl infile preamble_file 
	in
  let ast = 
		if is_md then
			ast_from_string (Md_lexer.lexer, Md_parser.top) contents 
		else
			ast_from_string (Tex_lexer.lexer, Tex_parser.top) contents 
	in
  let tex = Ast.to_tex ast in
  let xml = Ast.to_xml translator ast in
    (tex, xml)

let main () = 
	let arg_default_pl = ref None in
  (* Current directory by default *)
  let arg_meta_dir = ref "./meta"  in
	let arg_tmp_dir = ref "/tmp" in
	let arg_bib_file = ref None in
	let arg_preamble_file = ref None in
	let arg_infile = ref None in
	let arg_outfile = ref None in

  let spec = [
              ("-d", Arg.Set Utils.debug, "Enables debug mode; default is false.");
              ("-v", Arg.Set arg_verbose, "Enables verbose mode; default is false.");
		          ("-na", Arg.Set arg_na, "Turn off atomic mode; default is false.");
              ("-o", Arg.String (set_str_arg arg_outfile), "Sets output file");
              ("-meta", Arg.Set_string arg_meta_dir, "(Latex Only) Directory for meta information, e.g., highlighting definitions, lua filters, etc. Default = ./meta");
(*  Don't support default language.
              ("-lang", Arg.String (set_str_arg arg_default_pl), "Sets the default programming language.");
*)
              ("-tmp", Arg.Set_string arg_tmp_dir, "Sets the temporary directory, default is /tmp.");
              ("-preamble", Arg.String (set_str_arg arg_preamble_file), "(Latex Only) Sets LaTeX preamble, if any.");
              ("-bib", Arg.String (set_str_arg arg_bib_file), "(LaTeX only) Sets bibliography (bib) file if any.");
             ]
  in 

  let take_infile_name anon = 
    match !arg_infile with 
    | None -> arg_infile := Some anon
    | Some _ -> (printf "Warning: multiple input files specified, taking first.\n")
  in

  let usage_msg = "dc translates LaTeX/Markdown to Diderot XML. \n Usage: dc <file>.\n Options available:" 
  in
  let _  = Arg.parse spec take_infile_name usage_msg in
  let infile_name =  
    match !arg_infile with 
    | None -> (printf "Error: Missing input file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | Some x -> x
  in
  let outfile_name = 
		match !arg_outfile with 
    | None -> 
        let x = Utils.mk_xml_filename infile_name in
        (arg_outfile := Some x; x)
    | Some x -> x
  in
  let _ = printf "Executing command: dc %s\n" infile_name in
  let is_md = Utils.file_is_markdown infile_name in
  let (tex, xml) = input_to_xml is_md !arg_verbose !arg_tmp_dir !arg_meta_dir !arg_default_pl 
                         infile_name !arg_preamble_file in       
	let _ = 
		if is_md then
			()
		else
			match !arg_preamble_file with 
			| None -> printf "Warning: no LaTeX preamble was specified.\n"
			| _ -> ()
	in
  (* Write out atomic input file. *)
  let _ = 
		if !arg_na then
			()
		else
			let (atomic_file_name, atomic_file_name_uuid) = Utils.mk_atomic_filename !arg_tmp_dir infile_name in
			let _ = Out_channel.write_all atomic_file_name ~data:tex in
			let _ = Out_channel.write_all atomic_file_name_uuid ~data:tex in
			printf "Atomized input is in %s.\nUUID's version is in %s\n" atomic_file_name atomic_file_name_uuid
	in
  (* Write out xml output. *)
  let _ = Out_channel.write_all outfile_name ~data:xml in
  printf "Output written in %s\n" outfile_name 

let _ = main ()

