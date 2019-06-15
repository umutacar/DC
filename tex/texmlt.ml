open Core
open Lexing

module Ast = Ast_ast
module Lexer = Atom_lexer
module Parser = Atom_parser

let file_extension_xml = ".xml"
let verbose = ref false
let do_inline = ref false
let default_lang = ref None
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

let mk_translator be_verbose lang_opt preamble_filename = 
  let preamble = 
    match preamble_filename with 
    | None -> ""
    | Some x -> In_channel.read_all x
  in
    Tex2html.mk_translator_auto be_verbose !tmp_dir lang_opt preamble 


let tex2ast infile = 
	let ic = In_channel.create infile in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast = Parser.top Lexer.lexer lexbuf in
			match ast with 
			| None -> (printf "Parse Error."; exit 1)
			| Some ast -> ast
    with End_of_file -> exit 0

let ast2xml be_verbose lang_opt ast preamble_file = 
  (* Elaborate AST *)
(*
  let ast_elaborated = Ast.chapterEl ast_chapter in
*)
  (* Label AST *)
  let _ = Ast.assign_labels ast in

  (* Make XML *)
  let tex2html = mk_translator be_verbose lang_opt preamble_file in
  let xml = Ast.Segment.to_xml tex2html ast in
    printf "Parsed successfully chapter.\n";
    xml

let tex2xml be_verbose do_inline infile preamble_file lang_opt = 
  (* Preprocess *)
  let infile_inlined = 
        if do_inline then
          Preprocessor.inline_file infile
        else 
          infile
  in
  (* Make AST *)
	let ast_chapter = tex2ast infile_inlined in

  (* Translate to XML *)
  let xml_chapter = ast2xml be_verbose lang_opt ast_chapter preamble_file in
    xml_chapter

					
let main () = 
  let spec = [
              ("-v", Arg.Set verbose, "Enables verbose mode; default is false.");
(*              ("-inline", Arg.Set do_inline, "Inline latex input directives; default is false."); *)
              ("-tmp", Arg.Set_string tmp_dir, "Sets the temporary directory, default is /tmp.");
              ("-lang", Arg.String (set_str_arg default_lang), "Sets the default programming language.");
              ("-preamble", Arg.String (set_str_arg preamble_file), "Sets LaTeX preamble, if any.");
              ("-bib", Arg.String (set_str_arg bib_file), "Sets bibliography (bib) file if any.");
              ("-o", Arg.String (set_str_arg out_file), "Sets output file")
             ]
  in 

  let take_infile_name anon = 
    match !in_file with 
    | None -> in_file := Some anon
    | Some _ -> (printf "Warning: multiple input files specified, taking first.\n")
  in

  let usage_msg = "texmlt translates latex to XML. \n Usage: texmlt <latex file>.\n Options available:" 
  in
  let _  = Arg.parse spec take_infile_name usage_msg in
  let in_file_name =  
    match !in_file with 
    | None -> (printf "Error: Missing input Latex file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | Some x -> x
  in
  let _ = printf "Executing command: texmlt %s" in_file_name in
  let outfile_name = match !out_file with 
          | None -> 
            let x = Utils.mk_xml_filename in_file_name in
              (out_file := Some x; x)
          | Some x -> x
  in
  let xml_chapter = tex2xml !verbose !do_inline in_file_name !preamble_file !default_lang in       
  let _ = Out_channel.write_all outfile_name ~data:xml_chapter in
    (match !preamble_file with 
     | None -> printf "Warning: no LaTeX preamble was specified.\n"
     | _ -> ()
     ;
     printf "Output written in %s\n" outfile_name 
    )
  

let _ = main ()

