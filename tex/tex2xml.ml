open Core
open Lexer
open Lexing

let verbose = ref false
let default_lang = ref "C"
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

let mk_translator lang_opt preamble_filename = 
  let preamble = In_channel.read_all preamble_filename in
    Tex2html.mk_translator !tmp_dir lang_opt preamble 

let tex2ast infile = 
	let ic = In_channel.create infile in
   	try 
      let lexbuf = Lexing.from_channel ic in
	    let ast_chapter = Parser.chapter Lexer.token lexbuf in
        ast_chapter
    with End_of_file -> exit 0

let ast2xml lang_opt ast_chapter preamble_filename = 
  (* Elaborate AST *)
  let ast_elaborated = Ast.chapterEl ast_chapter in

  (* Label AST *)
  let ast_labeled = Ast.labelChapter ast_elaborated in

  (* Make XML *)
  let tex2html = mk_translator lang_opt preamble_filename in
  let chapter_xml = Ast.chapterToXml tex2html ast_labeled in
    printf "Parsed successfully chapter.\n";
    chapter_xml

let tex2xml do_inline infile preamble_filename lang_opt = 
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
  let xml_chapter = ast2xml lang_opt ast_chapter preamble_filename in
    xml_chapter

let main () =
	let args = Sys.argv in
    if Array.length args == 4 then
      let infile = Sys.argv.(1) in
      let preamble_filename = Sys.argv.(2) in
      let outfile = Sys.argv.(3) in
      let default_lang = Some "c" in
      let do_inline = true in
      let xml_chapter = tex2xml do_inline infile preamble_filename default_lang in      
         Out_channel.write_all outfile ~data:xml_chapter 
    else if Array.length args == 5 then
      let infile = Sys.argv.(1) in
      let preamble_filename = Sys.argv.(2) in
      let outfile = Sys.argv.(3) in
      let default_lang = Some (Sys.argv.(4)) in
      let do_inline = true in
      let xml_chapter = tex2xml do_inline infile preamble_filename default_lang in      
         Out_channel.write_all outfile ~data:xml_chapter 
    else if Array.length args == 6 then
      let infile = Sys.argv.(1) in
      let preamble_filename = Sys.argv.(2) in
      let outfile = Sys.argv.(3) in
      let default_lang = Some (Sys.argv.(4)) in
      let do_inline = bool_of_string (Sys.argv.(5)) in
      let xml_chapter = tex2xml do_inline infile preamble_filename default_lang in      
         Out_channel.write_all outfile ~data:xml_chapter 
    else
      printf "Usage: tex2xml  <input latex file> <input preamble file> <output xml file> [default language = *c/java/...] [inline = *true/false]\n";;			
					
let main () = 
  let spec = [
              ("-v", Arg.Set verbose, "Enables verbose mode");
              ("-tmp", Arg.Set_string tmp_dir, "Sets the temporary directory, default is /tmp");
              ("-lang", Arg.Set_string default_lang, "Sets the default programming language, default is C");
              ("-bib", Arg.String (set_str_arg bib_file), "Sets bibliography (bib) file if any");
              ("-o", Arg.String (set_str_arg out_file), "Sets output file")
             ]
  in 

  let preamble_and_infile anon = 
    match !preamble_file with 
    | None -> preamble_file := Some anon
    | Some _ -> 
      match !in_file with 
      | None -> in_file := Some anon
      | Some _ -> ()
  in

  let usage_msg = "tex2xml translates latex to XML. \n Usage: tex2xml <latex preamble> <latex file>. Options available:" 
  in
  let _  = Arg.parse spec preamble_and_infile usage_msg in
  let (preamble_file_name, in_file_name) =  
    match (!preamble_file, !in_file) with 
    | (None, _) -> (printf "Error: Missing preamble file!\n %s" (Arg.usage_string spec usage_msg); exit 1)
    | (_, None) -> (printf "Error: Missing input Latex file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | (Some p, Some i) -> (p, i)
  in
  let _ = printf "Executing command: tex2xml %s %s" preamble_file_name in_file_name in
    ()
let _ = main ()

