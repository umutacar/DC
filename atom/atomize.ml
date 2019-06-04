(* texel.ml 
 * Elaborator tool for LaTex.
 *) 
open Core
open AtomLexer
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


let elaborate do_inline do_groups infile = 
  let data = In_channel.read_all infile in
(*  let _ = printf "data = %s" data in *)
	let ic = In_channel.create infile in

   	try 
      let lexbuf = Lexing.from_channel ic in
	    let result = Atom_parser.chapter Atom_lexer.lexer lexbuf in
        result
    with End_of_file -> exit 0

					
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

  let usage_msg = "atomize elaborates latex by giving each atom a label. \n Usage: texel <latex file>.\n Options available:" 
  in
  let _  = Arg.parse spec take_infile_name usage_msg in
  let in_file_name =  
    match !in_file with 
    | None -> (printf "Error: Missing input Latex file! \n%s" (Arg.usage_string spec usage_msg); exit 1)
    | Some x -> x
  in
  let _ = printf "Executing command: atomize %s\n" in_file_name in
  let result = elaborate !do_inline !do_groups in_file_name  in
    match !out_file with 
    | None -> 
        printf "Successfully translater chapter to TeX:\n %s\n" result 
    | Some outfile ->
      let _ = Out_channel.write_all outfile ~data:result in
      let _ = printf "Successfully translated chapter. Output in %s\n" outfile in
        ()

let _ = main ()


