open Core
open Lexing

module Comment_lexer = Tex_comment_lexer

let arg_verbose = ref false

(* Set string argument *)
let set_str_arg r v = r := Some v

(* Get value of string argument *)
let get_str_arg r v =
  match !r with 
  | None -> (printf "Fatal Error"; exit 1)
  | Some s -> s

let rm_comments infile = 
	let ic = In_channel.create infile in
  let lexbuf = Lexing.from_channel ic in
  let contents = Comment_lexer.lexer lexbuf in
    contents


let main () = 
	let arg_infile = ref None in
	let arg_outfile = ref None in

  let spec = [
              ("-o", Arg.String (set_str_arg arg_outfile), "Sets output file");
             ]
  in 

  let take_infile_name anon = 
    match !arg_infile with 
    | None -> arg_infile := Some anon
    | Some _ -> (printf "Warning: multiple input files specified, taking first.\n")
  in

  let usage_msg = "cc removes comments from a LaTeX file. \n Usage: cc <file>.\n Options available:" 
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
        let x = Utils.file_derivative infile_name "nc" in
        (arg_outfile := Some x; x)
    | Some x -> x
  in
  let _ = printf "Executing command: cc %s\n" infile_name in
  let result = rm_comments infile_name in
  let _ = Out_channel.write_all outfile_name ~data:result in
  printf "Output written in %s\n" outfile_name 
		
  

let _ = main ()

