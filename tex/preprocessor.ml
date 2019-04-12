(**********************************************************************
 ** tex/preprocessor.ml
 **********************************************************************)
open Core
open Stdio
open Utils
open Re2

(* Inline the contents of \input{filename} directives *)
let inline_file_to infile outfile  =
  let inline_file (m: Match.t) =
    let source = Match.get ~sub:(`Name "filename") m in
    let target = 
      match source with 
      | None -> raise (Constants.Fatal_Error "texUtils.inline: match returned none")
      | Some x -> In_channel.read_all (Utils.file_ensure_tex x)
    in 
    let _ = d_printf "texUtils.inline: source = %s \n" (Option.value ~default:"" source) in
    let _ = d_printf "texUtils.inline: target = %s \n" target in
      target
  in

  (* The quad escape's are due to ocaml's string representation that requires escaping \ *)
  let regex = create_exn "\\\\input{(?P<filename>[^{}]*)}" in
  let pattern = pattern regex in
  let _ = d_printf "TexUtils.inline: Pattern for this regex = %s\n" pattern in 

  (* Read in contents. *)
  let contents = In_channel.read_all infile in
  let contents_new = ok_exn (replace inline_file regex contents) in
(*  let _ = d_printf "texUtils.inline: contents = %s" contents in *)
  let oc = Out_channel.create outfile in
  let () =  (fprintf oc "%s\n" contents_new; Out_channel.close oc) in
(*  let _ = d_printf "texUtils.inline: contents completed." in *)
    ()


let inline_file infile = 
  (* Preprocessing I: Inline *)
  let infile_base = Filename.basename infile in
  let infile_inlined = Utils.file_derivative infile_base Constants.ext_core in
  let infile_inlined = Constants.tmp_dir_name  ^ "/" ^ infile_inlined in
  let () = inline_file_to infile infile_inlined in
    infile_inlined