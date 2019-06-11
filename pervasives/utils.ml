open Core
open Printf

let debug = false
let d_printf args = 
  if debug then
    fprintf stdout args
  else 
    ifprintf stdout args

let d_printf_opt_str heading sopt = 
  match sopt with 
  | None -> d_printf  "%s = None" heading
  | Some x -> d_printf "%s = %s " heading x 

let printf_strlist heading (xs: string list) = 
  let s = String.concat ~sep:", " xs in
    printf "%s = %s \n" heading s  

let d_printf_strlist heading (xs: string list) = 
  let s = String.concat ~sep:", " xs in
    d_printf "%s = %s \n" heading s  


let file_derivative filename deriv = 
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ deriv
    | Some x -> filename_ ^ deriv ^ "." ^ x 

let mk_xml_filename filename = 
  let (filename_first, ext) = Filename.split_extension filename in
    filename_first ^ "." ^ Constants.ext_xml


let file_ensure_tex filename =
  let (filename_first, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_first ^ "." ^ Constants.ext_tex
    | Some x -> filename_first ^ x 
