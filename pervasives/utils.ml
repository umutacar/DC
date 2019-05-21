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


let file_derivative filename deriv = 
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ deriv
    | Some x -> filename_ ^ deriv ^ "." ^ x 


let file_ensure_tex filename =
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ Constants.ext_tex
    | Some x -> filename_ ^ x 
