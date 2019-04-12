open Core
open Printf

let debug = true
let d_printf args = 
  if debug then
    fprintf stdout args
  else 
    ifprintf stdout args



let file_derivative filename deriv = 
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ deriv
    | Some x -> filename_ ^ deriv ^ x 


let file_tex filename = 
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ Constants.ext_tex
    | Some x -> filename_ ^ x 
