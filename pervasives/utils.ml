open Core
open Printf

let debug = true

(* BEGIN: Debug Prints *) 


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

(* END Debug Prints *) 


(* BEGIN: File names etc *) 
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

(* END File names etc *) 

(* BEGIN: Operations on string lists *)
let map_concat f xs: string = 
  let xs_s: string list = List.map xs f in
  let result = List.fold_left xs_s  ~init:"" ~f:(fun result x -> result ^ x) in
    result

let map_concat_with connective f xs: string = 
  let xs_s: string list = List.map xs f in
  let result = List.fold_left xs_s  ~init:"" ~f:(fun result x -> result ^ connective ^ x) in
    result
(* END Operations on string lists *)

(* BEGIN: String and substring search *) 

let str_match_prefix search target = 
  Str.string_match search target 0 

let contains_substring search target =
  let _ = d_printf "contains_substring: search = %s target = %s\n" search target in
  let found = String.substr_index ~pos:0 ~pattern:search target in
  let res = 
    match found with 
    | None -> let _ = d_printf "contains_substring: found none\n" in false
    | Some x -> let _ = d_printf "contains_substring: found match %d\n" x in true 
  in
    res
(* END String and substring search *) 


(* BEGIN: Operations on float options *) 
let float_opt_to_string fopt = 
  match fopt with 
  | None -> "None"
  | Some x -> 
    let f = Float.to_string x in
    let _ = d_printf ("float_opt_to_string: points = %f\n") x in
      "Some" ^ f


let float_opt_to_string_opt fopt = 
  match fopt with 
  | None -> None
  | Some x -> 
    let f = Float.to_string x in
    let _ = d_printf ("fopt_opt_to_string_opt: points = %f\n") x in
      Some f


(* END Operations on float options *) 
