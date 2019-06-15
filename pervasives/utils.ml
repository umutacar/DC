open Core
open Printf

let debug = false

let str_of_str_list (xs: string list): string = 
  String.concat ~sep:", " xs

(* BEGIN: Debug Prints *) 


let d_printf args = 
  if debug then
    fprintf stdout args
  else 
    ifprintf stdout args

let d_printf_optstr heading sopt = 
  match sopt with 
  | None -> d_printf  "%s = None\n" heading
  | Some x -> d_printf "%s = %s\n" heading x 

let printf_strlist heading (xs: string list) = 
  let s = str_of_str_list xs in
    printf "%s = %s \n" heading s  

let d_printf_strlist heading (xs: string list) = 
  let s = str_of_str_list xs in
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

let map_reduce (f: 'a -> 'b) (g: 'b -> 'b -> 'c) (xs: 'a list) : 'c option = 
  let xs = List.map xs f in
    List.reduce xs g

(* Return a sublist of input consisting of elements of l that are unique in l.
   For example l = [0, 1, 1, 2] returns 0 and 2.
 *)
let uniques_of_list l = 
	let rec uniques rest = 
		match rest with 
		| [ ] -> [ ]
		| h::t ->
				if List.count l (fun x -> x = h) > 1 then
					uniques t
				else
					h:: (uniques t)
	in
	uniques l	 

(* END Operations on string lists *)



(* BEGIN: String and substring search *) 

(* Example "here" "here is an apple" will return ("here", "is an apple". 
   Requires:  there be at most one match of "here".
*)
let str_match_one_first (search:string) (target:string): (string * string) option = 
(*
	let _ = d_printf "first: searching: %s\n" search in
	let _ = d_printf "       in %s\n" target in
*)
	let re = Str.regexp search in
	let chunks = Str.full_split re target in
	match chunks with
	| h::[ ] -> 
(*			let _ = d_printf "no match\n" in *)
			None
	| ha::hb::_ -> 
			begin
			match (ha, hb) with 
			| (Str.Delim ha, Str.Text hb) -> Some (ha, hb)
			| _ -> None
			end


(* Example "there" "here is there" will return ("there", "here is"). *)
let str_match_last (search:string) (target:string): (string * string) option = 
(*	let _ = d_printf "last: searching: %s\n" search in *)
	let re = Str.regexp search in
	let chunks = List.rev (Str.full_split re target) in
	match chunks with
	| h::[ ] -> 
(*			let _ = d_printf "no match\n" in *)
			None 
	| ha::hb::_ -> 
			begin
			match (ha, hb) with 
			| (Str.Delim ha, Str.Text hb) -> Some (ha, hb)
			| _ -> None
			end

(* Example "here" "here is there" will return true. *)
let str_match_prefix (search:string) (target:string) = 
	let re = Str.regexp search in
  Str.string_match re target 0 

(* Example "here" "this is here" will return true. *)
let str_match_suffix (search:string) (target:string) = 
	let re = Str.regexp search in
	let i =  (String.length target) - (String.length search)   in
	Str.string_match re target i

let str_match_at (search:string) (target:string) (pos: int) = 
	let re = Str.regexp search in
	Str.string_match re target pos

(* Check that search string matches target exactly. *)
let str_match_full (search:string) (target:string): bool = 
(*	let _ = d_printf "last: searching: %s\n" search in *)
	let re = Str.regexp search in
	let chunks = Str.full_split re target in
	match chunks with
	| h::[ ] -> 
			begin
			match h with 
			| Str.Delim ha -> ha = target
			| _ -> false
			end
	| _ -> false
 
let contains_substring (search: string) (target: string) =
(*
  let _ = d_printf "contains_substring: search = %s target = %s\n" search target in
*)
  let found = String.substr_index ~pos:0 ~pattern:search target in
  let res = 
    match found with 
    | None -> 
(*				let _ = d_printf "contains_substring: found none\n" in *)
				false
    | Some x -> 
(*				let _ = d_printf "contains_substring: found match %d\n" x in *)
				true 
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
