open Core
open Printf

let debug = ref false

let int_times_float i f = 
  int_of_float(floor((float_of_int i) *. f))

let str_of_char x = String.make 1 x

let str_of_pval_opt x = 
  match x with 
  | None -> "None"
  | Some x -> "Some " ^ x


(* is C vertical space *)
let is_vert_space c = 
  c = '\n' || c = '\r' 

(* is C vertical space *)
let is_space c = 
  c = '\n' || c = '\r' || c = '\t' || c = ' ' 

let str_of_str_opt so = 
	match so with 
	| None -> ""
	| Some s -> s


let str_of_str_list (xs: string list): string = 
  String.concat ~sep:", " xs

let str_of_str2_list_with equals seperator (xs: (string * string) list): string = 
	let l = List.map xs ~f:(fun (item, body) -> item ^ equals ^ body) in 
  String.concat ~sep:seperator l

let str_of_kw_list_with equals seperator (xs: (string * string option) list): string = 
  let mapper (k, v) = 
    match v with 
    | None -> ""
    | Some v -> k ^ equals ^ v
  in
	let l = List.map xs ~f:mapper in
  String.concat ~sep:seperator l

let str_of_str2_list (xs: (string * string) list): string = 
  str_of_str2_list_with " " ", " xs


let str_of_items (xs: (string * string option * string option * string option * string) list): string = 
  let str_of_item (kind, tag, pvalopt, lopt, body) = 
    let label = 
      match lopt with 
			| None -> ""
			| Some l -> "\\label{" ^ l ^ "}\n"
		in
    let tag = 
      match tag with 
			| None -> ""
			| Some t -> t ^ ")"
		in
		match pvalopt with 
		| None -> kind ^ "\n" ^ label ^ body ^ "\n"
		| Some p -> kind ^ "[" ^ p ^ "]" ^ "\n" ^ label ^ body ^ "\n"
	in
	let l = List.map xs ~f:str_of_item in
  String.concat ~sep:", " l

(* BEGIN: Debug Prints *) 

let d_printf args = 
  if !debug then
    fprintf stdout args
  else 
    ifprintf stdout args

let v_printf be_verbose args = 
  if be_verbose then
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
let file_base_derivative filename deriv = 
  let (filename_, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_ ^ deriv
    | Some x -> filename_ ^ deriv ^ "." ^ x 

(* Given path/basename.ext 
 * Return path/diderot_basename.ext
 *)
let mk_atomic_filename tmp_dir filename = 
  let uuid = Uuid.to_string (Uuid.create ()) in
  let basename = Filename.basename filename in
  let dirname = Filename.dirname filename in  

  let atomic_basename = Constants.diderot_atomic ^ basename in 
  let atomic_basename_uuid = file_base_derivative atomic_basename ("-" ^ uuid) in
  let atomic_name =  Filename.concat dirname atomic_basename in
  let atomic_name_uuid = Filename.concat tmp_dir atomic_basename_uuid in
    (atomic_name, atomic_name_uuid)
													 
let mk_derivative_filename filename derivative = 
  let (filename_first, ext) = Filename.split_extension filename in
    filename_first ^ "." ^ derivative

let mk_xml_filename filename = 
  let (filename_first, ext) = Filename.split_extension filename in
    filename_first ^ "." ^ Constants.ext_xml

let file_ensure_tex filename =
  let (filename_first, ext) = Filename.split_extension filename in
    match ext with 
    | None -> filename_first ^ "." ^ Constants.ext_tex
    | Some x -> filename_first ^ x 

let file_is_markdown filename =
  let (filename_first, ext) = Filename.split_extension filename in
    match ext with 
    | None -> false
    | Some x -> 
				let _ = d_printf "file_is_markdown: %s\n" x in
				x = Constants.ext_markdown

let file_is_html filename =
  let (filename_first, ext) = Filename.split_extension filename in
    match ext with 
    | None -> false
    | Some x -> 
				let _ = d_printf "file_is_html %s\n" x in
				x = Constants.ext_html

let file_is_xml filename =
  let (filename_first, ext) = Filename.split_extension filename in
    match ext with 
    | None -> false
    | Some x -> 
				let _ = d_printf "file_is_xml: %s\n" x in
				x = Constants.ext_xml

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

let reduce (f: 'b -> 'b -> 'c) (xs: 'a list) : 'c option = 
	List.reduce xs f

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

let dedup_str_list l = 
  List.dedup String.compare l

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

(* This is dangerous.  If the body has things line \lstinline'x % 5 =
 *  0' then it is going to be treated as comment
 *)
let rm_comments body = 
  let diderot_percent = "__diderotpercentsign__" in
  let re_percent = Str.regexp "\\\\%" in
  let re_diderot_percent = Str.regexp diderot_percent in
  let re_comment = Str.regexp "[ \t]*%.*\n" in

  let body_nopercent = Str.global_replace re_percent diderot_percent body in
  let body_nocomment = Str.global_replace re_comment "" body_nopercent in
  let body_clean = Str.global_replace re_diderot_percent "\\%" body_nocomment in
    body_clean

(* Takes an atom body that has occurrences of the form 
   \begin{lstlisting}[language = my_language ...] 
   and cleanup it my_language by deleting [, ], {, }
   characters.
  
   Assumes that comments are removed.

   This algorithm is wonky.
   Ideally, it should iterate over the body string replace each match with its sanitized version.  This one replaces each match globally.  The only reason for this is that I could not find a primitive of the sort needed for iteration/mapping over the string.
 *)
let sanitize_lst_language body = 
  (* regexp, group \1 is the language matched *)
  let l = "\\\\begin{lstlisting}[ ]*\\[language[ ]*=[ ]*"  in
  let r = "\\({\\[[A-Za-z0-9]+\\][A-Za-z0-9]+}\\)" in
  let re_lst = Str.regexp (l ^ r) in
  let clean = Str.regexp "[]\\[{}]" in
  (* I want to define clean = Str.regexp "[\\]\\[{}]" but there is an exception
     * for close bracket.  It has to be the first char in the set.
     *)
  let mk_regexp s = Str.regexp (Str.quote s) in
  let next pos body = 
	try 
		let pos_match = Str.search_forward re_lst body pos in
		let lsthead = Str.matched_group 0 body in
		let language = Str.matched_group 1 body in
(*
		let _ = d_printf "matched lsthead = %s" lsthead in
		let _ = d_printf "matched language = %s" language in  
*)
    let language_c = Str.global_replace clean "" language in
    let lsthead_c = Str.global_replace (mk_regexp language) language_c lsthead in
    let body_c =  Str.global_replace (mk_regexp lsthead) lsthead_c body in
(*
    let _ = d_printf "Sanitized body = %s" body_c in
*)
		Some (pos_match, body_c, language, language_c)
	with
		Not_found -> (None)
	in
	let rec find pos body languages = 
		match (next pos body) with 
		| None -> 
				(body, languages)
		| Some (npos, nbody, language, language_c) -> 
				let l = (language, language_c)::languages in
				find (npos + 1) nbody l
	in
	find 0 body []

(* Find all the programming languages *)  
let find_lang contents  =
  let extract_lang (m: Re2.Match.t) =
    let source = Re2.Match.get ~sub:(`Name "lang") m in
      match source with 
      | None -> let _ = d_printf "utils.find_lang: None" in []
      | Some x -> let _ = d_printf "utils.find_lang: Some %s" x in [x]
  in
  (* The quad escape's are due to ocaml's string representation that requires escaping \ *)
  let regex = Re2.create_exn
                  "\\\\begin{lstlisting}\\[language[' ']*=[' ']*(?P<lang>[[:alnum:]]*)([','' ''=']|[[:alnum:]])*\\]"    
  in
  let pattern = Re2.pattern regex in
(*
  let _ = d_printf "utils.find_lang: Pattern for this regex = %s\no" pattern in 
*)
  let all_matches = Re2.get_matches_exn regex contents in
  let languages: string list = List.concat_map all_matches ~f:extract_lang in
(*  let _ = d_printf_strlist "utils.find_lange: languages" languages in *)
    languages

(* END String and substring search *) 


(* BEGIN: Operations on float options *) 
let float_opt_to_string fopt = 
  match fopt with 
  | None -> "None"
  | Some x -> 
    let f = Float.to_string x in
(*    let _ = d_printf ("float_opt_to_string: points = %f\n") x in *)
      "Some" ^ f


let float_opt_to_string_opt fopt = 
  match fopt with 
  | None -> None
  | Some x -> 
    let f = Float.to_string x in
(*    let _ = d_printf ("fopt_opt_to_string_opt: points = %f\n") x in *)
      Some f


(* END Operations on float options *) 

(* Association lists *)

(* Find value of key is list key-value list l *)
let find_in_list  (l: (string * string) list) (key: string) = 
	List.Assoc.find l ~equal:String.equal key


let drop_final_char s = 
  String.slice s 0 (String.length s - 1)


(* Construct a fill-in-the-blanks box for latex source *)
let mk_fill_in_box_latex len x =
  let content  = 
    match len with 
    | None -> 
			let target_per_char = "~" in 
			let l = String.length x in
      (* Make 50% larger box, of size at least 5 characters *)
			let ll = max 5 (int_times_float l 1.5) in
			List.init ll ~f:(fun i -> target_per_char) 
  	| Some l ->
	  		let target_per_char = "~" in 
  			List.init l ~f:(fun i -> target_per_char)
	in
	let lu = List.concat [["$\\lt$\\%\\%"]; content; ["\\%\\%$\\gt$"]] in 
	String.concat ~sep:"" lu

(* Construct a fill-in-the-blanks solution for latex source *)
let mk_fill_in_sol_latex len x =
	let target_per_char = "~" in 
    match len with 
    | None -> x
    | Some l ->
			let ll = max 2 (l - (String.length x) / 2) in
      let pad = String.concat ~sep:"" (List.init ll ~f:(fun i -> target_per_char)) in
      let all = List.concat [[pad]; [x]; [pad]] in 
      	String.concat ~sep:"" all

(* Construct a fill-in-the-blanks box for code source *)
let mk_fill_in_box_code x = 
  let target_per_char = " " in 
  let l = String.length x in
  let lu = List.init l ~f:(fun i -> target_per_char) in
  let lu = List.concat [["<%%"]; lu; ["%%>"]] in 
  String.concat ~sep:"" lu
