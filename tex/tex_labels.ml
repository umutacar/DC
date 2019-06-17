open Core
open Utils


module Words = English_words
module Tex = Tex_syntax

(* Turn off prints *)
let d_printf args = 
    ifprintf stdout args
let d_printf_strlist x y = 
	()

(* Labels *)

type t = (String.t, unit) Hashtbl.t

(** BEGIN: Globals **)
let label_counter = ref 0
(** END: Globals **)

let empty:unit -> t = fun () -> Hashtbl.create ~size:1024 (module String) 

(* Add label to the set *)
let add table label = 
  let _ = d_printf "Label_set.add label = %s:\n" label in
  try let _ = Hashtbl.find_exn table label  in
	    (d_printf "Label_set.add label = %s found in the table.\n" label;
     false)
    with Caml.Not_found -> 
      match Hashtbl.add table ~key:label ~data:() with
      | `Duplicate -> 
          (printf "Label_set.add: FATAL ERROR in Labeling.\n";
           exit Error_code.labeling_error_hash_table_corrupted)
      | `Ok -> true


let to_list table = 
	let all = Hashtbl.to_alist table in
	  List.map all ~f:(fun (x,y) -> x)

(* Create a new label from counter *)
let rec mk_label_from_number table = 
  let _ = label_counter := !label_counter + 1 in
  let label = 
		Tex.label_prefix_auto_pre ^ 
    (Int.to_string !label_counter) ^ 
    Tex.label_prefix_auto_pre  
	in
	  if add table label then
			(* succeeded. *)
			label
		else
			(* try again *)
			mk_label_from_number table 

(* Assuming that the label has of the form 
   (prefix as e.g., [ch | sec | cl ]) (separator as [:_]) label_name
   return label_name
 *)

(* If label has the form 
 * Examples: chapter:this -> this, ch:this-> this ch::this -> this.
 *
 * [A-Za-z]+[:_]+rest, where [:_]+ is the label delimiter, then
 * it splits the label into a prefix called "kind" and "rest".
 * It then checks that the kind starts with "ch" "sec" "gr" etc,
 * If it does, then it assumes that the label starts with a kind
 * prefix and returns the rest.
 *) 

let drop_label_prefix label = 
	let delimiter = Tex.pattern_label_delimiter in
  let tokens = Str.split (Str.regexp delimiter) label in
  if List.length tokens <= 1 then
    (* label does not have a kind prefixer *)
    label
  else
    (* Split into two at the colon 
     * kind has the form Str.Delim "xyz[:_]+"
     * rest has the form Str.Text  "xyz..." 
     *)

    let (Str.Delim kind)::(Str.Text rest)::nil = Str.bounded_full_split (Str.regexp "[A-Za-z]+[:_]+") label 2 in      
      if str_match_prefix Tex.pattern_ch_prefix kind ||
         str_match_prefix Tex.pattern_sec_prefix kind ||
         str_match_prefix Tex.pattern_gr_prefix kind then
         rest
      else
        label



(* Take kind, e.g., sec, gr, and prefix and a string list candidates make
   kind:prefix::s, for some s in candidates
 *)
let mk_label table kind prefix candidates = 
	let rec find candidates = 
		let _ = d_printf_strlist "Label_set.mk_label: candidates = %s\n" candidates in
    match candidates with 
    | [] -> 
        let _ = d_printf "Label_set.mk_label: failed to find a unique word.  Using unique.\n" in
        None
    | ls::rest ->
        let ls = kind ^ Tex.label_seperator ^ prefix ^ Tex.label_nestor ^ ls in
        let _ = d_printf "Label_set.mk_label: trying label = %s\n" ls in
        if add table ls then 
          let _ = d_printf "Label_set.add: Label = %s added to  the table.\n" ls in
          Some ls
        else
          let _ = d_printf "Label_set.add: Label = %s found in the table.\n" ls in
          find rest
  in
  find candidates

let mk_label_force table kind prefix candidates = 
  match candidates with 
  | [ ] -> 
    (* Generate based on numbers *)
    let ls = mk_label_from_number table in 
    let ls = kind ^ Tex.label_seperator ^ prefix ^ Tex.label_nestor ^ ls in
    let _ = d_printf "Label_set.mk_label_force label = %s\n" ls in
      ls
  | _ ->
    match mk_label table kind prefix candidates with 
    | None ->
      let ls = mk_label_from_number table in 
      let ls = kind ^ Tex.label_seperator ^ prefix ^ Tex.label_nestor ^ ls in
      let _ = d_printf "Label_set.forceCreateLabel: label = %s\n" ls in
			  ls
    | Some ls -> ls 
