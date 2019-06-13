open Core
open Utils


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
           exit ErrorCode.labeling_error_hash_table_corrupted)
      | `Ok -> true


let to_list table = 
	let all = Hashtbl.to_alist table in
	  List.map all ~f:(fun (x,y) -> x)

(* Create a new label from counter *)
let mk_label_from_number () = 
  let _ = label_counter := !label_counter + 1 in
    Tex_syntax.label_prefix_auto_pre ^ 
    (Int.to_string !label_counter) ^ 
    Tex_syntax.label_prefix_auto_pre  

(* Assuming that the label has of the form 
   (prefix as e.g., [ch | sec | cl ]) (separator as [:]) label_name
   ???????
 *)

let mk_label_prefix label = 
  let tokens = Str.split (Str.regexp (":")) label in
  if List.length tokens <= 1 then
    (* label does not have a kind prefixer *)
    label
  else
    (* Split into two at the colon 
     * kind has the form Str.Delim "xyz[:]+"
     * rest has the form Str.Text  "xyz..." 
     *)

    let (Str.Delim kind)::(Str.Text rest)::nil = Str.bounded_full_split (Str.regexp "[A-Za-z]+[:]+") label 2 in
      if str_match_prefix Tex_syntax.pattern_ch_prefix kind ||
         str_match_prefix Tex_syntax.pattern_sec_prefix kind ||
         str_match_prefix Tex_syntax.pattern_gr_prefix kind then
         rest
      else
        label




(* Take kind, e.g., sec, gr, and prefix and a string s make
   kind:prefix::s
 *)
let mk_label table kind prefix body = 
	let candidates = English_words.tokenize_spaces body in
	let rec find candidates = 
		let _ = d_printf_strlist "Label_set.createLabel: candidates = %s\n" candidates in
    match candidates with 
    | [] -> 
        let _ = d_printf "Label_set.createLabel: failed to find a unique word.  Using unique.\n" in
        None
    | ls::rest ->
        let ls = kind ^ Tex_syntax.label_seperator ^ prefix ^ Tex_syntax.label_nestor ^ ls in
        let _ = d_printf "Label_set.createLabel: trying label = %s\n" ls in
        if add table ls then 
          let heading = Tex_syntax.mk_label_force ls in
          let _ = d_printf "Label_set.add: Label = %s added to  the table.\n" ls in
          Some (heading, ls)
        else
          let _ = d_printf "Label_set.add: Label = %s found in the table.\n" ls in
          find rest
  in
  find candidates

let mk_label_force table kind prefix topt body_opt = 
  match (topt, body_opt) with 
  | (None, None) -> 
    (* Generate based on numbers *)
    let ls = mk_label_from_number () in 
    let ls = kind ^ Tex_syntax.label_seperator ^ prefix ^ Tex_syntax.label_nestor ^ ls in
    let _ = d_printf "Label_set.forceCreateLabel: label = %s\n" ls in
    let heading = Tex_syntax.mk_label_force ls in
      (heading, ls)
  | _ ->
  let body =
     match body_opt with 
    | None -> ""
    | Some body  -> body
  in
  let all = 
    match topt with 
    | None -> body
    | Some title  ->  (title ^ " " ^ body)
  in 
    match mk_label table kind prefix all with 
    | None ->
      let ls = mk_label_from_number () in 
      let ls = kind ^ Tex_syntax.label_seperator ^ prefix ^ Tex_syntax.label_nestor ^ ls in
      let _ = d_printf "Label_set.forceCreateLabel: label = %s\n" ls in
      let heading = Tex_syntax.mk_label_force ls in
        (heading, ls)
    | Some (heading, ls) -> 
        (heading, ls)
