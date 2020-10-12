(********************************************************************** 
 ** ast/ast.ml
 **********************************************************************)
open Core
open Utils
(*
let d_printf args = printf args
*)
module Labels = Tex_labels
module Md = Md_syntax
module Tex = Tex_syntax
module Words = English_words
module Xml = Xml_syntax

type ast_member = Ast_cookie | Ast_prompt | Ast_problem | Ast_atom | Ast_group | Ast_element | Ast_block | Ast_segment

(* An item is a kind * tag option * point value option * label option * body 
 * all are strings.
 *)
type t_item = (string * string option * string option *  string option * string)

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)
let newline = Tex.newline
let space = Tex.space
let correct_choice_indicator = Tex.correct_choice_indicator

let points_correct = 1.0
let points_incorrect = 0.0

let fmt_lstlisting_pl = 
	sprintf "\\begin{lstlisting}[language=%s,numbers=left]\n%s\n\\end{lstlisting}"

let fmt_lstlisting_nopl = 
	sprintf "\\begin{lstlisting}[numbers=left]\n%s\\end{lstlisting}"



(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Utilities
 **********************************************************************)

let force_float_string (points:string option) = 
	match points with
	| None -> "0.0"
	| Some p -> p

let multiply_points (points:string) (multiplier: string) = 
	let points = float_of_string points in
	let multiplier = float_of_string multiplier in
  let points = points *. multiplier in
	Float.to_string points

let divide_points (points:string)  (divisor: string) = 
	let points = float_of_string points in
	let divisor = float_of_string divisor in
  let points = points /. divisor in
	Float.to_string points

let add_points (x: string) (y: string) = 
	let x = float_of_string x in
	let y = float_of_string y in
	  Float.to_string (x +. y)

let max_points (x: string) (y: string) = 
	let x = float_of_string x in
	let y = float_of_string y in
  let max = if x > y then x else y in
	  Float.to_string max


(* if points (po) is None or 0.0 then
   empty string else the points *)
let normalize_point_val po = 
	match po with 
  | None -> None
  | Some p -> 
			let pts = float_of_string p in
			if pts = 0.0 then None
      else Some (Float.to_string pts)


(* In a question = list of prompts, 
 * a factor is a correct choice or a solution field
 *)
let sum_factors kind prompts = 
  let factor_of_prompt prompt =
		let item = List.nth_exn prompt 0 in
    let (kind, tag, pval, lopt, _) = item in
 		let _ = d_printf "factor_of_prompt: kind = %s\n" kind in
 		if Tex.is_scorable_prompt kind then 
      match pval with 
			| None -> Tex.point_value_of_prompt kind
			| Some pts -> pts 
		else Constants.zero_points
	in
	let counts = List.map prompts ~f:factor_of_prompt in
    if Tex.is_max_scorable kind then
    	match List.reduce counts ~f:max_points with 
    	| None -> 
    		let err = "Syntax Error: expecting a solution prompt (\\sol, \\choice, \\choice*) but found none.\n" in
        let _ = printf "%s\n" err in
    		raise (Constants.Syntax_Error err)
    	| Some c -> 	c
    else 
    	match List.reduce counts ~f:add_points with 
    	| None -> 
    		let err = "Syntax Error: expecting a solution prompt (\\sol, \\choice, \\choice*) but found none.\n" in
        let _ = printf "%s\n" err in
    		raise (Constants.Syntax_Error err)
    	| Some c -> 	c


(* prompts is of the form
 * [ [prompt_item, cookie_item, cookie_item], 
 *   [prompt_item, cookie_item, cookie_item], ...
 * ]
 *)
let assign_points_to_question_prompts (multiplier: string) (prompts: (t_item list) list)
	 : (t_item list) list 
	 = 
  let assign prompt =
    match prompt with 
		| [ ] -> 
				let err = "Fatal Error: Expecting a prompt None found" in
  			let _ = printf "%s\n" err in
				raise (Constants.Fatal_Error err)
		| p::cookies ->
				 let (kind, tag, pval, lopt, body) = p in
	       if Tex.is_scorable_prompt kind then
        	 let _ = d_printf "assign_points_to_question_prompts: kind = %s is scoroable\n" kind in
           let points = 
             match pval with 
          	 | None -> Tex.point_value_of_prompt kind 
          	 | Some points -> points in
           let points = multiply_points multiplier points in
           (kind, tag, Some points, lopt, body)::cookies
         else
        	 let _ = d_printf "assign_points_to_question_prompts: kind = %s is not scoroable\n" kind in
	         (kind, tag, Some Constants.zero_points, lopt, body)::cookies
	in
	List.map prompts ~f:assign


(* Tokenize title:
   Given Some "this is a title" it "sort of" returns 
   ["this-is-a-title", "this", "title", "is", "a"]
   except that it also removes stop words and short words.
 *)
let tokenize_title topt = 
  match topt with 
	| None -> [ ]
  | Some t ->
	    let tr = Words.tokenize_spaces_raw t in			
			match tr with
			| [ ] -> [ ] 
			| _ -> let l = String.concat ~sep:"-" tr in
             let _ = d_printf "add_label_of_title: label = %s\n" l in
				     let tf = Words.filter_tokens tr in
				     l::tf

let tokenize_title_body topt bopt = 
  let tt = tokenize_title topt in
	let tb = 
		match bopt with 
		| None -> [ ] 
		| Some s -> Words.tokenize_spaces s
	in
	(tt, tb)


(* Translate string to xml *)
let str_to_xml translator kind source =
	let source_xml = translator kind source in
    (source_xml, source)

(* Translate source string option to xml, return both *)
let str_opt_to_xml translator kind source_opt =
   match source_opt with 
   | None -> None 
   | Some source -> 
			 let source_xml = translator kind source  in
       Some (source_xml, source)

let depend_to_xml dopt = 
	match dopt with 
  |  None -> None
  |  Some ls -> Some (str_of_str_list ls)

let collect_labels (labels: (string list * string list) list): string list * string list = 
	let tt = List.map labels ~f:(fun (x, y) -> x) in
	let tb = List.map labels ~f:(fun (x, y) -> y) in
	let tt_merged = 
		match List.reduce tt (fun x y -> x @ y) with
		| None -> [ ]
		| Some ttm -> ttm
	in			
	let tb_merged =
		match List.reduce tb (fun x y -> x @ y) with
		| None -> [ ] 
		| Some tbm -> tbm
	in
	(tt_merged, tb_merged)

let collect_labels_from_atoms (labels: (string * string list * string list) list): string list * string list * string list = 
	let ls = List.map labels ~f:(fun (x, y, z) -> x) in
	let tt = List.map labels ~f:(fun (x, y, z) -> y) in
	let tb = List.map labels ~f:(fun (x, y, z) -> z) in
	let tt_merged = 
		match List.reduce tt (fun x y -> x @ y) with
		| None -> [ ]
		| Some ttm -> ttm
	in			
	let tb_merged =
		match List.reduce tb (fun x y -> x @ y) with
		| None -> [ ] 
		| Some tbm -> tbm
	in
	(ls, tt_merged, tb_merged)


(**********************************************************************
 ** END: Utilities
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
 *********************************************************************)

type t_label = Label of string
type t_depend = Depend of string list 
type t_point_val = float


module Cookie = 
struct
	type t = 
			{	mutable kind: string;
				mutable point_val: string option;
				mutable weight: string option;
				title: string option;
				tag: string option; 
				mutable label: string option; 
				depend: string list option;
				body: string;
				label_is_given: bool
			} 
  let kind cookie = cookie.kind
  let weight cookie = cookie.weight
  let point_val cookie = cookie.point_val
	let label cookie = cookie.label
	let depend cookie = cookie.depend
	let body cookie = cookie.body
	let label_is_given cookie = cookie.label_is_given

  let make   
			?tag: (tag = None) 
			?weight: (weight = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None) 
			kind
			body = 
    (* When created, the point value of a  cookie is always None. *)
		match label with 
		| None -> 
				{kind; tag; point_val=None; weight; title; label; depend; body; label_is_given=false}
		| Some _ -> 
				{kind; tag; point_val=None; weight; title; label; depend; body; label_is_given=true}

  (* Traverse cookie by applying f to its fields *) 
  let traverse cookie state f = 
		let {kind; point_val; title; label; body} = cookie in
		let _ = d_printf "Cookie.traverse: %s \n" kind in
(*
    let _ = d_printf_optstr "label " label in
*)
      f Ast_cookie state ~kind:(Some kind) ~point_val ~title:None ~label ~depend:None ~contents:(Some body)

  let to_tex cookie = 
		let {kind; tag; point_val; weight; title; label; body} = cookie in
    (* Use int value for point for idempotence in tex to tex translation *)
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let weight = normalize_point_val weight in *)
		let optarg = Tex.mk_tagged_point_val tag weight in
		let heading = Tex.mk_command kind optarg in
		let l = 
			if label_is_given cookie then	""
			else Tex.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body 


  let to_exam_tex cookie =
    let {kind; tag; point_val; weight; title; label; body} = cookie in
    (* Use int value for point for idempotence in tex to tex translation *)
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let weight = normalize_point_val weight in *)
		let optarg = Tex.mk_tagged_point_val tag weight in
		let heading = Tex.mk_command kind optarg in
		let l = 
			if label_is_given cookie then	""
			else Tex.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body


  let to_md cookie = 
		let {kind; point_val; weight; title; label; body} = cookie in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let weight = normalize_point_val weight in *)
		let weight = Md.mk_point_val weight in
		let heading = Md.mk_command kind weight in
		let l = 
			if label_is_given cookie then	""
			else Md.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body 


  (* If cookie doesn't have a label, then
	 * assign fresh label to prompt (unique wrt label_set).
   * Return nothing.
   * 
   * outer_label is unique for this prompt.
	*)
	let assign_label prefix label_set (outer_label, pos) cookie = 		
		let {kind; tag} = cookie in
		let (tt, tb) = tokenize_title_body cookie.title (Some (cookie.body)) in
    let t_all  = tt @ tb in
    let t_all = List.map t_all ~f:(Labels.nest_label_in outer_label) in

    (* Place  outer label extended with tag & pos.
     * This will be unique and the rest is probably not needed
     * but don't have time to think through it now.
     *)
    let outer_label = 
			match tag with 
			| None -> Labels.nest_label_in outer_label (string_of_int pos)
			| Some t -> Labels.nest_label_in outer_label (Labels.mk_label_from_tag t) 
		in

    let t_all  = [outer_label] @ t_all in
		let _ = 
			match cookie.label with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind cookie.kind in
					let l = Labels.mk_label_force label_set lk prefix (t_all) in
					cookie.label <- Some l
		| Some _ -> ()
		in
    	()

  let body_to_xml translator cookie =
		let _ = d_printf "cookie.body_to_xml: cookie = %s\n" cookie.kind in
    if Tex.is_cookie_algo_kind cookie.kind then
      translator Xml.code cookie.body
		else
			translator Xml.body cookie.body

  let propagate_point_value multiplier cookie = 
		let {kind; point_val; weight; title; label; body} = cookie in
		let _ = d_printf "Cookie.propagate_point_value %s\n" kind in
    match weight with
    | None -> 
			let err = "Fatal Error: Cookie.propagate_point_value: Expecting weight of zero None found" in
			let _ = printf "%s\n" err in
			raise (Constants.Fatal_Error err)
    | Some w -> 
	    let _ = cookie.point_val <- Some (multiply_points  w multiplier) in
			()

  let to_xml translator cookie = 
		let {kind; point_val; title; label; depend; body} = cookie in
		(* Translate body to xml *)
    let body_xml = body_to_xml translator cookie in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let point_val = normalize_point_val point_val in *)
    let depend = depend_to_xml depend in
    let titles = str_opt_to_xml translator Xml.title title in
		let r = 
			Xml.mk_cookie 
				~kind:kind 
        ~pval:point_val
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body_src:body
        ~body_xml:body_xml
   in
     r
end

type t_cookie = Cookie.t

module Prompt  = 
struct
	type t = 
			{	mutable kind: string;
				tag: string option; 
				mutable point_val: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				body: string;
				cookies: t_cookie list;
				label_is_given: bool
			} 
  let kind p = p.kind
  let point_val p = p.point_val
	let label p = p.label
	let depend p = p.depend
	let body p = p.body
	let label_is_given p = p.label_is_given

  let make   
			?tag: (tag = None) 
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None) 
			kind
			body
			cookies = 
		match label with 
		| None ->          
				{kind; tag; point_val; title; label; depend; body; cookies; label_is_given=false}
		| Some _ -> 
				{kind; tag; point_val; title; label; depend; body; cookies; label_is_given=true}

  (* Traverse prompt by applying f to its fields *) 
  let traverse prompt state f = 
		let {kind; point_val; title; label; body; cookies} = prompt in
		let _ = d_printf "Prompt.traverse: %s\n" kind in
(*
    let _ = d_printf_optstr "label " label in
*)
    let s = f Ast_prompt state ~kind:(Some kind) ~point_val ~title:None ~label ~depend:None ~contents:(Some body) in
		let f_tr_cookie state cookie = Cookie.traverse cookie state f in
		  List.fold_left cookies ~init:s ~f:f_tr_cookie 


  (* If this is a primary (question) prompt, then
   * return point value
   * otherwise point value is treated as zero.
   *)
  let get_primary_points prompt = 
		let {kind; point_val; title; label; body; cookies} = prompt in
    match point_val with
    | None -> 
			let err = sprintf "Fatal Error: Expecting point value of zero but None found, prompt = %s" kind in
			let _ = printf "%s\n" err in
			raise (Constants.Fatal_Error err)
    | Some p -> 
				if Tex.is_primary_question_prompt kind then
    			let _ = d_printf "Prompt.get_points %s, point_val=%s.\n" kind p in
	    		p
        else 
  		  	let _ = d_printf "Prompt.get_points %s, point_val=%s.\n" kind p in
          Constants.zero_points 

  let propagate_point_value multiplier prompt = 
		let {kind; point_val; title; label; body; cookies} = prompt in
    match point_val with
    | None -> 
				let err = sprintf "Fatal Error: Expecting point value None found, prompt = %s" kind in
  			let _ = printf "%s\n" err in
				raise (Constants.Fatal_Error err)
    | Some points -> 
        let points = multiply_points points multiplier in
				let _ = prompt.point_val <- Some points in
    		let _ = d_printf "Prompt.propagate_point_value kind = %s, point = %s\n" kind points in
        let _ = List.map cookies ~f:(Cookie.propagate_point_value points) in
				prompt.point_val

  let to_tex prompt = 
		let {kind; tag; point_val; title; label; body; cookies} = prompt in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let point_val = normalize_point_val point_val in *)
		let optarg = Tex.mk_tagged_point_val tag point_val in
		let heading = Tex.mk_command kind optarg in
		let cookies = map_concat_with newline Cookie.to_tex cookies in
		let l = 
			if label_is_given prompt then	""
			else Tex.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body ^ cookies


  let to_exam_tex prompt = 
		let {kind; tag; point_val; title; label; body; cookies} = prompt in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let point_val = normalize_point_val point_val in *)
		let optarg = Tex.mk_tagged_point_val tag point_val in
		let heading = Tex.mk_command kind optarg in
		let cookies = map_concat_with newline Cookie.to_exam_tex cookies in
		let l = 
			if label_is_given prompt then	""
			else Tex.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body ^ cookies


  let to_md prompt = 
		let {kind; point_val; title; label; body; cookies} = prompt in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
		(* let point_val = normalize_point_val point_val in *)
		let point_val = Md.mk_point_val point_val in
		let heading = Md.mk_command kind point_val in
		let cookies = map_concat_with newline Cookie.to_md cookies in
		let l = 
			if label_is_given prompt then	""
			else Md.mk_label label 

		in
		  heading ^ " " ^ l ^ 
		  body ^ cookies


  (* If prompt doesn't have a label, then
	 * assign fresh label to prompt (unique wrt label_set).
   * Return title and body tokens
	 * To assign label use words from title and body, but
   * nest them within the outer label provided (by the atom).
   * outer_label is unique for this prompt, within the atom.
	*)
	let assign_label prefix label_set (outer_label, pos) prompt = 		
    let {kind; tag} = prompt in
    let _ = d_printf "Prompt.label, is_given = %B\n" prompt.label_is_given in
		let (tt, tb) = tokenize_title_body prompt.title (Some (prompt.body)) in

    let t_all  = tt @ tb in
    let t_all = List.map t_all ~f:(Labels.nest_label_in outer_label) in

    (* Place  outer label extended with tag & pos.
     * This will be unique and the rest is probably not needed
     * but don't have time to think through it now.
     *)
    let outer_label = 
			match tag with 
			| None -> Labels.nest_label_in outer_label (string_of_int pos)
			| Some t -> Labels.nest_label_in outer_label (Labels.mk_label_from_tag t) 
		in
    let t_all  = [outer_label] @ t_all in

    (* Assign a label to prompt *)
		let l_raw = 
			match prompt.label with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind prompt.kind in
					let (l, l_raw) = Labels.mk_label_force_raw label_set lk prefix (t_all) in
					prompt.label <- Some l; l_raw
		| Some l -> l
		in
    (* Make a nesting label for each prompt, of the form atom-label::0, atom-label::1, ... *)
    let cookie_labels = List.init (List.length prompt.cookies) ~f:(fun i ->  (l_raw, i)) in
		let _ = List.map2  cookie_labels prompt.cookies ~f:(Cookie.assign_label prefix label_set) in
      (* Add atom label so that it can be used by the group. *)
		()

  (* tokenize the content	*)
	let tokenize prompt = 		
		let (tt, tb) = tokenize_title_body prompt.title (Some (prompt.body)) in
    	(tt, tb)

  let body_to_xml translator prompt =
		let _ = d_printf "prompt.body_to_xml: prompt = %s\n" prompt.kind in
    if Tex.is_secondary_choice_prompt prompt.kind then      
    	translator Xml.choice prompt.body
		else
      if Tex.is_prompt_refsol_fillin_ask prompt.kind then
        translator Xml.refsol_fillin_ask prompt.body
			else
  			translator Xml.body prompt.body

  (* TODO incorporate cookies. *)
  let to_xml translator prompt = 
		let {kind; point_val; title; label; depend; body; cookies} = prompt in
		(* Translate body to xml *)
    let body_xml = body_to_xml translator prompt in
    (* Update: do not normalize. 0.0 is important to keep. 
       because default is not always zero.  it depends on the cookie.
     *)
    (* let point_val = normalize_point_val point_val in *)
    let _ = d_printf "ast.prompt.to_xml: kind %s, point_val = %s\n" kind (str_of_pval_opt point_val) in
    let depend = depend_to_xml depend in
    let titles = str_opt_to_xml translator Xml.title title in
		let cookies = map_concat_with newline (Cookie.to_xml translator) cookies in
		let r = 
			Xml.mk_prompt 
				~kind:kind 
        ~pval:point_val
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body_src:body
        ~body_xml:body_xml
        ~cookies
   in
     r
end

type t_prompt = Prompt.t

(* An Atom.
 * An atom consist of usual fields, plus an optional problem.
 *)
module Atom  = 
struct
	type t = 
			{	mutable kind: string;
				mutable point_val: string option;
				mutable pl: string option;         (* programming language *)
				mutable pl_version: string option; (* programming language version *)
				mutable title: string option;
				mutable cover: string option;
				mutable sound: string option;
				mutable label: string option; 
				depend: string list option;
				body: string;
				caption: string option;
				prompts: t_prompt list;
				label_is_given: bool
			} 
  let kind a = a.kind
  let point_val a = a.point_val
  let pl a = a.pl
  let title a = a.title
	let label a = a.label
	let depend a = a.depend
	let body a = a.body
	let prompts a = a.prompts
	let label_is_given a = a.label_is_given


  let make   
			?pl: (pl = None) 
			?pl_version: (pl_version = None) 
			?point_val: (point_val = None) 
			?title: (title = None) 
			?cover: (cover = None) 
			?sound: (sound = None) 
			?label: (label = None) 
			?depend: (depend = None)
      ?caption: (caption = None)
			?prompts: (prompts = [])
			kind
			body = 
		match label with 
		| None -> 
				{kind; point_val; pl; pl_version; title; cover; sound; label; depend; caption; prompts; body=body; 
					label_is_given=false}
		| Some _ -> 
				{kind; point_val; pl; pl_version; title; cover; sound; label; depend; caption; prompts; body=body; 
					label_is_given=true}

  (* Traverse atom by applying f to its fields *) 
  let traverse atom state f = 
		let {kind; point_val; title; label; depend; prompts; body} = atom in
		let _ = d_printf "Atom.traverse: kind = %s \n" kind in
(*		let _ = d_printf "Atom.traverse: body = %s \n" body in *)
(*
    let _ = d_printf_optstr "label " label in
*)
    let s = f Ast_atom state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:(Some body) in    
		let f_tr_prompt state prompt = Prompt.traverse prompt state f in
		List.fold_left prompts ~init:s ~f:f_tr_prompt

  let to_tex atom = 
		let {kind; pl; pl_version; point_val; title; cover; sound; label; depend; caption; prompts; body} = atom in
		let _ = d_printf "Atom.to_tex kind = %s \n" kind in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
    let kw_args = ["version", pl_version; "cover", cover; "sound", sound] in
    let kw_args = Tex.mk_kw_args kw_args in
    let _ = d_printf "title = %s kw_args = %s\n" title kw_args in
    let caption = Tex.mk_caption caption in
		let prompts = map_concat_with newline Prompt.to_tex prompts in

		let h_begin = Tex.mk_begin_atom kind point_val title kw_args in
    let _ = d_printf "h_begin = %s\n" h_begin in
		let h_end = Tex.mk_end kind in
		let (l, l_figure) = 
			let label = Tex.mk_label label in
			if label_is_given atom then	("", label)
			else (label, label)

		in
		let d = Tex.mk_depend depend in		
      if kind = "figure" || kind = "table" then
        (* Always include label in figure or table *)
				h_begin ^
				d ^ 
				body ^ newline ^ caption ^ l_figure ^ newline ^
				h_end		
			else 
				h_begin ^
				l ^ 
				d ^ 
				body ^ newline ^ prompts ^ newline ^
				h_end  


  let to_exam_tex atom = 
		let {kind; pl; pl_version; point_val; title; cover; sound; label; depend; caption; prompts; body} = atom in
		let _ = d_printf "Atom.to_exam_tex kind = %s \n" kind in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
    let kw_args = ["version", pl_version; "cover", cover; "sound", sound] in
    let kw_args = Tex.mk_kw_args kw_args in
    let _ = d_printf "title = %s kw_args = %s\n" title kw_args in
    let caption = Tex.mk_caption caption in
		let prompts = map_concat_with newline Prompt.to_exam_tex prompts in

		let h_begin = Tex.mk_begin_atom kind point_val title kw_args in
    let _ = d_printf "h_begin = %s\n" h_begin in
		let h_end = Tex.mk_end kind in
		let (l, l_figure) = 
			let label = Tex.mk_label label in
			if label_is_given atom then	("", label)
			else (label, label)

		in
		let d = Tex.mk_depend depend in		
      if kind = "figure" || kind = "table" then
        (* Always include label in figure or table *)
				h_begin ^
				d ^ 
				body ^ newline ^ caption ^ l_figure ^ newline ^
				h_end		
			else 
				h_begin ^
				l ^ 
				d ^ 
				body ^ newline ^ prompts ^ newline ^
				h_end


  let to_md atom = 
		let {kind; point_val; title; label; depend; prompts; body} = atom in
		let point_val = normalize_point_val point_val in
		let point_val = Md.mk_point_val point_val in
		let title = Md.mk_title title in
		let prompts = map_concat_with newline Prompt.to_md prompts in
		let h_begin = Md.mk_begin kind point_val title in
		let h_end = Md.mk_end kind in
		let l = 
			if label_is_given atom then	""
			else Md.mk_label label 

		in
		let d = Md.mk_depend depend in		
    let a = 
		  h_begin ^
		  l ^ 
		  d ^ 
		  body ^ newline ^ prompts ^ newline ^
      h_end		
    in
      (String.strip ~drop:is_vert_space a) ^ newline


  (* If atom doesn't have a label, then
	 * assign fresh label to atom (unique wrt label_set).
   * Return title and body tokens.
	 * To assign label use words from title and body.  
   * 
   * Because problem atoms may have a lot of their content in 
   * their prompts, first tokenize the prompts and  use 
   * these.  
   * 
	*)
	let assign_label prefix label_set atom = 		
    let _ = d_printf "Atom.assign_label\n" in 
		let t_p = List.map atom.prompts ~f:(Prompt.tokenize) in
		let (tt_p, tb_p) = collect_labels t_p in
		let (tt, tb) = tokenize_title_body (title atom) (Some (body atom)) in
		let (tt_all, tb_all) = (tt @ tt_p, tb @ tb_p) in
		let l_raw = 
			match (label atom) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind atom) in
					let (l, l_raw) = Labels.mk_label_force_raw label_set lk prefix (tt_all @ tb_all) in
					atom.label <- Some l; l_raw
		| Some l -> l
		in
    let atom_label = 
			match label atom with 
			| None -> 
					let err = "Fatal Error: ast.ml: expecting atom label to exist.\n" in
					let _ = printf "%s" err in
						raise (Constants.Fatal_Error err);
			| Some l -> l
		in 
    (* Make a nesting label for each prompt, of the form atom-label::0, atom-label::1, ... *)
    let prompt_labels = List.init (List.length atom.prompts) ~f:(fun i ->  (l_raw, i)) in
		let _ = List.map2  prompt_labels atom.prompts ~f:(Prompt.assign_label prefix label_set) in
      (* Add atom label so that it can be used by the group. *)
    	(atom_label, tt_all, tb_all)


  let body_to_xml translator atom =
		let _ = d_printf "body_to_xml: atom = %s, Not promoting\n" atom.kind in
    let body = 
      if Tex.is_code atom.kind then
        match atom.pl with 
        | None -> fmt_lstlisting_nopl atom.body
        | Some pl -> fmt_lstlisting_pl (Option.value atom.pl  ~default:"c") atom.body  
      else
        atom.body
    in
		let _ = d_printf "atom body = %s\n" body in
		let (body, languages) = sanitize_lst_language body in
(*			let _ = d_printf "languages = %s\n" (str_of_str2_list languages) in *)
    let _ = d_printf "body sanitized:\n %s" body in
		translator Xml.body body

  let propagate_point_value atom = 
		let {kind; point_val; title; label; depend; prompts; body} = atom in
		let _ = d_printf "Atom.propagate_point_value: kind = %s title = %s\n" kind (str_of_str_opt title) in
    let prompt_points = List.map prompts ~f:Prompt.get_primary_points in
    let sum_opt = List.reduce prompt_points ~f:add_points in
    let _ = d_printf "Atom.propagate_point_value: sum over prompts = %s \n" (str_of_str_opt sum_opt) in
      match point_val with 
			| None -> 
        let _ = atom.point_val <- sum_opt in 
				force_float_string (sum_opt)
			| Some points ->
        match sum_opt with 
				| None -> 
						let _ = atom.point_val <- None in
						force_float_string atom.point_val
        | Some sum ->
            (* Divide sum as  a factor *)
						let multiplier = divide_points points sum in
						let _ = List.map prompts ~f:(Prompt.propagate_point_value multiplier) in
						points

  let to_xml translator atom = 
		(* Translate body to xml *)
		let body_xml = body_to_xml translator atom in
		(* Atom has changed, reload *)
		let {kind; point_val; pl; pl_version; title; cover; sound; label; depend; prompts; body; caption} = atom in
    let depend = depend_to_xml depend in
		let point_val = normalize_point_val point_val in
		let _ = d_printf "Atom.to_xml: point_val = %s" (str_of_str_opt point_val) in
    let titles = str_opt_to_xml translator Xml.title title in
		let prompts = map_concat_with newline (Prompt.to_xml translator) prompts in
    let captions = str_opt_to_xml translator Xml.caption caption in
		let r = 
			Xml.mk_atom 
				~kind:kind 
        ~pl:pl
        ~pl_version:pl_version
        ~pval:point_val
        ~topt:titles
        ~copt:cover
        ~sopt:sound
        ~lopt:label
				~dopt:depend 
        ~body_src:body
        ~body_xml:body_xml
        ~capopt:captions
        ~prompts
   in
     r
end

type atom = Atom.t

(* A group.
 * A group consist of usual fields plus a list of atoms.
 * Other than atoms, groups don't have their own bodies.
 *)
module Group =
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
				strategy: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				atoms: atom list
			} 

  let kind g = g.kind
  let point_val g = g.point_val
  let strategy g = g.strategy
  let title g = g.title
	let label g = g.label
	let depend g = g.depend
	let atoms g = g.atoms

	let make  
			?kind: (kind = Tex.kw_cluster) 
			?point_val: (point_val = None) 
			?strategy: (strategy = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None)
			atoms = 
				{kind; point_val; strategy; title; label; depend; atoms=atoms}

  (* Traverse (pre-order) group by applying f to its fields *) 
  let traverse group state f = 
		let {kind; point_val; strategy; title; label; depend; atoms} = group in
		let s = f Ast_group state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:None in

		let atom_tr_f state atom = Atom.traverse atom state f in
		let _ = d_printf "Group.traverse: %s \n" kind in
(*
    let _ = d_printf_optstr "label " label in
*)
		  List.fold_left atoms ~init:s ~f:atom_tr_f

  let to_tex group = 
		let {kind; point_val; strategy; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
    let descriptor = Tex.mk_segment_descriptor point_val strategy in
		let title = Tex.mk_title title in
		let h_begin = Tex.mk_begin kind descriptor title in
		let h_end = Tex.mk_end kind in
		let l = Tex.mk_label label in
		let d = Tex.mk_depend depend in
		let atoms = map_concat_with newline Atom.to_tex atoms in
		  h_begin ^ 
		  l ^ 
		  d ^ 
		  atoms ^ h_end		


  let to_exam_tex group = 
		let {kind; point_val; strategy; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
    let descriptor = Tex.mk_segment_descriptor point_val strategy in
		let title = Tex.mk_title title in
		let h_begin = Tex.mk_begin kind descriptor title in
		let h_end = Tex.mk_end kind in
		let l = Tex.mk_label label in
		let d = Tex.mk_depend depend in
		let atoms = map_concat_with newline Atom.to_exam_tex atoms in
		  h_begin ^ 
		  l ^ 
		  d ^ 
		  atoms ^ h_end		


  let to_md group = 
		let {kind; point_val; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
		let point_val = Md.mk_point_val point_val in
		let title = Md.mk_title title in
		let h_begin = Md.mk_begin kind point_val title in
		let h_end = Md.mk_end kind in
		let l = Md.mk_label label in
		let d = Md.mk_depend depend in
		let atoms = map_concat_with newline Atom.to_md atoms in
		  h_begin ^ 
		  l ^ 
		  d ^ 
		  atoms ^ h_end		


  (* If group doesn't have a label, then
	 * assign fresh label to each atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label first use the atom labels nested within.
   * If that doesn't work then use words from title and body.  
   * Example: if atom has label grm:chapter_label::atom_label
   *          then group could have label grp:grm:chapter_label::atom_label.
   *          This will be priority to ensure stability.
   *)
	let assign_label prefix label_set group = 		
		let t_a = List.map group.atoms ~f:(Atom.assign_label prefix label_set) in
    let (atom_labels, tt_a, tb_a) = collect_labels_from_atoms t_a in
		let tt_g = tokenize_title (title group) in
    let (tt_all, tb_all) = (tt_g @ tt_a, tb_a) in
		let _ = 
			match (label group) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind group) in
          let lopt = 
						match atom_labels with 
						| [ ] -> None
						| _ -> Labels.mk_label label_set lk None atom_labels
					in
					let l = 
						match lopt with 
						| None -> Labels.mk_label_force label_set lk prefix (tt_all @ tb_all) 
						| Some l -> l
					in
	  				group.label <- Some l
		| Some _ -> ()
		in
		(tt_all, tb_all)

  let propagate_point_value group = 
		let {kind; point_val; strategy; title; label; depend; atoms} = group in
		let _ = d_printf "Group.propagate_point_value: kind = %s title = %s\n" kind (str_of_str_opt title) in
		let points = List.map atoms ~f:Atom.propagate_point_value in
    let sum = List.reduce points ~f:add_points in
    let _ = group.point_val <- sum in
		force_float_string sum

  let to_xml translator group = 
		let {kind; point_val; strategy; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
    let titles = str_opt_to_xml translator Xml.title title in
    let depend = depend_to_xml depend in
		let atoms = map_concat_with newline (Atom.to_xml translator) atoms in

    (* We will use the kind of the group as a segment name.
       Because these are unique this creates no ambiguity.
       We know which segments are groups.
     *)

		let r = 
			Xml.mk_segment 
				~kind:kind 
        ~pval:point_val
        ~strategy:strategy
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body:atoms
   in
     r

end

type group = Group.t

module Element = 
struct
	type t = 
		| Element_group of group
		| Element_atom of atom

	let mk_from_group g = 
		Element_group g

	let mk_from_atom a = 
		Element_atom a


  (* Traverse element by applying f *) 
  let traverse e state f = 
		match e with
		| Element_atom a ->
				Atom.traverse a state f
		| Element_group g ->
				Group.traverse g state f

	let to_tex e = 
		match e with
		| Element_atom a ->
				Atom.to_tex a
		| Element_group g ->
				Group.to_tex g


	let to_exam_tex e = 
		match e with
		| Element_atom a ->
				Atom.to_exam_tex a
		| Element_group g ->
				Group.to_exam_tex g


	let to_md e = 
		match e with
		| Element_atom a ->
				Atom.to_md a
		| Element_group g ->
				Group.to_md g


  (* If block doesn't have a label, then
	 * assign fresh label the block (unique wrt label_set).
	 * To assign label use words from title and body.
	 * Return the label assigned.
	*)
	let assign_label prefix label_set e =
 		match e with
		| Element_atom a ->
				let (ls, lt, lb) = Atom.assign_label prefix label_set a in
				(lt, lb)
		| Element_group g ->
				Group.assign_label prefix label_set g

  let normalize e = 
 		match e with
		| Element_atom a ->
				(* Insert an empty group *)
				let g = Group.make ~kind:Tex.kw_cluster [a] in
				Element_group g				
		| Element_group g ->
				Element_group g

  let propagate_point_value element = 
 		match element with
		| Element_atom a -> 
				Atom.propagate_point_value a 
		| Element_group g -> 
				Group.propagate_point_value g 
					
	let to_xml translator e = 
		match e with
		| Element_atom a ->
				Atom.to_xml translator a
		| Element_group g ->
				Group.to_xml translator g

end

type element = Element.t

module Block = 
struct
	type t = 
			{	
				mutable point_val: string option;
				mutable label: string option; 
				mutable elements: element list
			} 
			
  let point_val b = b.point_val
	let label b = b.label
	let elements b = b.elements
  let make 
			?point_val:(point_val = None)  
			?label:(label = None)  
			es = 
		{point_val; label; elements = es}

  (* Traverse (pre-order) block by applying f to its fields *) 
  let traverse block state f = 
		let {point_val; label; elements} = block in
		let s = f Ast_block state ~kind:None ~point_val ~title:None ~label ~depend:None ~contents:None in

		let element_tr_f state e = Element.traverse e state f
		in 	
		  List.fold_left elements ~init:state ~f:element_tr_f

	let to_tex b = 
		map_concat_with "\n" Element.to_tex (elements b)

	let to_exam_tex b = 
		map_concat_with "\n" Element.to_exam_tex (elements b)

	let to_md b = 
		map_concat_with "\n" Element.to_md (elements b)

  (* We don't assign labels to blocks, but 
	 * tokenize the contents.
	*)
	let assign_label prefix label_set block = 		
		let t_a = List.map (elements block) ~f:(Element.assign_label prefix label_set)  in
		let (tt_a, tb_a) = collect_labels t_a in
		  tt_a @ tb_a  

  let rec normalize block = 
		let {point_val; label; elements} = block in
		let elements = List.map elements ~f:Element.normalize in
		  block.elements <- elements

  let propagate_point_value block = 
		let {point_val; label; elements} = block in
		let _ = d_printf "Block.propagate_point_value: label = %s\n" (str_of_str_opt label) in
		let points_elements = List.map elements ~f:Element.propagate_point_value in
    let points_sum = List.reduce (points_elements) ~f:add_points in
    let _ = block.point_val <- points_sum in
		force_float_string points_sum

  let to_xml translator block = 
		let {point_val; label; elements} = block in
		let elements = map_concat_with newline (Element.to_xml translator) elements in
		elements


end

type block = Block.t

module Segment =
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
				strategy: string option;
 				title: string;
				mutable label: string option; 
				depend: string list option;
				block: block;
				mutable subsegments: t list
			} 
  let kind s = s.kind
  let point_val s = s.point_val
  let strategy s = s.strategy
  let title s = s.title
	let label s = s.label
	let depend g = g.depend
	let block s = s.block
	let subsegments s = s.subsegments

	let make  
			?kind: (kind = Tex.kw_gram) 
			?point_val: (point_val = None) 
			?strategy: (strategy = None) 
			?label: (label = None) 
			?depend: (depend = None)
			title
			block
			subsegments = 
		let label = 
			match label with 
			| None ->
					begin
						match Block.label block with 
						| None -> None
						| Some l -> 
								(* Steal blocks label.
								 * Make sure it is set to None. 
								 * Important because the block's label, if any, belongs to
								 * the chapter.
								*)
								let _ = block.Block.label <- None in
								Some l
					end
			| Some l -> Some l
		in
		{kind; point_val; strategy; title; label; depend; 
		 block = block; subsegments = subsegments}


  (* Traverse (pre-order) group by applying f to its fields *) 
  let rec traverse segment state f = 

		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
		let _ = d_printf "Segment.traverse: %s title = %s\n" kind title in
(*
    let _ = d_printf_optstr "label " label in
*)
		let s = f Ast_segment state ~kind:(Some kind) ~point_val ~title:(Some title) ~label ~depend ~contents:None in

		let block_tr_f state block = Block.traverse block state f in
		let subsegment_tr_f state subsegment = traverse subsegment state f in

		let state_b = Block.traverse block state f in
		let state_s = List.fold_left subsegments ~init:state_b ~f:subsegment_tr_f in
		state_s


  let rec propagate_point_value segment : string = 
		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
		let _ = d_printf "Segment.propagate_point_value %s title = %s\n" kind title in
(*
    let _ = d_printf_optstr "label " label in
*)
		let points_block = Block.propagate_point_value block in
		let points_subsegments = List.map subsegments ~f:propagate_point_value in
    let points_sum_opt = List.reduce (points_block::points_subsegments) ~f:add_points in
    let _ = segment.point_val <- points_sum_opt in
		force_float_string points_sum_opt


  (* Convert to string with levels.
   * Used for debugging only.
   *)
  let rec to_tex_level level segment = 		
		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
    let descriptor = Tex.mk_segment_descriptor point_val strategy in
		let level_str = string_of_int level in
		let h_begin = Tex.mk_segment_header (level_str ^ kind) descriptor title in
		let h_end = Tex.mk_end kind in
		let l_opt = Tex.mk_label label in
		let d_opt = Tex.mk_depend depend in
		let block = Block.to_tex block in
		let subsegments = map_concat_with "\n" (to_tex_level (level + 1)) subsegments in
		  h_begin ^ 
		  l_opt ^ 
		  d_opt ^ 
		  block ^ newline ^ 
      subsegments

  let rec to_tex segment = 
		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
    let descriptor = Tex.mk_segment_descriptor point_val strategy in
		let h_begin = Tex.mk_segment_header kind descriptor title in
		let h_end = Tex.mk_end kind in
		let l_opt = Tex.mk_label label in
		let d_opt = Tex.mk_depend depend in
		let block = Block.to_tex block in
		let subsegments = map_concat_with "\n" to_tex subsegments in
		  h_begin ^ 
		  l_opt ^ 
		  d_opt ^ 
		  block ^ newline ^ 
      subsegments

  let rec to_exam_tex segment = 
		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
    let descriptor = Tex.mk_segment_descriptor point_val strategy in
		let h_begin = Tex.mk_segment_header kind descriptor title in
		let h_end = Tex.mk_end kind in
		let l_opt = Tex.mk_label label in
		let d_opt = Tex.mk_depend depend in
		let block = Block.to_exam_tex block in
		let subsegments = map_concat_with "\n" to_exam_tex subsegments in
		  h_begin ^ 
		  l_opt ^ 
		  d_opt ^ 
		  block ^ newline ^ 
      subsegments


  let rec to_md segment = 
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
		let point_val = Md.mk_point_val point_val in
		let h_begin = Md.mk_segment_header kind point_val title in
		let h_end = Md.mk_end kind in
		let l_opt = Md.mk_label label in
		let d_opt = Md.mk_depend depend in
		let block = Block.to_md block in
		let subsegments = map_concat_with "\n" to_md subsegments in
		  h_begin ^ 
		  l_opt ^ 
		  d_opt ^ 
		  block ^ newline ^ 
      subsegments

(*
  let rec to_tex segment =
		to_tex_level 0 segment
*)

  (* Given a flat list of segments with a unique top level segment
	 * nest all segments, and return the top level segment.
 	 *)
  let rec nest_segments (segments: t List.t): t option = 

    (* Assuming segments are properly nested, 
		 * take segments that are nested in home.
		 * Properly nested means that the algorithm can stop
		 * when it encounters the first segment that is not 
		 * nested within it.
		 *)
		let rec take_nesteds home segments = 
	 begin
			match segments with 
			| [ ] -> (home, [ ])
			| h::t ->
					if Tex.segment_is_nested (kind h) (kind home) then
						let s = home.subsegments in
						let _ = home.subsegments <- s @ [h] in
						  take_nesteds home t 
					else
						(home, segments)
	 end
		in
    (* Nest the tail.
		 * Take the head and nest the tail with head
     *)
		let rec nest segments = 
			match segments with 
			| [ ] -> [ ]
			| h::t ->				
					let tt = nest t in 
					let (hh, ttt) = take_nesteds h tt in
					hh::ttt
		in
		match nest segments with 
		| [ ] -> None
		| h::[ ] -> Some h
		| _ -> None
				

  (* If segment doesn't have a label, then
	 * assign fresh label to segment atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label use words from title and block.
	*)
	let rec assign_label prefix label_set segment = 		
		let t_b = Block.assign_label prefix label_set (block segment) in
		let _ = List.map (subsegments segment) ~f:(assign_label prefix label_set) in
	  match (label segment) with 
		| Some _ -> ()
		| None ->
  	  let lk = Tex_syntax.mk_label_prefix_from_kind (kind segment) in
			let tt_s = tokenize_title (Some (title segment)) in
			match Labels.mk_label label_set lk (Some prefix) tt_s with
			| None -> 
	      let tokens = tt_s @ t_b in
        let l = Labels.mk_label_force label_set lk prefix tokens in
      	segment.label <- Some l
    	| Some l -> segment.label <- Some l

  let assign_label_force segment label_set kind = 
		let tokens = tokenize_title (Some (title segment)) in			
    (* label kind *)
    let lk = Tex.mk_label_prefix_from_kind kind in
		let l = Labels.mk_label_force label_set lk "" tokens in
    let _ = d_printf "Segment.mk_label_force label = %s\n" l in
    let _ = segment.label <- Some l in
    l

  let rec normalize segment = 
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let _ = Block.normalize block in
		let _ = List.map subsegments ~f:normalize in
		()

  let rec to_xml translator segment = 
		let {kind; point_val; strategy; title; label; depend; block; subsegments} = segment in
    let _ = d_printf "ast.segment.to_xml: title = %s\n" title in
		let point_val = normalize_point_val point_val in
    let _ = d_printf "ast.segment.to_xml: point_val = %s\n" (str_of_pval_opt point_val) in
    let titles = str_opt_to_xml translator Xml.title (Some title) in
    let depend = depend_to_xml depend in
		let block =  Block.to_xml translator block in
		let subsegments = map_concat_with newline (to_xml translator) subsegments in
		let body = block ^ newline ^ subsegments in
		let r = 
			Xml.mk_segment
				~kind:kind 
        ~pval:point_val
        ~strategy:strategy
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body:body
   in
     r
end
type segment = Segment.t

type ast = segment 


(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)

let map f xs = 
  List.map xs f

(* Check that the nesting structure of the ast is correct *)
let is_wellformed ast = 
  let s = ast in
	let wf = 
		map_reduce 
			(fun ss -> Tex.segment_is_nested (Segment.kind ss) (Segment.kind s)) 
			(fun x y -> x || y)
			(Segment.subsegments s) 
	in
	match wf with 
	|	None -> true
	| Some flag -> flag

(**********************************************************************
 ** END Utilities
 *********************************************************************)

(* Check_preamble:
   Check that
	 1) there exists at most one preamble atom,
	 2) if it exists, preamble is the first atom.
 *)
let check_preamble ast: bool =
  (* Traversal visit function to check atom kind.
     no: number of atoms preceding this atom.
		 found: indicates the number of preambles found.
		 no: number of atoms.  it is not used.
 	*)
	let check 
			member_kind
			(no, found)
			~(kind: string option)
			~(point_val: string option) 
			~(title: string option) 
			~(label: string option) 
			~(depend: (string list) option) 
			~(contents: string option) = 
		begin
    match member_kind with 
		| Ast_atom ->
				let Some (kind) = kind in
				if kind = Xml.preamble then
					let _ = d_printf "Preamble found: pos = %d, found = %d\n" no found in
					(no+1, found + 1)
				else
					(no+1, found)
		| _ -> (no, found)
		end
	in
	let (_, found) = Segment.traverse ast (0, 0) check in	
	if found = 0  then
		(d_printf ("Preamble check: no preamble found.\n");
		 true)
  else if found = 1 then
		(d_printf ("Preamble check:  one preamble found.\n");
		 true)
  else if found > 1 then
		(d_printf ("Preamble check: error. Multiple preambles found.\n");
		 false)
	else
		(printf ("Preamble check:  error. Out of range.\n");
		 false)


(* Collect all the labels in the ast
 * in a label set and return it.
 *)
let collect_labels ast: Labels.t =
	let label_set = Labels.empty () in

  (* Traversal visit function to add the label
	 * of an AST member to label set 
 	*)
	let add_label 
			member_kind
			label_set 
			~(kind: string option)
			~(point_val: string option) 
			~(title: string option) 
			~(label: string option) 
			~(depend: (string list) option) 
			~(contents: string option) = 
    match label with 
		| None -> 
				label_set
		| Some (s: string) -> 
				let _ = Labels.add label_set s in
				label_set
	in
	let label_list = Labels.to_list label_set in
  (*
  let _ = d_printf_strlist "Initial labels: " label_list in
  *)
	let label_set = Segment.traverse ast label_set add_label in
	let label_list = Labels.to_list label_set in
  (*
  let _ = d_printf_strlist "All labels: " label_list in
	*)
	  label_set

(* Assign labels to all members of the AST. *)
let assign_labels ast = 
	let label_set = collect_labels ast in  

  (* Make sure that the chapter has a label *)
  let chlabel =
  	match (Segment.label ast) with 
		| None -> 
				let l = Segment.assign_label_force ast label_set Tex.kw_chapter in
        let _ = d_printf "chapter/None label = %s\n" l in
				Some l
		| Some l ->
        let _ = d_printf "chapter/Some label = %s\n" l in
				Some l 
	in
	match chlabel with 
	| None -> (printf "ast.assign_labels: Fatal Error. Chapter found without label" ; exit 1)
	| Some chl -> 
			let prefix = Labels.drop_label_prefix chl in
      let _ = d_printf "chapter prefix label = %s" prefix in
      Segment.assign_label prefix label_set ast 

(* Put the AST into normal form.
 * If an atom does not have an enclosing group/cluster, 
 * then insert one.
 *)
let normalize ast = 
	Segment.normalize ast

let to_md ast = 
	Segment.to_md ast

let to_tex ast = 
	Segment.to_tex ast

let to_exam_tex ast = 
	Segment.to_exam_tex ast

let to_xml atom_translator ast = 
	let xml:string = Segment.to_xml atom_translator ast in
	Xml.mk_standalone xml 

(* Ast validation *)
let validate ast = 
  let passed = check_preamble ast in
	if passed then
		( (* printf "ast.ast: Ast validation passed.\n"; *)
		 ast)
	else
		(printf "ast.ast: Fatal Error: Ast validation failed. Terminating.\n";
		 exit 1)
 
let propagate_point_values ast = 
  Segment.propagate_point_value ast

(* Create a problem from items *)

(* Create a cookie from an item *)
let cookie_of_item (item: t_item): t_cookie = 
	let (kind, tag, point_val, label, body) = item in
  (* For cookies, the point val of an item is a weight *)
  let weight = point_val in
  let _ = d_printf "cookie_of_item: kind %s, point_val %s, label = %s body %s\n" kind (str_of_pval_opt point_val) (str_of_str_opt label) body in
	if Tex.is_cookie kind then  
    match weight with 
    | None ->
      let weight = Some (Tex.get_cookie_weight kind) in
  		Cookie.make ~tag ~weight ~label kind body 
    | Some _ ->
  		Cookie.make ~tag ~weight ~label kind body 
	else
		(printf "Parse Error"; exit 1)

(* Create a prompt from an item list that starts with a prompt and 
 * continues with the cookies of that prompt.
 *)
let prompt_of_items (items: t_item list): t_prompt = 
	match items with 
		[ ] -> (printf "Fatal Internal Error"; exit 1)
	| item::rest_items ->
			let  (kind, tag, point_val, label, body) = item in
      let _ = d_printf "prompt_of_items: kind = %s point_val = %s label = %s.\n" kind (str_of_pval_opt point_val) (str_of_str_opt label) in
			if Tex.is_prompt kind then
        let _ = d_printf "prompt_of_items: kind = %s is prompt.\n" kind in
				let cookies = List.map rest_items ~f:cookie_of_item in
				Prompt.make ~tag ~point_val ~label kind body cookies 
  		else
        (* item is a field for the current prompt *)
  			(printf "Parse Error: I was expecting a prompt here but saw kind = %s." kind;
	  		 exit 1)

(* Algorithm
 * - Take prompts and separate them into questions.
 *   Each question has the form [primary_prompt, secondary_prompt, secondary_prompt ...]
 *
 * - Assign each question a point value.
 *)
let assign_points_to_prompts prompts = 
  let _ =  d_printf "Start: assign_points_to_prompts\n" in

  let rec take_secondary_prompts prompts = 
    match prompts with 
		| [ ] -> ([ ], prompts)
		| h::t -> 
      (* Find the head item *) 
	    let head_item = List.nth_exn h 0 in
      let (kind, tag, pval, label, body) = head_item in 
      let _ =  d_printf "take_next_question: kind = %s pval = %s label = %s\n" kind (str_of_pval_opt pval) (str_of_str_opt label) in 
			if Tex.is_primary_question_prompt kind then
        (* head item is primary, so start a new question *)
  			([ ], prompts)
	  	else            	      
		    let (q, t_prompts) = take_secondary_prompts t in
     	 (h::q, t_prompts)
	 in
   let rec take_next_question prompts = 
     match prompts with 
  	 | [ ] -> ([ ], prompts)
  	 | h::t -> 
        (* Find the head item *) 
				let head_item = List.nth_exn h 0 in
        let (kind, tag, pval, label, body) = head_item in 
        let _ =  d_printf "take_next_question: kind = %s pval = %s label = %s\n" kind (str_of_pval_opt pval) (str_of_str_opt label) in 
				if Tex.is_primary_question_prompt kind then
          (* head item is primary, so start a new question *)
	        let (rest, t_prompts)  = take_secondary_prompts t in
					(h::rest, t_prompts)
				else            	      
				let err = "Fatal Error: Expecting a primary prompt None found" in
				let _ = printf "%s\n" err in
				raise (Constants.Fatal_Error err)
  in
  let rec take_all_questions prompts = 
    match prompts with 
		| [ ] -> [ ]
		| _ ->
				let (q, t_prompts) = take_next_question prompts in
				let questions = take_all_questions t_prompts in
				q::questions
  in
  let assign_points_to_question question = 
    let _ = d_printf ("assign_points_to_question\n") in
    match question with 
	  | [ ] ->
				let err = "Fatal Error: Expecting a primary prompt but None found!" in
				let _ = printf "%s\n" err in
				raise (Constants.Fatal_Error err)
	 | head_prompt::prompts ->
     let head_item::t_head_prompt = head_prompt in
     let (kind, tag, pval, label, body) = head_item in      	 
     let n_factors = 
			 try sum_factors kind prompts with
				 Constants.Syntax_Error s -> 
					 let err = sprintf "\n%s\nContext: Question prompt:\n>%s>\n" s body in
           let _ = printf "%s\n" err in
					 raise (Constants.Syntax_Error "Syntax Error")					 
		 in
     (* Take the point value of the question prompt (head item) *)
     let points = 
       match pval with
    	 | None -> Constants.default_points_per_question
  	   | Some p -> p 
     in

     (* Check that at least one solution was specified
      * Otherwise raise error unless points is zero.
      *)
     let points_per_factor = 
        if (float_of_string n_factors) > 0.0 then
					divide_points points n_factors
        else if (float_of_string points) > 0.0 then          
  				let err = sprintf"Fatal Error: A question must have at least on solution or correct-choice prompt!  I have found none.\n  Total factors = %s \n Context: %s\n" n_factors body in 
	   			let _ = printf "%s\n" err in
  		  		raise (Constants.Fatal_Error err)
        else 
          (* both points and factors are zero *)
          "0.0"
		 in
     let _ = d_printf  "assign_points_to_question: points_per_factor: %s, n_factors: %s\n" points_per_factor n_factors in
     (* Update question prompt point value *)
     let head_prompt = (kind, tag, Some points, label, body)::t_head_prompt in

     (* Scale prompts now. *)
     let prompts = assign_points_to_question_prompts points_per_factor prompts in 
 	   let question = head_prompt::prompts in
	   question
  in
	let _ = d_printf ("Taking all questions") in 
	let questions = take_all_questions prompts in
	let questions = List.map questions ~f:assign_points_to_question in
  List.concat questions	
  
(* Create a prompt list from an item list.
 * The item list must start with the problem.
 *)
let prompts_of_items (items: t_item list) =
  (* Given a pair `current` of prompt and prompts, and an item, 
	 * this function looks at the item.
   * If the item's kind is a prompt kind, then it starts a new prompt 
   * and pushes the current prompt on to the prompts.
   * Otherwise, item is a cookie and it pushes the item into the current prompt.
   *
   * current prompt is a list of items.
   * current prompts is a list of prompts.
   * example:
   *     prompts = [ [prompt0, cookie00, cookie01], [prompt1, cookie10, cookie11]]
   *     current prompt = [prompt2, cookie20, cookie21],
   *     where prompt and cookie are both items.         
   *)
  let collect (current: (t_item list) * ((t_item list) list)) (item: t_item) = 
		let (cp, prompts) = current in
		let  (kind, tag, point_val, label, body) = item in
		let _ = d_printf "ast.collect: kind = %s\n" kind in

		if Tex.is_prompt kind then
      (* item is a prompt *)
			let prompt = [item] in
			(prompt, prompts @ [cp])
		else if Tex.is_cookie kind then
      (* item is a cookie for the current prompt *)
			let cookie = [item] in
        (cp @ cookie, prompts)
		else
			let _ = printf "Parse Error: I was expecting a 'prompt' or a 'cookie' but saw kind = %s.\n" kind in
			exit 1
	in
	begin
		match items with 
		| [ ] -> [ ]
		| item::items_rest ->
				let (kind, tag, point_val, label, body) = item in
    		let _ = d_printf "ast.prompt_of_items: top kind = %s\n" kind in
				if Tex.is_primary_question_prompt kind then
					let prompt = [item] in
					let (prompt, prompts) = List.fold items_rest ~init:(prompt, []) ~f:collect in
					let prompts = prompts @ [prompt] in         
					let prompts = assign_points_to_prompts prompts in
          let result = List.map prompts ~f:prompt_of_items in
          let _ = d_printf "prompts_of_items: made %d prompts" (List.length result) in
					result
				else if Tex.is_primary_algo_prompt kind then
					let prompt = [item] in
					let (prompt, prompts) = List.fold items_rest ~init:(prompt, []) ~f:collect in
					let prompts = prompts @ [prompt] in         
(*					let prompts = assign_points_to_prompts prompts in *)
          let result = List.map prompts ~f:prompt_of_items in
          let _ = d_printf "prompts_of_items: made %d prompts" (List.length result) in
					result
				else
					(printf "Parse Error: I was expecting a primary prompt here.\n";
					 exit 1)
	end
