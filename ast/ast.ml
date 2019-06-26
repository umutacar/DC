open Core
open Utils

(* Turn off all prints *)
let d_printf args = 
    ifprintf stdout args


module Labels = Tex_labels
module Tex = Tex_syntax
module Words = English_words
module Xml = Xml_syntax

type ast_member = Ast_prompt | Ast_problem | Ast_atom | Ast_group | Ast_element | Ast_block | Ast_segment

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)
let newline = Tex.newline
let space = Tex.space
let correct_choice_indicator = Tex.correct_choice_indicator

let points_correct = 1.0
let points_incorrect = 0.0

(* *_single_par flags are used for html post-processing:
 *  "true" means that this was a single paragraph and the
 * <p> </p> annotations must be removed.
 *) 
let body_is_single_par = Tex2html.Generic false
let explain_is_single_par = Tex2html.Generic false
let hint_is_single_par = Tex2html.Generic false
let refsol_is_single_par = Tex2html.Generic false
let rubric_is_single_par = Tex2html.Generic false
let title_is_single_par = Tex2html.Generic true
let atom_is_code lang_opt arg_opt = Tex2html.Code (lang_opt, arg_opt)
(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Utilities
 **********************************************************************)

(* if points (po) is None or 0.0 then
   empty string, else the points *)
let normalize_point_val po = 
	match po with 
  | None -> None
  | Some p -> 
			let pts = float_of_string p in
			if pts = 0.0 then None
      else Some (Float.to_string pts)

let tokenize sa_opt sb_opt = 
	let mk sopt =
		match sopt with 
		| None -> [ ] 
		| Some s -> Words.tokenize_spaces s
	in
	(mk sa_opt, mk sb_opt)

(* Translate string to xml *)
let str_to_xml tex2html kind source =
	let source_xml = tex2html kind source in
    (source_xml, source)

(* Translate source string option to xml, return both *)
let str_opt_to_xml tex2html kind source_opt =
   match source_opt with 
   | None -> None 
   | Some source -> 
			 let source_xml = tex2html kind source  in
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

(**********************************************************************
 ** END: Utilities
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
 *********************************************************************)

type t_label = Label of string
type t_depend = Depend of string list 
type t_point_val = float

module Prompt  = 
struct
	type t = 
			{	mutable kind: string;
				mutable point_val: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				mutable body: string;
				label_is_given: bool
			} 
  let kind p = p.kind
  let point_val p = p.point_val
	let label p = p.label
	let depend p = p.depend
	let body p = p.body
	let label_is_given p = p.label_is_given

  let make   
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None) 
			kind
			body = 
		match label with 
		| None -> 
				{kind; point_val; title; label; depend; body; label_is_given=false}
		| Some _ -> 
				{kind; point_val; title; label; depend; body; label_is_given=true}

  (* Traverse prompt by applying f to its fields *) 
  let traverse prompt state f = 
		let {kind; point_val; title; label; body} = prompt in
		let _ = d_printf "Prompt.traverse: %s " kind in
(*
    let _ = d_printf_optstr "label " label in
*)
      f Ast_prompt state ~kind:(Some kind) ~point_val ~title:None ~label ~depend:None ~contents:(Some body)

  let to_tex prompt = 
		let {kind; point_val; title; label; body} = prompt in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let heading = Tex.mk_command kind point_val in
		let l = 
			if label_is_given prompt then	""
			else Tex.mk_label label 

		in
		  heading ^ l ^ 
		  body 


  (* If prompt doesn't have a label, then
	 * assign fresh label to prompt (unique wrt label_set).
   * Return title and body tokens
	 * To assign label use words from title and body.  
	*)
	let assign_label prefix label_set prompt = 		
    let _ = printf "Prompt.label, is_given = %B\n" prompt.label_is_given in
		let (tt, tb) = tokenize prompt.title (Some (prompt.body)) in
		let _ = 
			match prompt.label with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind prompt.kind in
					let l = Labels.mk_label_force label_set lk prefix (tt @ tb) in
					prompt.label <- Some l
		| Some _ -> ()
		in
    	(tt, tb)

  let body_to_xml tex2html prompt =
		let _ = d_printf "prompt.body_to_xml: prompt = %s" prompt.kind in
		tex2html Xml.body prompt.body

  let to_xml tex2html prompt = 
		let {kind; point_val; title; label; depend; body} = prompt in
		(* Translate body to xml *)
    let body_xml = body_to_xml tex2html prompt in
		let point_val = normalize_point_val point_val in
    let depend = depend_to_xml depend in
    let titles = str_opt_to_xml tex2html Xml.title title in
		let r = 
			Xml.mk_prompt 
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

type prompt = Prompt.t

module Problem =
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				body: string;
				prompts: prompt list
			} 

  let kind p = p.kind
  let point_val p = p.point_val
  let title p = p.title
	let label p = p.label
	let depend p = p.depend
	let body p = p.body
	let prompts p = p.prompts

	let make  
			?kind: (kind = Tex.kw_cluster) 
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None)
			body 
			prompts = 
				{kind; point_val; title; label; depend; body=body; prompts=prompts}

  (* Traverse (pre-order) problem by applying f to its fields *) 
  let traverse problem state f = 
		let {kind; point_val; title; label; depend; body; prompts} = problem in
		let s = f Ast_problem state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:(Some body) in

		let prompt_tr_f state prompt = Prompt.traverse prompt state f in
		let _ = d_printf "Problem.traverse: %s " kind in
(*
    let _ = d_printf_optstr "label " label in
*)
		  List.fold_left prompts ~init:s ~f:prompt_tr_f

  let to_tex problem = 
		let {kind; point_val; title; label; depend; body; prompts} = problem in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
		let heading = Tex.mk_command kind point_val in
		let l = Tex.mk_label label in
		let d = Tex.mk_depend depend in
		let prompts = map_concat_with newline Prompt.to_tex prompts in
		  heading ^  l ^  d ^ 
      body ^ prompts

  (* If problem doesn't have a label, then
	 * assign fresh label to_tex problem prompt (unique wrt label_set).
   * Return title and body tokens
	 * To assign label use words from title and body.  
	*)
	let assign_label prefix label_set problem = 		
    let _ = printf "Problem.assign_label\n" in
		let t_p = List.map problem.prompts ~f:(Prompt.assign_label prefix label_set) in
		let (tt_p, tb_p) = collect_labels t_p in
		let (tt, tb) = tokenize (title problem) (Some (body problem)) in
		let tt_all = tt @ tt_p in
		let tb_all = tb @ tb_p in
		let t_all = tt_all @ tb_all in
		let _ = 
			match  problem.label with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind problem.kind in
					let l = Labels.mk_label_force label_set lk prefix t_all in
					problem.label <- Some l
		| Some _ -> ()
		in
		(tt_all, tb_all)

  let to_xml tex2html problem = 
		let {kind; point_val; title; label; depend; prompts} = problem in
		let point_val = normalize_point_val point_val in
    let titles = str_opt_to_xml tex2html Xml.title title in
    let depend = depend_to_xml depend in
		let prompts = map_concat_with newline (Prompt.to_xml tex2html) prompts in
		let r = 
			Xml.mk_problem 
				~kind:kind 
        ~pval:point_val
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body:prompts
   in
     r

end

type problem = Problem.t


module Atom  = 
struct
	type t = 
			{	mutable kind: string;
				mutable point_val: string option;
				mutable title: string option;
				mutable label: string option; 
				depend: string list option;
				mutable body: string;
        problem: problem option; 
				label_is_given: bool
			} 
  let kind a = a.kind
  let point_val a = a.point_val
  let title a = a.title
	let label a = a.label
	let depend a = a.depend
	let body a = a.body
	let problem a = a.problem
	let label_is_given a = a.label_is_given

  let make   
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None)
			?problem: (problem = None)
			kind
			body = 
		match label with 
		| None -> 
				{kind; point_val; title; label; depend; problem; body=body; 
					label_is_given=false}
		| Some _ -> 
				{kind; point_val; title; label; depend; problem; body=body; 
					label_is_given=true}

  (* Traverse atom by applying f to its fields *) 
  let traverse atom state f = 
		let {kind; point_val; title; label; depend; problem; body} = atom in
		let _ = d_printf "Atom.traverse: %s " kind in
(*
    let _ = d_printf_optstr "label " label in
*)
    let s = f Ast_atom state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:(Some body) in
	  match problem with 
		| None -> s
		| Some p -> Problem.traverse p s f 

  let to_tex atom = 
		let {kind; point_val; title; label; depend; problem; body} = atom in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
		let problem = 
			match problem with 
			| None -> ""
			| Some problem -> Problem.to_tex problem 
		in
		let h_begin = Tex.mk_begin kind point_val title in
		let h_end = Tex.mk_end kind in
		let l = 
			if label_is_given atom then	""
			else Tex.mk_label label 

		in
		let d = Tex.mk_depend depend in		
		  h_begin ^
		  l ^ 
		  d ^ 
		  body ^ newline ^ problem ^ newline ^
      h_end		



  (* If atom doesn't have a label, then
	 * assign fresh label to atom (unique wrt label_set).
   * Return title and body tokens.
	 * To assign label use words from title and body.  
	*)
	let assign_label prefix label_set atom = 		
    let _ = printf "Atom.assign_label\n" in
		let (tt_p, tb_p) = 
			match atom.problem with
			| None -> ([], [])
			| Some p -> Problem.assign_label prefix label_set p in
		let (tt, tb) = tokenize (title atom) (Some (body atom)) in
		let (tt_all, tb_all) = (tt @ tt_p, tb @ tb_p) in
		let _ = 
			match (label atom) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind atom) in
					let l = Labels.mk_label_force label_set lk prefix (tt_all @ tb_all) in
					atom.label <- Some l
		| Some _ -> ()
		in
    	(tt_all, tb_all)

  let body_to_xml tex2html atom =
		if atom.kind = Xml.lstlisting then
			let _ = d_printf "body_to_xml: atom = %s, Promoting to code" atom.kind in
			let _ = atom.kind <- Xml.code in
			let title = str_of_str_opt atom.title in
			let newbody = 
				"\\begin{lstlisting}" ^ "[" ^ title ^ "]" ^ newline ^
				atom.body ^ newline ^
				"\\end{lstlisting}"					
			in
			let _ = atom.body <- newbody in
			let _ = atom.title <- None in
			let body_xml = tex2html Xml.body newbody in
			body_xml
		else
			let _ = d_printf "body_to_xml: atom = %s, Not promoting to code" atom.kind in
			tex2html Xml.body atom.body
			
  let to_xml tex2html atom = 
		(* Translate body to xml *)
    let body_xml = body_to_xml tex2html atom in
		(* Atom has changed, reload *)
		let {kind; point_val; title; label; depend; problem; body} = atom in
    let depend = depend_to_xml depend in
		let point_val = normalize_point_val point_val in
    let titles = str_opt_to_xml tex2html Xml.title title in
		let problem_xml:string = 
			match problem with 
			| None -> ""
			| Some problem -> Problem.to_xml tex2html problem 
		in
		let r = 
			Xml.mk_atom 
				~kind:kind 
        ~pval:point_val
        ~topt:titles
        ~lopt:label
				~dopt:depend 
        ~body_src:body
        ~body_xml:body_xml
        ~problem_xml
        ~ilist_opt:None
        ~hints_opt:None
        ~refsols_opt:None
        ~explains_opt:None
        ~rubric_opt:None
   in
     r


end

type atom = Atom.t

module Group =
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				atoms: atom list
			} 

  let kind g = g.kind
  let point_val g = g.point_val
  let title g = g.title
	let label g = g.label
	let depend g = g.depend
	let atoms g = g.atoms

	let make  
			?kind: (kind = Tex.kw_cluster) 
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None)
			atoms = 
				{kind; point_val; title; label; depend; atoms=atoms}

  (* Traverse (pre-order) group by applying f to its fields *) 
  let traverse group state f = 
		let {kind; point_val; title; label; depend; atoms} = group in
		let s = f Ast_group state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:None in

		let atom_tr_f state atom = Atom.traverse atom state f in
		let _ = d_printf "Group.traverse: %s " kind in
(*
    let _ = d_printf_optstr "label " label in
*)
		  List.fold_left atoms ~init:s ~f:atom_tr_f

  let to_tex group = 
		let {kind; point_val; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
		let h_begin = Tex.mk_begin kind point_val title in
		let h_end = Tex.mk_end kind in
		let l = Tex.mk_label label in
		let d = Tex.mk_depend depend in
		let atoms = map_concat_with newline Atom.to_tex atoms in
		  h_begin ^ 
		  l ^ 
		  d ^ 
		  atoms ^ h_end		

  (* If group doesn't have a label, then
	 * assign fresh label to each atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label use words from title and body.  
	*)
	let assign_label prefix label_set group = 		
		let t_a = List.map group.atoms ~f:(Atom.assign_label prefix label_set) in
    let (tt_a, tb_a) = collect_labels t_a in
		let tt_g = Words.tokenize_spaces_opt (title group) in
    let (tt_all, tb_all) = (tt_g @ tt_a, tb_a) in
		let _ = 
			match (label group) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind group) in
					let l = Labels.mk_label_force label_set lk prefix (tt_all @ tb_all) in
					group.label <- Some l
		| Some _ -> ()
		in
		(tt_all, tb_all)

  let to_xml tex2html group = 
		let {kind; point_val; title; label; depend; atoms} = group in
		let point_val = normalize_point_val point_val in
    let titles = str_opt_to_xml tex2html Xml.title title in
    let depend = depend_to_xml depend in
		let atoms = map_concat_with newline (Atom.to_xml tex2html) atoms in
		let r = 
			Xml.mk_group 
				~kind:kind 
        ~pval:point_val
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

  (* If block doesn't have a label, then
	 * assign fresh label the block (unique wrt label_set).
	 * To assign label use words from title and body.
	 * Return the label assigned.
	*)
	let assign_label prefix label_set e =
 		match e with
		| Element_atom a ->
				Atom.assign_label prefix label_set a
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

	let to_xml tex2html e = 
		match e with
		| Element_atom a ->
				Atom.to_xml tex2html a
		| Element_group g ->
				Group.to_xml tex2html g

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

  let to_xml tex2html block = 
		let {point_val; label; elements} = block in
		let elements = map_concat_with newline (Element.to_xml tex2html) elements in
		elements


end

type block = Block.t

module Segment =
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
 				title: string;
				mutable label: string option; 
				depend: string list option;
				block: block;
				mutable subsegments: t list
			} 
  let kind s = s.kind
  let point_val s = s.point_val
  let title s = s.title
	let label s = s.label
	let depend g = g.depend
	let block s = s.block
	let subsegments s = s.subsegments

	let make  
			?kind: (kind = Tex.kw_gram) 
			?point_val: (point_val = None) 
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
		{kind; point_val; title; label; depend; 
		 block = block; subsegments = subsegments}

  (* Traverse (pre-order) group by applying f to its fields *) 
  let rec traverse segment state f = 
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let _ = d_printf "Segment.traverse: %s title = %s " kind title in
(*
    let _ = d_printf_optstr "label " label in
*)
		let s = f Ast_segment state ~kind:(Some kind) ~point_val ~title:(Some title) ~label ~depend ~contents:None in

		let block_tr_f state block = Block.traverse block state f in
		let subsegment_tr_f state subsegment = traverse subsegment state f in

		let state_b = Block.traverse block state f in
		let state_s = List.fold_left subsegments ~init:state_b ~f:subsegment_tr_f in
		state_s

  (* Convert to string with levels.
   * Used for debugging only.
   *)
  let rec to_tex_level level segment = 		
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let level_str = string_of_int level in
		let h_begin = Tex.mk_segment_header (level_str ^ kind) point_val title in
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
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let h_begin = Tex.mk_segment_header kind point_val title in
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
			let tt_s = Words.tokenize_spaces (title segment) in
			match Labels.mk_label label_set lk prefix tt_s with
			| None -> 
	      let tokens = tt_s @ t_b in
        let l = Labels.mk_label_force label_set lk prefix tokens in
      	segment.label <- Some l
    	| Some l -> segment.label <- Some l

  let rec normalize segment = 
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let _ = Block.normalize block in
		let _ = List.map subsegments ~f:normalize in
		()


  let rec to_xml tex2html segment = 
		let {kind; point_val; title; label; depend; block; subsegments} = segment in
		let point_val = normalize_point_val point_val in
    let titles = str_opt_to_xml tex2html Xml.title (Some title) in
    let depend = depend_to_xml depend in
		let block =  Block.to_xml tex2html block in
		let subsegments = map_concat_with newline (to_xml tex2html) subsegments in
		let body = block ^ newline ^ subsegments in
		let r = 
			Xml.mk_group 
				~kind:kind 
        ~pval:point_val
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
					let _ = printf "Preamble found: pos = %d, found = %d" no found in
					(no+1, found + 1)
				else
					(no+1, found)
		| _ -> (no, found)
		end
	in
	let (_, found) = Segment.traverse ast (0, 0) check in	
	if found = 0  then
		(printf ("Preamble check: no preamble found.\n");
		 true)
  else if found = 1 then
		(printf ("Preamble check:  one preamble found.\n");
		 true)
  else if found > 1 then
		(printf ("Preamble check: error. Multiple preambles found.\n");
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
  let chlabel = Segment.label ast in
	 match chlabel with 
	 | None -> (printf "Fatal Error." ; exit 1)
	 | Some chl -> 
     let prefix = Labels.drop_label_prefix chl in
       Segment.assign_label prefix label_set ast 

let normalize ast = 
	Segment.normalize ast

let to_tex ast = 
	Segment.to_tex ast

let to_xml tex2html ast = 
	let xml:string = Segment.to_xml tex2html ast in
	Xml.mk_standalone xml 

(* Ast validation *)
let validate ast = 
  let passed = check_preamble ast in
	if passed then
		(printf "Ast validation passed.\n";
		 ast)
	else
		(printf "Fatal Error: Ast validation failed. Terminating.\n";
		 exit 1;
		 ast)
 
