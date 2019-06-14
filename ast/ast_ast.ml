open Core
open Utils

module Labels = Label_set
module Tex = Tex_syntax
module Words = English_words

type ast_member = Ast_atom | Ast_group | Ast_element | Ast_block | Ast_segment

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)
let newline = Tex.newline
let space = Tex.space
let correct_choice_indicator = Tex.correct_choice_indicator

let points_correct = 1.0
let points_incorrect = 0.0

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

(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
*********************************************************************)


type t_label = Label of string
type t_depend = Depend of string list 
type t_point_val = float

module Atom  = 
struct
	type t = 
			{	kind: string;
				mutable point_val: string option;
				title: string option;
				mutable label: string option; 
				depend: string list option;
				body: string;
				label_is_given: bool
			} 
  let kind a = a.kind
  let point_val a = a.point_val
  let title a = a.title
	let label a = a.label
	let depend a = a.depend
	let body a = a.body
	let label_is_given a = a.label_is_given

  let make   
			?point_val: (point_val = None) 
			?title: (title = None) 
			?label: (label = None) 
			?depend: (depend = None)
			kind
			body = 
		match label with 
		| None -> 
				{kind; point_val; title; label; depend; body=body; label_is_given=false}
		| Some _ -> 
				{kind; point_val; title; label; depend; body=body; label_is_given=true}

  (* Traverse atom by applying f to its fields *) 
  let traverse atom state f = 
		let {kind; point_val; title; label; depend; body} = atom in
		let _ = d_printf "Atom.traverse: %s " kind in
    let _ = d_printf_optstr "label " label in
      f Ast_atom state ~kind:(Some kind) ~point_val ~title ~label ~depend ~contents:(Some body)

  let to_tex atom = 
		let {kind; point_val; title; label; depend; body} = atom in
		let point_val = normalize_point_val point_val in
		let point_val = Tex.mk_point_val point_val in
		let title = Tex.mk_title title in
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
		  body ^ newline ^
      h_end		



  (* If atom doesn't have a label, then
	 * assign fresh label to atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label use words from title and body.  
	*)
	let assign_label label_set atom = 		
		let (tt, tb) = tokenize (title atom) (Some (body atom)) in
		let _ = 
			match (label atom) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind atom) in
					let l = Labels.mk_label_force label_set lk "prefix" (tt @ tb) in
					atom.label <- Some l
		| Some _ -> ()
		in
    	(tt, tb)


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
    let _ = d_printf_optstr "label " label in
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
	 * assign fresh label to_tex group atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label use words from title and body.  
	*)
	let assign_label label_set group = 		
		let t_a = List.map (atoms group) ~f:(Atom.assign_label label_set) in
		let tt = List.map t_a ~f:(fun (x, y) -> x) in
		let tb = List.map t_a ~f:(fun (x, y) -> y) in
		let tt_a = 
			match List.reduce tt (fun x y -> x @ y) with
			| None -> [ ]
			| Some tt_a -> tt_a
		in			
		let tb_a =
			match List.reduce tb (fun x y -> x @ y) with
			| None -> [ ] 
			| Some tb_a -> tb_a 
		in
		let tt_g = Words.tokenize_spaces_opt (title group) in
		let ttb_a = tt_g @ tt_a @ tb_a
		in
		let _ = 
			match (label group) with 
			| None ->
					let lk = Tex_syntax.mk_label_prefix_from_kind (kind group) in
					let l = Labels.mk_label_force label_set lk "prefix" ttb_a in
					group.label <- Some l
		| Some _ -> ()
		in
		(tt_a, tb_a)

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
	let assign_label label_set e =
 		match e with
		| Element_atom a ->
				Atom.assign_label label_set a
		| Element_group g ->
				Group.assign_label label_set g

end

type element = Element.t

module Block = 
struct
	type t = 
			{	
				mutable point_val: string option;
				mutable label: string option; 
				elements: element list
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
	let assign_label label_set block = 		
		let t_a = List.map (elements block) ~f:(Element.assign_label label_set)  in
		let tt = List.map t_a ~f:(fun (x, y) -> x) in
		let tb = List.map t_a ~f:(fun (x, y) -> y) in
		let tt_a = 
			match List.reduce tt (fun x y -> x @ y) with 
			| None -> [ ] 
			| Some tt_a -> tt_a 						
		in
		let tb_a = 
			match List.reduce tb (fun x y -> x @ y) with 
			| None -> [ ] 
			| Some tb_a -> tb_a 						
		in
		  tt_a @ tb_a  
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
    let _ = d_printf_optstr "label " label in
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
				

  (* If group doesn't have a label, then
	 * assign fresh label to_tex group atom (unique wrt label_set).
   * Return the updated label set.
	 * To assign label use words from title and body.  
	*)
	let rec assign_label label_set segment = 		
		let t_b = Block.assign_label label_set (block segment) in
		let _ = List.map (subsegments segment) ~f:(assign_label label_set) in
	  match (label segment) with 
		| Some _ -> ()
		| None ->
  	  let lk = Tex_syntax.mk_label_prefix_from_kind (kind segment) in
			let tt_s = Words.tokenize_spaces (title segment) in
			match Labels.mk_label label_set lk "prefix" tt_s with
			| None -> 
	      let tokens = tt_s @ t_b in
        let l = Labels.mk_label_force label_set lk "prefix" tokens in
      	segment.label <- Some l
    	| Some l -> segment.label <- Some l
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

let index = ref 0
let mk_index () = 
  let r = string_of_int !index in
  let _ = index := !index + 1 in
    r

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

let to_tex ast = 
	Segment.to_tex ast

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
  let _ = d_printf_strlist "Initial labels: " label_list in
	let label_set = Segment.traverse ast label_set add_label in
	let label_list = Labels.to_list label_set in
  let _ = d_printf_strlist "All labels: " label_list in
	  label_set


let assign_labels ast = 
	let label_set = collect_labels ast in
	Segment.assign_label label_set ast 

 
