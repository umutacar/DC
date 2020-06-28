(********************************************************************** 
 ** tex/tex_parser.mly
 **********************************************************************)
%{
open Core
open Printf
open Utils


module Ast = Ast
module Tex = Tex_syntax

module Atom_lexer = Tex_atom_lexer
module Atom_parser = Tex_atom_parser

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args
*)
let parse_error s = printf "Parse Error: %s"

let mk_point_val_f_opt (s: string option) = 
  match s with
  | None -> (None, "None")
  | Some x -> (Some (float_of_string x), "Some " ^ x)

let extend_labels ell elopt = 
	match elopt with 
	| None -> ell
	| Some l -> ell @ [l]

let d_print_labels ell =
	d_printf_strlist "labels = " ell

let take_label ell = 
	match ell with 
	| [ ] -> None
	| h::_ -> Some h


let labels: (string list) ref = ref [ ] 
let insert_label l =
  labels := l::(!labels)
let get_labels () = 
	!labels
let get_label () = 
	match !labels with 
	| [ ] -> None
	| h::_ -> Some h

let reset_labels () = 
	labels := [ ]

(* Given kind and segments,
 * partition segments into those that are subsegments of
 * kind and that are not, return them in that order.
 *)
let nesteds_and_not kind (segments: Ast.segment List.t) = 
	let is_subsegment s = 
		Tex.segment_is_nested (Ast.Segment.kind s) kind 		
	in
	List.partition_tf segments is_subsegment

let str_of_items items = 
	str_of_str2_list items

(* Given input string, 
 * parse it using Atom_parser
 *)
let parse_atom input = 
	let _ = d_printf "* matched atom: atom_to_ast input =\n%s" input in 
  let lexbuf = Lexing.from_string input in
	Atom_parser.top Atom_lexer.lexer lexbuf

let mk_prompt (kind, point_val, body) =
	Ast.Prompt.make ~point_val:point_val kind body 

(*
let mk_problem items = 
	begin
		match items with 
		| [ ] -> None
		| (kind_problem, point_val, body)::prompts ->
				let	prompts = List.map prompts ~f:mk_prompt in
				let p = Ast.Problem.make ~kind:kind_problem ~point_val:point_val body prompts in
				Some p
	end
*)

let str_of_kw_args kw_args =
  str_of_str2_list_with " = " ";" kw_args
%}	

%token EOF


%token <string> KW_FOLD
(* KW_HEADING token has kind, argument, point value, strategy *)
%token <string * string * string option * string option> KW_HEADING
%token <string * string * string option> KW_BEGIN_GROUP
%token <string * string> KW_END_GROUP


%token <string> HSPACE
%token <string> NEWLINE

(* the "chunk" itself and the label defined if any *)
%token <string * string option> CHUNK
(* the "chunk" itself and the label defined if any *)
%token <string * string option> PAR_CHUNK



%start top

%type <Ast.ast option> top

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/

/* Horizontal space.
 */
hspace: 
  s = HSPACE
  {s}

hspaces: 
  {""}
| xs = hspaces;
  x = hspace
  { xs ^ x }

/* Non-space chunk.
 * Not at a paragraph-start position.
 */
chunk: 
| k = CHUNK
  { let (text, ellopt) = k in
    k
	}

/* Non-space chunk at the beginning of a paragraph */
par_chunk: 
| k = PAR_CHUNK
  { let (text, elopt) = k in
(*    let _ = d_printf "Parser matched par_chunk = %s" d in *)
	  k
	}

/* All blobs */
blob: 
  s = hspace
  { (s, None)}
| c = chunk
  { c }

blobs: 
  {"", [ ]}
| bs = blobs;
  b = blob
  { let (bs, ell) = bs in
	  let (b, elopt) = b in
		(bs ^ b, extend_labels ell elopt)
	}
/* A newline. */
newline: 
  nl = NEWLINE
  {nl}

/* A visibly empty line. */
emptyline: 
| s = hspaces;
  nl = newline
  {s ^ nl}

emptylines:
  {""}
| i = emptylines
  x = emptyline
  {i ^ x}

/* A nonempty line. */
line: 
  hs = hspaces;
  k = chunk;
  bs = blobs;
  nl = newline
  {let (k, elopt) = k in
	 let (bs, ll) = bs in
	 let l = hs ^ k ^ bs ^ nl in
(*   let _ = d_printf "* line: %s.\n" l in	 *)
     (l, extend_labels ll elopt)
  }

/* A nonempty line at the start of a paragraph. 
 * Starts with a chunk
 */
line_parstart: 
  hs = hspaces;
  ps = par_chunk;
  bs = blobs;
  nl = newline
  {
		let (ps, elopt) = ps in
		let (bs, ell) = bs in
		let all = hs ^ ps ^ bs ^ nl in
(*		let _ = d_printf "!Parser matched: line_parstart_sig %s.\n" l in *)
    (all, extend_labels ell elopt)
  }


/* A text paragraph. 
 *
 */
textpar: 
	| lp = line_parstart;
		tail = textpar_tail;
		{ 
  	  let (all, ell_lp) = lp in
			let (tail, ell_tail) = tail in
 	 		(all ^ tail, ell_lp @ ell_tail)
	}  

textpar_tail:
  el = emptyline
  {"", [ ]}
| l = line;
  tp = textpar_tail
  { 
		let (l, ell_l) = l in
    let (tp, ell_tp) = tp in
    (l ^ tp, ell_l @ ell_tp)
  } 


/**********************************************************************
 ** BEGIN: Latex Segments
 **********************************************************************/

heading:
  h = KW_HEADING 
		{ let (kind, heading, pval_opt, strategy_opt) = h in 
		  (kind, heading, pval_opt, strategy_opt) 
		}

top:
	fs = emptylines;
  hspaces;
  ss = segments;
  EOF
  {match Ast.Segment.nest_segments ss with
	 | None -> 
	 let _ = printf "Fatal Error.  There should be a top level chapter" in
	 None
	 | Some s -> Some s
	 }

segment: 
  h = heading;
  b = block;
  {
   let (kind, title, pval_opt, strategy_opt) = h in
   (*
   let _ = d_printf ("!parser: kind=%s, strategy = %s, title = %s matched\n") kind (str_of_str_opt strategy_opt) title in 
   *)
     Ast.Segment.make ~kind:kind ~point_val:pval_opt ~strategy:strategy_opt title b []
  }	  

/* segments */
segments:
  { [ ] }
| s = segment; 
  ss = segments;
  { 
	 [ s ] @ ss 
	 }


/**********************************************************************
 ** END: Latex Segments
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Blocks
 ** A blocks is sequence of elements
 **********************************************************************/

block: 
| es = elements; 
  tt = emptylines;
  hs = hspaces  
  {
	 let (es, ell_es) = es in
	 let label = take_label ell_es in
(*   let _ = d_printf "parser matched: block.\n" in *)
(*   let _ = d_print_labels ell_es in *)
     Ast.Block.make ~label es
  }

/**********************************************************************
 ** END: Blocks
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Elements
 ** An element is a group, a problem cluster, or an atom 
 **********************************************************************/

atom: 
  fs = emptylines;
  tp_all = textpar;
  {	 
	 let (all, ell) = tp_all in
   let _ = d_printf "tex_parser: atom all = %s \n"  all  in
   let a = parse_atom all in
	 let (kind, popt, plopt, kw_args, lopt, body, capopt, prompts) = 
	   match a with 
		 | None -> (Tex.kw_gram, None, None, [], None, all, None, [])
		 | Some (kind, popt, kw_args, lopt, body, capopt, items) -> 
         match Tex.is_atom kind with
				 | Some (kind, plopt) -> 
             let prompts = Ast.prompts_of_items items in
    					 (kind, popt, plopt, kw_args, lopt, body, capopt, prompts)
         | None ->
           (* Promote all non-atom, but single standing environments to gram atom. *)
					 (Tex.kw_gram, None, None, [], None, all, None, [])
					 
	 in			 
(*   let _ = d_printf "tex_parser: atom.kw_args = %s \n" (str_of_kw_args kw_args) in *)
	 let body = String.strip ~drop:is_vert_space body in
   let _ = d_printf "tex_parser: atom body = %s \n"  body  in
   let version_opt = find_in_list kw_args "version" in
   let topt = find_in_list kw_args "title" in
   let copt = find_in_list kw_args "cover" in
   let sopt = find_in_list kw_args "sound" in
   let pl_version_opt = 
     if Tex.is_code kind then   
			 let _ = d_printf "code atom: version = %s\n" (str_of_str_opt version_opt) in
			 version_opt
		 else
			 None
	 in

	 if (Tex.is_label_only body) && (capopt==None) && (List.length(prompts)==0) then
		 ([ ], ell)
	 else
		 let a = Ast.Atom.make 
				 ~point_val:popt 
				 ~pl:plopt 
				 ~pl_version:pl_version_opt
				 ~title:topt
         ~cover:copt
         ~sound:sopt 
				 ~label:lopt  
				 ~caption:capopt  
				 ~prompts:prompts
				 kind  
				 body
		 in
		 ([ a ], ell)
  }

atoms:
| 
	{ ([ ], [ ]) }
| aa = atoms;
	el = emptylines;
  f = KW_FOLD
	a = atom
		{ let (aa, ell_aa) = aa in
		  let (a, ell_a) = a in
			  (aa @ a, ell_aa @ ell_a)
		}
| aa = atoms;
	a = atom
		{ let (aa, ell_aa) = aa in
		  let (a, ell_a) = a in
			(aa @ a, ell_aa @ ell_a)
		 }

atoms_and_tailspace:
  aa = atoms;
	ts = emptylines
		{ aa }

group: 
| fs = emptylines;
  hs = hspaces;
	b = KW_BEGIN_GROUP
  aa = atoms_and_tailspace;  
  e = KW_END_GROUP
  { let (aa, ell_aa) = aa in
		let (kb, hb, _) = b in
	  let (ke, he) = e in
	    if kb = ke then
				let kind = kb in
				let label = take_label ell_aa in
				Ast.Group.make ~kind ~label aa
			else
				(printf "Error: group start and end should match.";
				 exit 1)
 }


element:
| a = atom
  { 
(*	  let _ = d_printf "!Parser: matched element: atom\n %s" "a" in *)
	  match a with 
		| ([ ], ell) -> ([ ], ell)
		| (a::[ ], ell) -> ([Ast.Element.mk_from_atom a], ell) 
  }
| g = group;
  { 
(*	 let _ = d_printf "!Parser: matched group\n %s" "g" in *)
      ([Ast.Element.mk_from_group g], [ ]) 
  }

elements:
  {([ ], [ ])}
| es = elements;
  e = element; 
  { let (es, ell_es) = es in
	  let (e, ell_e) = e in
  	  (es @ e, ell_es @ ell_e)
 }

/**********************************************************************
 ** END: Elements
 **********************************************************************/


