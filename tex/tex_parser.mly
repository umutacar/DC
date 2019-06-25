%{
open Core
open Printf
open Utils


module Ast = Ast
module Tex = Tex_syntax

module Atom_lexer = Atom_lexer
module Atom_Parser = Atom_parser

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
let atom_to_ast input = 
	let _ = d_printf "atom_to_ast input = %s" input in
      let lexbuf = Lexing.from_string input in
	    Atom_parser.top Atom_lexer.lexer lexbuf


%}	

%token EOF


%token <string> KW_FOLD
%token <string * string * string option> KW_HEADING
%token <string * string * string option> KW_BEGIN_GROUP
%token <string * string> KW_END_GROUP


%token <string> HSPACE
%token <string> NEWLINE

(* the "character" itself and the label defined if any *)
%token <string * string option> SIGCHAR
(* the "character" itself and the label defined if any *)
%token <string * string option> PAR_SIGCHAR

%token <string> ENV

(* points, title, label, body, all *) 
%token <string> PAR_ENV

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

/* Non-space char.
 * Not at a paragraph-start position.
 */
sigchar: 
| d = SIGCHAR
  { let (d, ellopt) = d in
  	(d, ellopt) 
	}
| e = ENV
  { let _ = d_printf "* env: %s\n" e in
  	  (e, None)
  }

/* Non-space char at the beginning of a paragraph */
parstart: 
| d = PAR_SIGCHAR
  { let (d, elopt) = d in
(*    let _ = d_printf "Parser matched par_sigchar = %s" d in *)
	  (d, elopt) 
	}
| e = PAR_ENV
  { let _ = d_printf "* env: %s\n" e in
  	  (e, None)
  }

/* All characters */
char: 
  s = hspace
  { (s, None)}
| d = sigchar
  { let (d, elopt) = d in 
	  (d, elopt)
	} 

chars: 
  {"", [ ]}
| cs = chars;
  c = char
  { let (cs, ell) = cs in
	  let (c, elopt) = c in
		(cs ^ c, extend_labels ell elopt)
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
  d = sigchar;
  cs = chars;
  nl = newline
  {let (d, elopt) = d in
	 let (cs, ll) = cs in
	 let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "* line: %s.\n" l in	 
     (l, extend_labels ll elopt)
  }

/* A nonempty line at the start of a paragraph. 
 * Starts with a sigchar
 */
line_parstart: 
  hs = hspaces;
  ps = parstart;
  cs = chars;
  nl = newline
  {
		let (ps, elopt) = ps in
		let (cs, ell) = cs in
		let all = hs ^ ps ^ cs ^ nl in
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
		{ let (kind, heading, pval_opt) = h in 
  		let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
			(kind, heading, pval_f_opt) 
		}

top:
	fs = emptylines;
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
   let (kind, title, pval_opt) = h in
(*   let _ = d_printf ("!parser: %s %s matched") kind title in *)
     Ast.Segment.make ~kind:kind title b []
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
  tt = emptylines
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
/*
atom: 
  fs = emptylines;
  tp = textpar;
  {	 
	 let (all, ell) = tp in
	 let all = String.strip ~drop:is_vert_space all in
	 let single = Tex.take_single_env all in
	 let (kind, popt, topt,  lopt, body) = 
	   match single with 
		 | None -> 
				 (Tex.kw_gram, None, None, None, all)
		 | Some (env, _) -> 
				 let atom = atom_to_ast all in						 
				 (env,  popt, topt, lopt, body)  (* favor body computed by the lexer *)
	 in 
(*	 let _ = d_printf "parser matched atom: body = \n %s \n" body in *)
	 let body = String.strip ~drop:is_vert_space body in
	   if Tex.is_label_only body then
(*			 let _ = d_printf "atom is label only" in *)
			 ([ ], ell)
		 else
			 let a = Ast.Atom.make 
					 ~point_val:popt 
					 ~title:topt 
					 ~label:lopt  
					 kind  
					 body
			 in
			 ([ a ], ell)
  }
*/

atom: 
  fs = emptylines;
  tp_all = textpar;
  {	 
	 let (all, ell) = tp_all in
   let a = atom_to_ast all in
	 let (kind, popt, topt,  lopt, body) = 
	   match a with 
		 | None -> (Tex.kw_gram, None, None, None, all)
		 | Some (kind, popt, topt, lopt, body, items, all) -> (kind, popt, topt,  lopt, body)
	 in			 
	 let body = String.strip ~drop:is_vert_space body in
	 if Tex.is_label_only body then
		 ([ ], ell)
	 else
		 let a = Ast.Atom.make 
				 ~point_val:popt 
				 ~title:topt 
				 ~label:lopt  
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
