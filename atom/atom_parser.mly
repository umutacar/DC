%{
open Core
open Printf
open Utils


module Ast = Ast_ast
module Tex = Tex_syntax

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

%}	

%token EOF


%token <string> KW_FOLD
%token <string * string * string option> KW_HEADING
%token <string * string * string option> KW_BEGIN_GROUP
%token <string * string> KW_END_GROUP


%token <string> HSPACE
%token <string> NEWLINE
%token <string * string option> SIGCHAR
%token <string option * string option * string option * string * string> ENV
%token <string * string option> PAR_SIGCHAR
%token <string option * string option * string option * string * string> PAR_ENV

%start top

%type <Ast_ast.ast option> top

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

/* Non-space char */
sigchar: 
| d = SIGCHAR
  { let (d, ellopt) = d in
  	(d, ellopt) 
	}
| e = ENV
  { let (popt, topt, lopt, body, all) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched env, label = %s env = %s" label all in
  	  (all, None)
  }

/* Non-space char at the beginning of a paragraph */
parstart: 
| d = PAR_SIGCHAR
  { let (d, elopt) = d in
	  (None, None, None, d, d, elopt) 
	}
| e = PAR_ENV
  { let (popt, topt, lopt, body, all) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched par_env, label = %s env = %s" label in
  	  (popt, topt, lopt, body, all, None)
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
   let _ = d_printf "!Parser mached: significant line: %s.\n" l in	 
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
		let (popt, topt, lopt, body, all, elopt_ps) = ps in
		let (cs, ell) = cs in
		let l = hs ^ all  ^ cs ^ nl in
		let _ = d_printf "!Parser matched: line_parstart_sig %s.\n" l in
    (popt, topt, lopt, body, all, extend_labels ell elopt_ps)
  }


/* A text paragraph. 
 *
 */
textpar: 
	| lp = line_parstart;
		tail = textpar_tail;
		{ 
  	  let (popt, topt, lopt, body, all, ell_lp) = lp in
			let (tail, ell_tail) = tail in
 	 		(popt, topt, lopt, body, all ^ tail, ell_lp @ ell_tail)
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
   let _ = d_printf ("!parser: %s %s matched") kind title in
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
   let _ = d_printf "parser matched: block.\n" in 
   let _ = d_print_labels ell_es in
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
	 let (popt, topt, lopt, body, all, ell) = tp_all in
	 let all = String.strip all in
	 let single = Tex.take_single_env all in
	 let (kind, body) = 
	   match single with 
		 | None -> (Tex.kw_gram, all)
		 | Some (env, _) -> (env,  body)  (* favor body computed by the lexer *)
	 in 
	 let _ = d_printf "parser matched atom: body = \n %s \n" body in
	 let body = String.strip body in
	   if Tex.is_label_only body then
			 let _ = d_printf "atom is label only" in
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
				let label = get_label () in 
				let _ = reset_labels () in 
				Ast.Group.make ~kind ~label aa
			else
				(printf "Error: group start and end should match.";
				 exit 1)
 }


element:
| a = atom
  { let _ = d_printf "!Parser: matched element: atom\n %s" "a" in
	  match a with 
		| ([ ], ell) -> ([ ], ell)
		| (a::[ ], ell) -> ([Ast.Element.mk_from_atom a], ell) 
  }
| g = group;
  { let _ = d_printf "!Parser: matched group\n %s" "g" in
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
