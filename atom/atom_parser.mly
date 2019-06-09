%{
open Core
open Printf
open Utils

let parse_error s = printf "Parse Error: %s"

let mk_point_val_f_opt (s: string option) = 
  match s with
  | None -> (None, "None")
  | Some x -> (Some (float_of_string x), "Some " ^ x)

module Ast = Ast_ast
module Tex = Tex_syntax
%}	

%token EOF


%token <string> KW_FOLD
%token <string * string * string option> KW_HEADING
%token <string * string * string option> KW_BEGIN_GROUP
%token <string * string> KW_END_GROUP
%token <string * string> KW_LABEL_AND_NAME


%token <string> HSPACE
%token <string> NEWLINE
%token <string> SIGCHAR
%token <string * string option * string option> ENV
%token <string * string> PAR_LABEL_AND_NAME
%token <string> PAR_SIGCHAR
%token <string * string option * string option> PAR_ENV

%start top
%type <Ast_ast.ast> top

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
  { d }
| e = ENV
  { let (es, topt, lopt) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched env, label = %s env = %s" label es in
  	  es
  }
| l = KW_LABEL_AND_NAME
  { let (all, label) = l in 
    let _ = d_printf "Parser matched label = %s all = %s" label all in
      all
  }

/* Non-space char at the beginning of a paragraph */
parstart: 
| d = PAR_SIGCHAR
  { (d, None, None) }
| e = PAR_ENV
  { let (es, topt, lopt) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched par_env, label = %s env = %s" label es in
  	  (es, topt, lopt)
  }
| l = PAR_LABEL_AND_NAME
  { let (all, label) = l in 
    let _ = d_printf "Parser matched par_label = %s all = %s" label all in
      (all, None, None)
  }


/* All characters */
char: 
  s = hspace
  {s}
| d = sigchar
  {d} 

chars: 
  {""}
| xs = chars;
  x = char
  {xs ^ x}

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
  {let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "!Parser mached: significant line: %s.\n" l in
     l
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
		let (d, topt, lopt) = ps in
		let l = hs ^ d ^ cs ^ nl in
		let _ = d_printf "!Parser matched: line_parstart_sig %s.\n" l in
    (l, topt, lopt)
  }


/* A text paragraph. 
 *
 */
textpar: 
	| lp = line_parstart;
		tail = textpar_tail;
		{ 
  	  let (x, topt, lopt) = lp in
 	 		(x ^ tail, topt, lopt)
	}  

textpar_tail:
  el = emptyline
  {""}
| x = line;
  tp = textpar_tail
  { 
    x ^ tp
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
  s = segment;
  EOF
  { s }

segment: 
  h = heading;
  b = block;
  ss = segments
  {
   let (kind, title, pval_opt) = h in
   let _ = d_printf ("!parser: %s %s matched") kind title in
     Ast.Segment.make ~kind:kind title b ss
  }	  

/* segments */
segments:
  { [ ] }
| ss = segments; 
  s = segment;
  { ss @ [ s ] }

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
   let _ = d_printf ("parser matched: blocks.\n") in 
     Ast.Block.make es
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
	 let (tp, topt, lopt) = tp_all in
	 let tp = String.strip tp in
	 let single = Tex.take_single_env tp in
	 let (kind, body) = 
	   match single with 
		 | None -> (Tex.kw_gram, tp)
		 | Some (env, body) -> (env,  body)
	 in
	   Ast.Atom.make ~title:topt ~label:lopt  kind  (String.strip body)
  }

atoms:
| 
	{ [ ] }
| aa = atoms;
	el = emptylines;
  f = KW_FOLD
	a = atom
		{ aa @ [ a ] }
| aa = atoms;
	a = atom
		{ aa @ [ a ] }

atoms_and_tailspace:
  aa = atoms;
	ts = emptylines
		{ aa }


group: 
| fs = emptylines;
	b = KW_BEGIN_GROUP
  aa = atoms_and_tailspace;  
  e = KW_END_GROUP
  { let (kb, hb, _) = b in
	  let (ke, he) = e in
	    if kb = ke then
				Ast.Group.make aa
			else
				(printf "Error: group start and end should match.";
				 exit 1)
 }


element:
| a = atom
  {let _ = d_printf "!Parser: matched element: atom\n %s" "a" in
	 [Ast.Element.mk_from_atom a] 
  }
| g = group;
  { let _ = d_printf "!Parser: matched group\n %s" "g" in
      [Ast.Element.mk_from_group g] 
  }

elements:
  {[ ]}
| es = elements;
  e = element; 
  {es @ e}

/**********************************************************************
 ** END: Elements
 **********************************************************************/
