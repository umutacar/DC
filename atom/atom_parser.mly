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
%token <string option * string option * string option * string * string> ENV
%token <string * string> PAR_LABEL_AND_NAME
%token <string> PAR_SIGCHAR
%token <string option * string option * string option * string * string> PAR_ENV

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
  { let (popt, topt, lopt, body, all) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched env, label = %s env = %s" label all in
  	  all
  }
| l = KW_LABEL_AND_NAME
  { let (all, label) = l in 
    let _ = d_printf "Parser matched label = %s all = %s" label all in
		let _ = insert_label label in
      all
  }

/* Non-space char at the beginning of a paragraph */
parstart: 
| d = PAR_SIGCHAR
  { (None, None, None, d, d) }
| e = PAR_ENV
  { let (popt, topt, lopt, body, all) = e in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "Parser matched par_env, label = %s env = %s" label in
  	  (popt, topt, lopt, body, all)
  }
| l = PAR_LABEL_AND_NAME
  { let (all, label) = l in 
    let _ = d_printf "Parser matched par_label = %s all = %s" label all in
		let _ = insert_label label in
      (None, None, None, all, all)
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
		let (popt, topt, lopt, body, all) = ps in
		let l = hs ^ all  ^ cs ^ nl in
		let _ = d_printf "!Parser matched: line_parstart_sig %s.\n" l in
    (popt, topt, lopt, body, all)
  }


/* A text paragraph. 
 *
 */
textpar: 
	| lp = line_parstart;
		tail = textpar_tail;
		{ 
  	  let (popt, topt, lopt, body, all) = lp in
 	 		(popt, topt, lopt, body, all ^ tail)
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
   let _ = d_printf "parser matched: block.\n" in 
	 let _ = d_printf_strlist "block labels = " (get_labels ()) in
	 let label = get_label () in 
	 let _ = reset_labels () in 
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
	 let (popt, topt, lopt, body, all) = tp_all in
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
			 [ ]
		 else
			 let a = Ast.Atom.make 
					 ~point_val:popt 
					 ~title:topt 
					 ~label:lopt  
					 kind  
					 body
			 in
			 [ a ]
  }

atoms:
| 
	{ [ ] }
| aa = atoms;
	el = emptylines;
  f = KW_FOLD
	a = atom
		{ aa @ a }
| aa = atoms;
	a = atom
		{ aa @ a }

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
		| [ ] -> [ ]
		| a::[ ] -> [Ast.Element.mk_from_atom a] 
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
