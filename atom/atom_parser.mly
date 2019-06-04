%{
open Core
open Printf
open Ast
open Utils

let parse_error s = printf "Parse Error: %s"

let mk_point_val_f_opt (s: string option) = 
  match s with
  | None -> (None, "None")
  | Some x -> (Some (float_of_string x), "Some " ^ x)

%}	

%token EOF


%token <string * string * string option> KW_HEADING
%token <string * string option> KW_CHAPTER
%token <string * string option> KW_SECTION
%token <string * string option> KW_SUBSECTION
%token <string * string option> KW_SUBSUBSECTION	
%token <string * string option> KW_PARAGRAPH	

%token <string * string> KW_LABEL_AND_NAME

%token <string> COMMENT
%token <string> ENV
%token <string> HSPACE
%token <string> NEWLINE
%token <string> PERCENT_ESC
%token <string> SIGCHAR

%token <string> PAR_COMMENT
%token <string> PAR_ENV
%token <string> PAR_PERCENT_ESC
%token <string> PAR_SIGCHAR


%start top
%type <string> top

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/

/* Horizontal space.
   Includes comments.
 */
hspace: 
  s = HSPACE
  {s}

hspaces: 
  {""}
| xs = hspaces;
  x = hspace
  {xs ^ x}

/* Non-space char */
sigchar: 
  d = SIGCHAR
  {d}
| d = PERCENT_ESC
  {d}
| e = ENV
  {let _ = d_printf "parser matched: sigchar, env = %s" e in
     e
  }

/* Non-space char at the beginning of a paragraph */
parsigchar: 
  d = PAR_SIGCHAR
  {d}
| d = PAR_PERCENT_ESC
  {d}
| e = PAR_ENV
  {let _ = d_printf "parser matched: sigchar, env = %s" e in
     e
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
  s = hspaces;
  nl = newline
  {s ^ nl}

/* A comment line. */
commentline:
  hs = hspaces;
  c = COMMENT;
 {let _ = d_printf "!Parser mached: commentline.\n" in
   hs ^ c
  }
   
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
| hs = hspaces;
  d = sigchar;
  cs = chars;
  c = COMMENT;
  {let l = hs ^ d ^ cs ^ c in
   let _ = d_printf "!Parser mached: significant line: %s.\n" l in
     l
  }
| hs = hspaces;
  c = COMMENT;
 {let l =  hs ^ c in 
  let _ = d_printf "!Parser mached: line (comment): %s\n" l in
    l  
  }

/* A nonempty line at the start of a paragraph. */
parline: 
  hs = hspaces;
  d = parsigchar;
  cs = chars;
  nl = newline
  {let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "!Parser mached: par begin %s.\n" l in
     l
  }
| hs = hspaces;
  d = parsigchar;
  cs = chars;
  c = COMMENT;
  {let l = hs ^ d ^ cs ^ c in
   let _ = d_printf "!Parser mached: significant line %s.\n" l in
     l
  }
| hs = hspaces;
  c = PAR_COMMENT;
  {let l = hs ^ c in
   let _ = d_printf "!Parser mached: significant line %s.\n" l in
     l
  }


ignorables:
  {""}
| i = ignorables
  x = emptyline
  {i ^ x}

/* A latex environment. */
env: 
  x = ENV
  {x}  

/* A text paragraph. 
   It contains environments because environments are 
   significant characters.
*/
textpar: 
  x = parline;
  tail = textpar_tail
  {x ^ tail}  

textpar_tail:
  el = emptyline
  {el}
| x = line;
  tp = textpar_tail
  { 
    x ^ tp
  } 

/**********************************************************************
 ** BEGIN: Latex Segments
 **********************************************************************/

label:
  l = KW_LABEL_AND_NAME
  {let (all, label) = l in 
   let _ = d_printf "Parser matched label = %s all = %s" label all in
     all
  }

heading:
  h = KW_HEADING 
		{ let (kind, heading, pval_opt) = h in 
  		let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
			(kind, heading, pval_f_opt) 
		}

top:
  s = segment;
  EOF
  {s}

segment: 
  h = heading;
  l = option(label);
  b = block;
  ss = segments
  {
   let (kind, heading, pval_opt) = h in
   let _ = d_printf ("!parser: %s %s matched") kind heading in
     heading ^ b ^ ss
  }	  

segments:
	{ " "}
| ss = segments; 
  s = segment;
  { ss ^ s }

/**********************************************************************
 ** END: Latex Segments
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Blocks
 ** A blocks is sequence of elements
 **********************************************************************/

block: 
| es = elements; 
  tt = ignorables
  {
   let _ = d_printf ("parser matched: blocks.\n") in 
     es ^ tt
  }

/**********************************************************************
 ** END: Blocks
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Elements
 ** An element is a group, a problem cluster, or an atom 
 **********************************************************************/

element:
  ft = ignorables;
	e = env  
  {let _ = d_printf "!Parser: matched element %s" e in
     ft ^ e
  }
|
 ft = ignorables;
  tp = textpar;
  {let para = ft ^ "\\begin{gram}" ^ "\n" ^ tp ^ "\n" ^ "\\end{gram}\n" in
   let _ = d_printf "!Parser: matched text paragraph\n %s" para in
     para
  }


elements:
  {""}
| es = elements;
  e = element; 
  {es ^ e}

/**********************************************************************
 ** END: Elements
 **********************************************************************/
