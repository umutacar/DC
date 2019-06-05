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
%token <string> SIGCHAR

%token <string> PAR_COMMENT
%token <string> PAR_ENV
%token <string> PAR_SIGCHAR

%start top
%type <string> top

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
  {xs ^ x}

/* Non-space char */
sigchar: 
| d = SIGCHAR
  {d}

bigchar:
| d = SIGCHAR
  {d}
| e = ENV
  {let _ = d_printf "parser matched: sigchar, env = %s" e in
     e
  }


/* Non-space char at the beginning of a paragraph */
parsigchar: 
| d = PAR_SIGCHAR
  {d}

/* Non-space char at the beginning of a paragraph */
parbigchar: 
| d = PAR_SIGCHAR
  {d}
| e = PAR_ENV
  {let _ = d_printf "parser matched: sigchar, env = %s" e in
     e
  }
 
/* All characters */
char: 
  s = hspace
  {s}
| d = bigchar
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

emptylines:
  {""}
| i = emptylines
  x = emptyline
  {i ^ x}

/* A nonempty line. */
line: 
  hs = hspaces;
  d = bigchar;
  cs = chars;
  nl = newline
  {let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "!Parser mached: significant line: %s.\n" l in
     l
  }

/* A nonempty line at the start of a paragraph. 
 * Starts with a sigchar
 */
line_parstart_sig: 
  hs = hspaces;
  d = parsigchar;
  cs = chars;
  nl = newline
  {let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "!Parser mached: par begin %s.\n" l in
     l
  }

/* A nonempty line at the start of a paragraph, starts with an env.
*/
line_parstart_env_alone: 
  fs = hspaces;
  e = ENV;
  ts = hspaces;
  nl = newline
  {let l = fs ^ e ^ ts ^ nl in
   let _ = d_printf "!Parser mached: par begin with env %s.\n" l in
     l
  }

line_parstart_env_nonalone: 
  fs = hspaces;
  e = ENV;
  ts = hspaces;
  s = bigchar;
  cs = chars;
  nl = newline
  {let l = fs ^ e ^ ts ^ s ^ cs ^ nl in
   let _ = d_printf "!Parser mached: par begin with env %s.\n" l in
     l
  }


/* A text paragraph. 
 *
 */
textpar: 
	| x = line_parstart_sig;
		tail = textpar_tail
			{x ^ tail}  
	| x = line_parstart_env_nonalone;
		tail = textpar_tail
			{x ^ tail}  
	| x = line_parstart_env_alone;
		tail = textpar_tail_sig
			{x ^ tail}  

textpar_tail:
  el = emptyline
  {el}
| x = line;
  tp = textpar_tail
  { 
    x ^ tp
  } 

textpar_tail_sig:
| x = line;
  el = emptyline
  {el}
| x = line;
  tp = textpar_tail_sig
  { 
    x ^ tp
  } 

/* TODO: complete */
envpar: 
  fs = hspaces;
  e = PAR_ENV
  ts = hspaces;
  nl = newline
  {let p = fs ^ e ^ ts ^ nl in
   let _ = d_printf "!Parser mached: par env %s.\n" p in
     p
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
     "\n" ^ heading ^ b ^ ss
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
  tt = emptylines
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
| ft = emptylines;
  tp = textpar;
  {let para = ft ^ "\\begin{gram}" ^ "\n" ^ tp ^ "\n" ^ "\\end{gram}\n" in
   let _ = d_printf "!Parser: matched text paragraph\n %s" para in
     para
  }
| ft = emptylines;
  ep = envpar;
  {let para = ft ^ ep in
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
