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


%token <string> NEWLINE
%token <string> HSPACE
%token <string> SIGCHAR
%token <string> PERCENT
%token <string> PERCENT_ESC
%token <string> COMMENT
%token <string> NEWLINE_COMMENT
%token <string> ENV


%token <string * string option> KW_CHAPTER
%token <string * string option> KW_SECTION
%token <string * string option> KW_SUBSECTION
%token <string * string option> KW_SUBSUBSECTION	
%token <string * string option> KW_PARAGRAPH	

%start chapter
%type <string> chapter

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
  {e}
 
/* All characters */
char: 
  s = hspace
  {s}
| p = PERCENT
  {p}
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

emptylines:
  {""}
| els = emptylines;
  el = emptyline
  {let _ = d_printf "!Parser mached: emptylines.\n" in
     els; el
   }

/* A comment line. */
commentline:
  hs = hspaces;
  p = PERCENT;
  cs = chars;
  nl = newline
 {let _ = d_printf "!Parser mached: commentline.\n" in
   hs ^ p ^ cs ^ newline
  }
   
/* A nonempty, non-comment line. */
line: 
  hs = hspaces;
  d = sigchar;
  cs = chars;
  nl = newline
  {let l = hs ^ d ^ cs ^ nl in
   let _ = d_printf "!Parser mached: significant line %s.\n" l in
     l
  }

ignorables:
  {""}
| i = ignorables
  x = emptyline
  {i ^ x }
| i = ignorables
  x = commentline
  {i ^ x }


/* A latex environment. */
env: 
  x = ENV
  {x}  

/* A text paragraph. */
textpar: 
  x = line;
  tail = textpar_tail
  {x ^ tail}  

textpar_tail:
  el = emptyline
  {el}
| cl = commentline
  {cl}
| x = line;
  tp = textpar_tail
  { 
    x ^ tp
  } 
| x = commentline;
  tp = textpar_tail
  { 
    x ^ tp
  } 

/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/
/* Return  heading and title pair. */ 
mk_heading(kw_heading):
  h = kw_heading
  {let (heading, pval_opt) = h in 
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
     (heading, pval_f_opt) 
  }

mk_section(kw_section, nested_section):
  h = mk_heading(kw_section); 
  b = block;
  ps = paragraphs;
  ns = mk_sections(nested_section);
  {
   let (heading, pval_opt) = h in
   let _ = d_printf ("!parser: section %s matched") heading in
     heading ^ b ^ ps ^ ns
  }	  

mk_sections(my_section):
|  {""}
| ss = mk_sections(my_section); s = my_section
  {ss ^ s}

chapter:
  h = mk_heading(KW_CHAPTER); 
  b = block; 
  ps = paragraphs;
  ss = mk_sections(section); 
  EOF 
  {
   let (heading, pval_opt) = h in
   let result = heading ^ b ^ ps ^ ss in
   let _ = d_printf "Chapter mached:\n %s\n" result in
     result
  }	

section: 
  s = mk_section(KW_SECTION, subsection)
  {
   s   
  }	  

subsection: 
  s = mk_section(KW_SUBSECTION, subsubsection)
  {
   s
  }	  	

subsubsection:
  h = mk_heading(KW_SUBSUBSECTION); 
  b = block;
  ps = paragraphs;
  {
   let (heading, pval_opt) = h in
     heading ^ 
     b ^
     ps
  }	  

paragraph:  
  h = mk_heading(KW_PARAGRAPH); 
  b = block;
  {
   let _ = d_printf ("Parser matched: paragraph.\n") in
   let (heading, pval_opt) = h in
     heading ^ b 
  }	  

paragraphs:
| 
 { "" }
| p = paragraph; 
  ps = paragraphs;
  {ps ^ p}

/**********************************************************************
 ** END: Latex Sections
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Blocks
 ** A blocks is  sequence of atoms/groups followed by paragraphs
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
| ft = ignorables;
  tp = textpar;
  {let para = ft ^ "\\begin{gram}" ^ "\n" ^ tp ^ "\n" ^ "\\end{gram}" in
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
