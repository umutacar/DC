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

%token <string> COMMENT_LINE
%token <string> LINE
%token <string> EMPTY_LINE
%token <string> ENV

%token <string * string option> KW_CHAPTER
%token <string * string option> KW_SECTION
%token <string * string option> KW_SUBSECTION
%token <string * string option> KW_SUBSUBSECTION	
%token <string * string option> KW_PARAGRAPH	

%start chapter

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/

/* A nonempty line. */
line: 
  l = LINE
  {l}

/* A latex environment. */
env: 
  x = ENV
  {x}  


/* A text paragraph. */
textpar: 
  x = line;
  y = EMPTY_LINE  
  {x ^ y}  
| x = line;
  tp = textpar
  { 
    x ^ tp
  } 

comments:
  x = COMMENT_LINE
  { x }
| x = COMMENT_LINE;  
  y = comments
  { x ^ y}
| x = COMMENT_LINE;
  y = comments
  { x ^ y}

commentpar:
  x = comments;
  y = EMPTY_LINE
  { x ^ y }

commentpars:
  x = commentpar;
  { x }
| x = commentpar;
  y = commentpars;
  { x ^ y
  }

/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/
/* Return  heading and title pair. */ 
mk_heading(kw_heading):
  h = kw_heading; b = curly_box 
  {let (heading, pval_opt) = h in 
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (bo, bb, bc) = b in 
     (heading ^ bo ^ bb ^ bc, pval_f_opt, bb) 
  }

mk_section(kw_section, nested_section):
  h = mk_heading(kw_section); 
  b = block;
  ps = paragraphs;
  ns = mk_sections(nested_section);
  {
   let (heading, pval_opt, t) = h in
   let _ = d_printf ("!parser: section %s matched") heading in
     heading ^ b ^ ps ^ ns
  }	  

mk_sections(my_section):
|  {[]}
| ss = mk_sections(my_section); s = my_section
  {ss ^ s}

chapter:
  p = commentpars;
  h = mk_heading(KW_CHAPTER); 
  b = block; 
  ps = paragraphs;
  ss = mk_sections(section); 
  EOF 
  {
   let (heading, pval_opt, t) = h in
   let tt = "" in
     p ^
     heading ^
     b ^
     ps ^
     ss
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
   let (heading, pval_opt, t) = h in
     heading ^ 
     b ^
     ps
  }	  

paragraph:  
  h = mk_heading(KW_PARAGRAPH); 
  b = block;
  {
   let _ = d_printf ("Parser matched: paragraph.\n") in
   let (heading, pval_opt, t) = h in
     heading ^ b 
  }	  

paragraphs:
| 
 { [ ] }
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
  {
   let _ = d_printf ("parser matched: blocks.\n") in 
     es
  }

/**********************************************************************
 ** END: Blocks
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Elements
 ** An element is a group, a problem cluster, or an atom 
 **********************************************************************/

element:
	e = env
  {e}
| tp = textpar
  {"\\begin{gram}" ^ "\n" ^ tp ^ "\n" ^ "\\end{gram}"}

elements:
  {""}
| es = elements;
  e = element; 
  {es ^ "\n" ^ e}

/**********************************************************************
 ** END: Elements
 **********************************************************************/
