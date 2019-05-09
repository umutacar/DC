%{
open Core
open Printf
open Ast
open Utils

let parse_error s = printf "Parse Error: %s"
let kw_atom_definition = "definition"

let set_sections_option(r, vo) = 
  match vo with 
  |	None -> ()
  |	Some v -> (r:=v; ())

let set_option_with_intertext (r, vo) = 
  match vo with 
  |	None -> ""
  |	Some (v, it) -> (r:=v; it)

let set_block_option_with_intertext (r, vo) = 
  match vo with 
  |	None -> ""
  |	Some (v, it) -> (r:=v; it)

let mk_point_val_f_opt (s: string option) = 
  match s with
  | None -> (None, "None")
  | Some x -> (Some (float_of_string x), "Some " ^ x)

%}	

%token EOF

%token <string> WORD
%token <string> ENV

/* A hint is heading, body, solution option, explain option, ending */
%token <string * string * string option * string option * (string * string)> HINT

/* A ref sol is heading, body, explain option, ending */
%token <string * string * string option * (string * string)> REFSOL 

/* ilist is 
 * kind * kw_begin * point-value option * (item-separator-keyword, point value option, item-body) list * kw_end list 
 */
%token <string * string * (string * string * string) option * ((string * string option * string) list) * string> ILIST

%token <string> BACKSLASH
%token <string> PERCENT  /* latex special \% */
%token <string> O_CURLY C_CURLY	
%token <string> O_SQ_BRACKET C_SQ_BRACKET
%token <string> COMMENT_LINE

%token <string * string * string option> KW_BEGIN_ATOM
%token <string * string> KW_END_ATOM 
%token <string> KW_LABEL
%token <string * string list * string> KW_DEPEND
%token <string * string> KW_LABEL_AND_NAME

%token <string> KW_CHAPTER
%token <string> KW_SECTION, KW_TITLED_QUESTION
%token <string> KW_SUBSECTION
%token <string> KW_SUBSUBSECTION	

/* cluster is heading and point value option */
%token <string * string option> KW_BEGIN_CLUSTER 
%token <string> KW_END_CLUSTER
%token <string> KW_BEGIN_GROUP KW_END_GROUP
%token <string> KW_BEGIN_PROBLEM_CLUSTER KW_END_PROBLEM_CLUSTER
	
%start chapter

%type <Ast.chapter> chapter
%type <Ast.section> section
%type <Ast.subsection> subsection
%type <Ast.subsubsection> subsubsection
%type <Ast.group> group
%type <Ast.atom> atom

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Words and Boxes 
 **********************************************************************/

/* A curly box looks like
   { word } 
   { [ word ] { word } ] 
   etc.  
*/
curly_box:
  o = O_CURLY;  b = boxes;  c = C_CURLY 
  {(o, b, c)}

sq_box:
  o = O_SQ_BRACKET; b = boxes;  c = C_SQ_BRACKET 
  {(o, b, c)}
 
word: 
  w = WORD 
  {w}
| x = COMMENT_LINE
  {x}
| bone = BACKSLASH btwo = BACKSLASH  /* latex special: // */
  {bone ^ btwo}
| b = BACKSLASH c = O_CURLY  /* latex special: \{ */
  {b ^ c}
| b = BACKSLASH c = C_CURLY /* latex special: \} */
  {b ^ c}
| b = BACKSLASH s = O_SQ_BRACKET  /* latex special: \[ */
  {b ^ s}
| b = BACKSLASH s = C_SQ_BRACKET /* latex special: \] */
  {b ^ s}
| x = PERCENT /* latex special: \% */
  {x}
| b = BACKSLASH w = WORD
  {b ^ w}

env: 
  x = ENV
  {x}  

blob:
  x = env
  {let _ = d_printf ("!parser: blob/env matched = %s\n.") x in
     x
  }
| x = word
  {x}

/* a box is the basic unit of composition */
box:
  x = blob
  {x}
| b = curly_box 
  {let (bo, bb, bc) = b in bo ^ bb ^ bc}
| b = sq_box 
  {let (bo, bb, bc) = b in bo ^ bb ^ bc}

boxes:
  {""}
| bs = boxes; b = box
  {bs ^ b }

boxes_start_no_sq:
  {""}
|	x = blob; bs = boxes
  {x ^ bs}
|	b = curly_box; bs = boxes
  {let (bo, bb, bc) = b in bo ^ bb ^ bc ^ bs }

/* I don't think hints should have a preamble, because
   it is in inside of diderot atoms */
hint:
| hi = HINT
  {let (h_b, body, sol_opt, exp_opt, h_e) = hi in
   let _ = d_printf ("!parser: hint matched") in     
     (h_b, body, sol_opt, exp_opt, h_e)
  }

/* I don't think solutions should have a preamble, because
   it is in inside of diderot atoms */
refsol:
| s = REFSOL
  {let (h_b, body, exp_opt, h_e) = s in
   let _ = d_printf ("!parser: refsol matched") in     
     (h_b, body, exp_opt, h_e)
  }


/* I don't think ilist should have a preamble, because
   it is in inside of diderot atoms */
ilist:
| il = ILIST
  {let (kind, kw_b, arg_opt, ilist, kw_e) = il in
   let ilist = List.map ilist ~f:(fun (x, pval, y) -> (x, fst (mk_point_val_f_opt pval), y)) in
   let items = List.map ilist ~f:(fun (x, pval, y) -> Ast.mk_item (x, pval, y)) in
   let _ = d_printf ("!parser: ilist matched") in
     match arg_opt with
     | None -> 
         Ast.IList ("", (kind, kw_b, None, items, kw_e))
     | Some (o_s, point_val, c_s) -> 
       let kw_b_all = kw_b ^ o_s ^ point_val ^ c_s in
       let point_val_f = float_of_string point_val in
         Ast.IList ("", (kind, kw_b_all, Some point_val_f, items, kw_e))        
  }

/**********************************************************************
 ** END: Words and Boxes 
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Diderot Keywords
 **********************************************************************/
/* Return full text "\label{label_string}  \n" plus label */
/*
label:
  l = KW_LABEL; b = curly_box 
  {let (bo, bb, bc) = b in 
   let h = l ^ bo ^ bb ^ bc in
     Ast.Label(h, bb)}
*/
label:
  l = KW_LABEL_AND_NAME
  {let (all, label) = l in 
   let _ = d_printf "Parser matched label = %s all = %s" label all in
     Ast.Label(all, label)}

depend:
  d = KW_DEPEND
  {let (hb, ds, he) = d in      
     Ast.Depend (hb, ds)
  }

/**********************************************************************
 ** END Diderot Keywords
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/
/* Return  heading and title pair. */ 
mk_heading(kw_heading):
  hc = kw_heading; b = curly_box 
  {let (bo, bb, bc) = b in (hc ^ bo ^ bb ^ bc, bb) }

mk_section(kw_section, nested_section):
  h = mk_heading(kw_section); 
  l = option(label); 
  sbso = option(blocks_and_intertext);
  sso = option(mk_sections(nested_section));
  {
   let (heading, t) = h in
   let _ = d_printf ("!parser: section %s matched") heading in
   let sbs = ref [] in
   let ss = ref [] in
   let it = set_block_option_with_intertext (sbs, sbso) in
   let _ = set_sections_option(ss, sso) in
     (heading, t, l, !sbs, it, !ss)
  }	  

mk_sections(my_section):
| s = my_section; {[s]}
| ss = mk_sections(my_section); s = my_section
  {List.append ss [s]}

chapter:
  preamble = boxes;
  h = mk_heading(KW_CHAPTER); 
  l = label; 
  sbso = option(blocks_and_intertext); 
  sso = option(mk_sections(section)); 
  EOF 
  {
   let (heading, t) = h in
   let sbs = ref [] in
   let ss = ref [] in
   let it = set_block_option_with_intertext (sbs, sbso) in
   let _ = set_sections_option(ss, sso) in
     Ast.Chapter(preamble, (heading, t, l, !sbs, it, !ss))
  }	

section: 
  desc = mk_section(KW_SECTION, subsection)
  {
     Ast.Section desc
  }	  
| desc = mk_section(KW_TITLED_QUESTION, subsection)
  {
     Ast.Section desc
  }	  

subsection: 
  desc = mk_section(KW_SUBSECTION, subsubsection)
  {
     Ast.Subsection desc
  }	  
	

subsubsection:
  h = mk_heading(KW_SUBSUBSECTION); 
  l = option(label); 
  sbso = option(blocks_and_intertext); 
  {
   let (heading, t) = h in
   let sbs = ref [] in
   let it = set_block_option_with_intertext (sbs, sbso) in
     Ast.Subsubsection (heading, t, l,!sbs, it)
  }	  

/*
paragraph:  
  h = mk_heading(KW_PARAGRAPH); 
  l = option(label); 
  sbso = option(blocks_and_intertext); 
  {
   let (heading, t) = h in
   let sbs = ref [] in
   let it = set_block_option_with_intertext (sbs, sbso) in
     Ast.Paragraph (heading, t, l,!sbs, it)
  }	  
*/
/**********************************************************************
 ** END: Latex Sections
 **********************************************************************/

blocks_and_intertext:
  xs = blocks; intertext = boxes;
  {let _ = d_printf ("parser matched: blocks_and_intertext.\n")  in
     (xs, intertext)
  } 

blocks:
|	x = block
  {[x]}
| xs = blocks;
  x = block; 
  {List.append xs [x]}

block:
|	e = element
  { Ast.Block_Block e }
| c = cluster
  { Ast.Block_Cluster c }
  
/**********************************************************************
 ** BEGIN: Cluster
 ** A cluster is a titled sequence of groups, and atoms 
 **********************************************************************/

cluster:
| preamble = boxes;   
  h_begin = KW_BEGIN_CLUSTER;
  l = option(label); 
  bso = option(elements_and_intertext);
  h_end = KW_END_CLUSTER
  {
   let _ = d_printf ("!parser: cluster matched") in
   let (h_begin, pval_opt) = h_begin in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let bs = ref [] in
   let it = set_option_with_intertext (bs, bso) in
     Ast.Cluster (preamble, (h_begin, pval_f_opt, None, l, !bs, it, h_end))
  }	  
| preamble = boxes;   
  h_b = KW_BEGIN_CLUSTER;
  t = sq_box; 
  l = option(label); 
  bso = option(elements_and_intertext);
  h_end = KW_END_CLUSTER
  {
   let _ = d_printf ("!parser: cluster matched") in
   let (h_b, pval_opt) = h_b in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (bo, tt, bc) = t in
   let title_part = bo ^ tt ^ bc in
   let h_begin = h_b ^ title_part in
   let _ = d_printf ("!parser: cluster matched") in
   let bs = ref [] in
   let it = set_option_with_intertext (bs, bso) in
     Ast.Cluster (preamble, (h_begin, pval_f_opt, Some tt, l, !bs, it, h_end))
  }	  

/**********************************************************************
 ** END: Cluster
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Elements
 ** An element is a groups and atoms 
 **********************************************************************/

element:
	a = atom
  {Ast.Element_Atom a}
| pc = problemcluster
  {Ast.Element_ProblemCluster pc}
| g = group
  {Ast.Element_Group g}

elements:
	e = element
  {[e]}
| es = elements;
  e = element; 
  {List.append es [e]}

/* Drop intertext */
elements_and_intertext:
  es = elements; intertext = boxes;
  {let _ = d_printf ("parser matched: elements_and_intertext.\n") in
     (es, intertext)
  } 


/**********************************************************************
 ** END: Elements
 **********************************************************************/
			
/**********************************************************************
 ** BEGIN: Groups
 **********************************************************************/

end_group:
  he = KW_END_GROUP
  {he}

/* There is a shift reduce conflict here but it doesn't shifting does 
   the right thing. 
*/   
group:
| preamble = boxes;   
  h_begin = KW_BEGIN_GROUP; 
  l = option(label); 
  ats_it = atoms_and_intertext; 
  h_end = end_group
  {let (ats, it) = ats_it in
     Ast.Group (preamble, (h_begin, None, l, ats, it, h_end))
  }

| preamble = boxes; 
  hb = KW_BEGIN_GROUP;
  t = sq_box; 
  l = option(label); 
  ats_it = atoms_and_intertext; 
  h_end = end_group;
  {let (bo, tt, bc) = t in
   let title_part = bo ^ tt ^ bc in
   let h_begin = hb ^ title_part in
   let (ats, it) = ats_it in
     Ast.Group (preamble, (h_begin, Some tt, l, ats, it, h_end))
  }

/**********************************************************************
 ** END: Groups
 **********************************************************************/

/**********************************************************************
 ** BEGIN: Atoms
 **********************************************************************/

atoms:
  {[]}		
| ats = atoms; a = atom
  { List.append ats [a] }

/* Drop intertext */
atoms_and_intertext:
  ats = atoms; it = boxes;
  {(ats, it)}			

mk_atom_tail (kw_e):
|  il = option(ilist); 
   h_e = kw_e
  { (il, None, None, None, None, h_e) }
| il = option(ilist); 
  s = refsol
  {let (h_b, body, exp_opt, h_e) = s in
     (il, Some h_b, None, Some body, exp_opt, h_e)
  }
| il = option(ilist); 
  s = hint
  {let (h_b, body, sol_opt, exp_opt, h_e) = s in
     (il, Some h_b, Some body, sol_opt, exp_opt, h_e)
  }


/*  diderot atom */


mk_atom(kw_b, kw_e):

/* atoms with label */
| preamble = boxes;
  h_b = kw_b;
  topt = option (sq_box)
  l = label;
  dopt = option (depend);
  bs = boxes; 
  tail = mk_atom_tail (kw_e)
  {
   let (kind, h_bb, pval_opt) = h_b in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (il, _, hint, sol, exp, h_e) = tail in
   let (_, h_end) = h_e in
     match topt with 
     | None -> 
       let h_begin = h_bb in
       let _ = d_printf "\n \n Parsed Atom.1 kind = %s h_begin = %s pval_f_opt = %s " kind h_begin pval_opt_str in
         Atom (preamble, (kind, h_begin, pval_f_opt, None, Some l, dopt, bs, il, hint, sol, exp, h_end))
     | Some t ->
       let (bo, tt, bc) = t in
       let h_begin = h_bb ^ bo ^ tt ^ bc in   
       let _ = d_printf "\n Parsed Atom.2 kind = %s title = %s pval_f_opt = %s " kind tt pval_opt_str in
         Atom (preamble, (kind, h_begin, pval_f_opt, Some tt, Some l, dopt, bs, il, hint, sol, exp, h_end))
  }

/* atoms without labels but with depends, may have titles or not */
| preamble = boxes;
  h_b = kw_b;
  topt = option (sq_box);
  d = depend;
  bs = boxes;
  tail = mk_atom_tail (kw_e)
  {
   let (kind, h_bb, pval_opt) = h_b in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (il, _, hint, sol, exp, h_e) = tail in
   let (_, h_end) = h_e in
     match topt with 
     | None -> 
       let h_begin = h_bb in
       let _ = d_printf "\n \n Parsed Atom.3 kind = %s h_begin = %s pval_f_opt = %s " kind h_begin pval_opt_str in
         Atom (preamble, (kind, h_begin, pval_f_opt, None, None, Some d, bs, il, hint, sol, exp, h_end))
     | Some t ->
       let (bo, tt, bc) = t in
       let h_begin = h_bb ^ bo ^ tt ^ bc in   
       let _ = d_printf "\n Parsed Atom.4 kind = %s title = %s pval_f_opt = %s " kind tt pval_opt_str in
         Atom (preamble, (kind, h_begin, pval_f_opt, Some tt, None, Some d, bs, il, hint, sol, exp, h_end))
  }

/* atoms without labels, without depends, without titles */
| preamble = boxes;
  h_b = kw_b;
  bs = boxes_start_no_sq; 
  tail = mk_atom_tail (kw_e)
  {
   let (kind, h_begin, pval_opt) = h_b in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (il, _, hint, sol, exp, h_e) = tail in
   let (_, h_end) = h_e in
     d_printf "\n Parsed Atom.5 kind = %s h_begin = %s pval_f_opt = %s " kind h_begin pval_opt_str;

     Atom (preamble, (kind, h_begin, pval_f_opt, None, None, None, bs, il, hint, sol, exp, h_end)) 
  }

/* atoms without labels, without depends, with titles */
| preamble = boxes;
  h_b = kw_b;
  t = sq_box; 
  bs = boxes; 
  tail = mk_atom_tail (kw_e)
  {
   let (kind, h_bb, pval_opt) = h_b in
   let (pval_f_opt, pval_opt_str) = mk_point_val_f_opt pval_opt in
   let (il, _, hint, sol, exp, h_e) = tail in
   let (_, h_end) = h_e in
   let (bo, tt, bc) = t in
   let h_begin = h_bb ^ bo ^ tt ^ bc in   
     d_printf "\n Parsed Atom.6 kind = %s h_begin = %s title = %s pval_f_opt = %s " kind h_begin tt pval_opt_str;
     Atom (preamble, (kind, h_begin, pval_f_opt, Some tt, None, None, bs, il, hint, sol, exp, h_end))
  }

atom:
|	x = mk_atom(KW_BEGIN_ATOM, KW_END_ATOM)
  { x }


/**********************************************************************
 ** END: Atoms
 **********************************************************************/