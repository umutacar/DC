%{
open Core
open Printf
open Ast
open Utils

let parse_error s = printf "Parse Error: %s"
let kw_atom_definition = "definition"

let set_option_with_intertext (r, vo) = 
  match vo with 
  |	None -> ""
  |	Some (v, it) -> (r:=v; it)

let set_superblock_option (r, vo) = 
  match vo with 
  |	None -> ()
  |	Some v -> (r:=v; ())

let set_superblock_option_with_intertext (r, vo) = 
  match vo with 
  |	None -> ""
  |	Some (v, it) -> (r:=v; it)


%}	


%token EOF

%token <string> WORD
%token <string> ENV
/* ilist is kind * kw_begin * point-value option * (item-separator-keyword, item-body) list * kw_end list */
%token <string * string * (string * string * string) option * ((string * string) list) * string> ILIST

%token <string> BACKSLASH
%token <string> PERCENT  /* latex special \% */
%token <string> O_CURLY C_CURLY	
%token <string> O_SQ_BRACKET C_SQ_BRACKET
%token <string> COMMENT_LINE

%token <string * string> KW_BEGIN_ATOM KW_END_ATOM 
%token <string> KW_LABEL
%token <string * string> KW_LABEL_AND_NAME

%token <string> KW_CHAPTER
%token <string> KW_SECTION, KW_TITLED_QUESTION
%token <string> KW_SUBSECTION
%token <string> KW_SUBSUBSECTION	
%token <string> KW_PARAGRAPH	
%token <string> KW_SUBPARAGRAPH

%token <string> KW_BEGIN_CLUSTER KW_END_CLUSTER
%token <string> KW_BEGIN_GROUP KW_END_GROUP
	
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

/* I don't think ilist should have a preamble, because
   it is in inside of diderot atoms */
ilist:
| il = ILIST
  {let (kind, kw_b, arg_opt, ilist, kw_e) = il in
   let items = List.map ilist ~f:(fun (x,y) -> Ast.mk_item (x, y)) in
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
  sbso = option(superblocks_and_intertext);
  sso = option(mk_sections(nested_section));
  {
   let (heading, t) = h in
   let _ = d_printf ("!parser: section %s matched") heading in
   let sbs = ref [] in
   let ss = ref [] in
   let it = set_superblock_option_with_intertext (sbs, sbso) in
   let _ = set_superblock_option (ss, sso) in
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
  sbso = option(superblocks_and_intertext); 
  sso = option(mk_sections(section)); 
  EOF 
  {
   let (heading, t) = h in
   let sbs = ref [] in
   let ss = ref [] in
   let it = set_superblock_option_with_intertext (sbs, sbso) in
   let _ = set_superblock_option (ss, sso) in
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
  desc = mk_section(KW_SUBSUBSECTION, paragraph)
  {
     Ast.Subsubsection desc
  }	  

paragraph:  
  h = mk_heading(KW_PARAGRAPH); 
  l = option(label); 
  sbso = option(superblocks_and_intertext); 
  {
   let (heading, t) = h in
   let sbs = ref [] in
   let it = set_superblock_option_with_intertext (sbs, sbso) in
     Ast.Paragraph (heading, t, l,!sbs, it)
  }	  

/**********************************************************************
 ** END: Latex Sections
 **********************************************************************/

superblocks_and_intertext:
  xs = superblocks; intertext = boxes;
  {let _ = d_printf ("parser matched: superblocks_and_intertext.\n")  in
     (xs, intertext)
  } 

superblocks:
|	x = superblock
  {[x]}
| xs = superblocks;
  x = superblock; 
  {List.append xs [x]}

superblock:
|	b = block
  { Ast.Superblock_Block b }
| c = cluster
  { Ast.Superblock_Cluster c }
  

/**********************************************************************
 ** BEGIN: Cluster
 ** A cluster is a titled sequence of groups, and atoms 
 **********************************************************************/
cluster:
| preamble = boxes;   
  h_begin = KW_BEGIN_CLUSTER;
  l = option(label); 
  bso = option(blocks_and_intertext);
  h_end = KW_END_CLUSTER
  {
   let _ = d_printf ("!parser: cluster matched") in
   let bs = ref [] in
   let it = set_option_with_intertext (bs, bso) in
     Ast.Cluster (preamble, (h_begin, None, l, !bs, it, h_end))
  }	  
| preamble = boxes;   
  h_b = KW_BEGIN_CLUSTER;
  t = sq_box; 
  l = option(label); 
  bso = option(blocks_and_intertext);
  h_end = KW_END_CLUSTER
  {
   let _ = d_printf ("!parser: cluster matched") in
   let (bo, tt, bc) = t in
   let title_part = bo ^ tt ^ bc in
   let h_begin = h_b ^ title_part in
   let _ = d_printf ("!parser: cluster matched") in
   let bs = ref [] in
   let it = set_option_with_intertext (bs, bso) in
     Ast.Cluster (preamble, (h_begin, Some tt, l, !bs, it, h_end))
  }	  

/**********************************************************************
 ** END: Cluster
 **********************************************************************/



/**********************************************************************
 ** BEGIN: Blocks
 ** A block is a sequence of groups and atoms 
 **********************************************************************/

block:
	a = atom
  {Ast.Block_Atom a}
| g = group
  {Ast.Block_Group g}

blocks:
	b = block
  {[b]}
| bs = blocks;
  b = block; 
  {List.append bs [b]}

/* Drop intertext */
blocks_and_intertext:
  bs = blocks; intertext = boxes;
  {let _ = d_printf ("parser matched: blocks_and_intertext.\n") in
     (bs, intertext)
  } 


/**********************************************************************
 ** END: Blocks
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

/*  diderot atom */
mk_atom(kw_b, kw_e):
| preamble = boxes;
  h_b = kw_b;
  l = label;
  bs = boxes; 
  il = option(ilist); 
  h_e = kw_e;
  {
   let (kind, h_begin) = h_b in
   let (_, h_end) = h_e in
     d_printf "Parsed Atom.1 kind = %s h_begin = %s" kind h_begin;
     Atom (preamble, (kind, h_begin, None, Some l, bs, il, h_end))
  }

| preamble = boxes;
  h_b = kw_b;
  t = sq_box; 
  l = label;
  bs = boxes; 
  il = option(ilist); 
  h_e = kw_e;
  {
   let (kind, h_bb) = h_b in
   let (_, h_end) = h_e in
   let (bo, tt, bc) = t in
   let h_begin = h_bb ^ bo ^ tt ^ bc in   
     d_printf "Parsed Atom.2 kind = %s title = %s" kind tt;
     Atom (preamble, (kind, h_begin, Some tt, Some l, bs, il, h_end))
  }

| preamble = boxes;
  h_b = kw_b;
  bs = boxes_start_no_sq; 
  il = option(ilist); 
  h_e = kw_e;
  {
   let (kind, h_begin) = h_b in
   let (_, h_end) = h_e in
     d_printf "Parsed Atom.3 kind = %s h_begin = %s" kind h_begin;
     Atom (preamble, (kind, h_begin, None, None, bs, il, h_end)) 
  }

| preamble = boxes;
  h_b = kw_b;
  t = sq_box; 
  bs = boxes; 
  il = option(ilist); 
  h_e = kw_e;
  {
   let (kind, h_bb) = h_b in
   let (_, h_end) = h_e in
   let (bo, tt, bc) = t in
   let h_begin = h_bb ^ bo ^ tt ^ bc in   
     d_printf "Parsed Atom.4 kind = %s h_begin = %s title = %s" kind h_begin tt;
     Atom (preamble, (kind, h_begin, Some tt, None, bs, il, h_end))
  }

atom:
|	x = mk_atom(KW_BEGIN_ATOM, KW_END_ATOM)
  { x }


/**********************************************************************
 ** END: Atoms
 **********************************************************************/