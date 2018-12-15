%{
open Core
open Printf
open Ast

let parse_error s = printf "Parse Error: %s"
let kw_atom_definition = "definition"
%}	


%token EOF

%token <string> ATOM
%token <string> WORD

%token <string> BACKSLASH
%token <string> O_CURLY C_CURLY	
%token <string> O_SQ_BRACKET C_SQ_BRACKET

%token <string> KW_BEGIN KW_END	
%token <string> KW_BEGIN_ALGORITHM KW_END_ALGORITHM	
%token <string> KW_BEGIN_CODE KW_END_CODE
%token <string> KW_BEGIN_COROLLARY KW_END_COROLLARY
%token <string> KW_BEGIN_COSTSPEC KW_END_COSTSPEC
%token <string> KW_BEGIN_DATATYPE KW_END_DATATYPE	
%token <string> KW_BEGIN_DATASTR KW_END_DATASTR	
%token <string> KW_BEGIN_DEFINITION KW_END_DEFINITION	
%token <string> KW_BEGIN_EXAMPLE KW_END_EXAMPLE
%token <string> KW_BEGIN_EXERCISE KW_END_EXERCISE
%token <string> KW_BEGIN_HINT KW_END_HINT
%token <string> KW_BEGIN_IMPORTANT KW_END_IMPORTANT
%token <string> KW_BEGIN_LEMMA KW_END_LEMMA
%token <string> KW_BEGIN_NOTE KW_END_NOTE
%token <string> KW_BEGIN_PARAGRAPH KW_END_PARAGRAPH
%token <string> KW_BEGIN_PREAMBLE KW_END_PREAMBLE
%token <string> KW_BEGIN_PROBLEM KW_END_PROBLEM
%token <string> KW_BEGIN_PROOF KW_END_PROOF
%token <string> KW_BEGIN_PROPOSITION KW_END_PROPOSITION
%token <string> KW_BEGIN_REMARK KW_END_REMARK
%token <string> KW_BEGIN_SOLUTION KW_END_SOLUTION
%token <string> KW_BEGIN_SYNTAX KW_END_SYNTAX
%token <string> KW_BEGIN_TEACHASK KW_END_TEACHASK
%token <string> KW_BEGIN_THEOREM KW_END_THEOREM
	
%token <string> KW_LABEL

%token <string> KW_CHAPTER
%token <string> KW_SECTION
%token <string> KW_SUBSECTION
%token <string> KW_SUBSUBSECTION	
%token <string> KW_PARAGRAPH	
%token <string> KW_SUBPARAGRAPH

%token <string> ENV_B_GROUP ENV_E_GROUP
	
%start chapter
%{ open Atoms %}

%type <Ast.chapter> chapter
%type <Ast.section> section
%type <Ast.section List.t> sections
%type <Ast.group> group
%type <Ast.atom> atom

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
| b = BACKSLASH w = WORD
  {b ^ w}
| kwb = KW_BEGIN; co = O_CURLY; w = WORD; cc = C_CURLY 
  {kwb ^ co ^ w ^ cc}
| kwe = KW_END; co = O_CURLY; w = WORD; cc = C_CURLY 
  {kwe ^ co ^ w ^ cc}

/* a box is the basic unit of composition */
box:
  w = word 
  {w}
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
|	w = word; bs = boxes
  {w ^ bs}
|	b = curly_box; bs = boxes
  {let (bo, bb, bc) = b in bo ^ bb ^ bc ^ bs }

/**********************************************************************
 ** END: Words and Boxes 
 **********************************************************************/


/**********************************************************************
 ** BEGIN: Diderot Keywords
 **********************************************************************/
label:
  l = KW_LABEL; b = curly_box 
  {let (bo, bb, bc) = b in Ast.Label(l ^ bo, bb, bc) }


/**********************************************************************
 ** END Diderot Keywords
 **********************************************************************/


/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/
chapter_heading:
  hc = KW_CHAPTER; b = curly_box 
  {let (bo, bb, bc) = b in (hc ^ bo ^ bb ^ bc, bb) }

section_heading:
  hs = KW_SECTION; b = curly_box 
  {let (bo, bb, bc) = b in (hs ^ bo ^ bb ^ bc, bb) }

chapter:
  h = chapter_heading; l = option(label); bs = blocks; ss = sections; EOF 
  {let (hc, t) = h in Ast.Chapter(t, l, hc, bs, ss)}
|	h =  chapter_heading; l = option(label); ss = sections; EOF
  {let (hc, t) = h in Ast.Chapter (t, l, hc, [], ss)}		

section: 
  h = section_heading; l = option(label); bs = blocks
  {let (hc, t) = h in Ast.Section(t, l, hc, bs)}		
	
sections:
| s = section; {[s]}
| ss = sections; s = section
  {List.append ss [s]}

/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/

/* BEGIN: Blocks
 * A blocks is a sequence of groups and atoms 
 */
blocks:
	b = block 
  {[b]}
| bs = blocks; b = block
  {List.append bs [b]}

block:
	a = atom
  {Ast.Block_Atom a}
| g = group
  {Ast.Block_Group g}
/* END: Blocks */

			
/* BEGIN: Groups and atoms */

env_b_group:
  hb = ENV_B_GROUP
  {hb}

env_b_group_sq:
  hb = ENV_B_GROUP; b = sq_box
  {let (bo, bb, bc) = b in (hb ^ bo ^ bb ^ bc, bb)}

env_e_group:
  he = ENV_E_GROUP
  {he}

group:
  hb = env_b_group; l = option(label); ats = atoms; he = env_e_group
  {Ast.Group (None, l, hb, ats, he)}
| hb = env_b_group_sq; l = option(label); ats = atoms; he = env_e_group
  {let (hb, t) = hb in Ast.Group (Some t, l, hb, ats, he)}

atoms:
  {[]}		
| ats = atoms; a = atom
  { List.append ats [a] }
	

atom_(kw_b, kw_e):
| b = kw_b;
  l = label;
  bs = boxes; 
  e = kw_e
  {
   Atom (b, None, Some l, b, bs, e) 
  }

| b = kw_b;
  t = sq_box; 
  l = label;
  bs = boxes; 
  e = kw_e
  {
   let (bo, tt, bc) = t in
     Atom (b, Some tt, Some l, b, bs, e) 
  }

| b = kw_b;
  bs = boxes_start_no_sq; 
  e = kw_e
  {
   Atom (b, None, None, b, bs, e) 
  }

| b = kw_b;
  t = sq_box; 
  bs = boxes; 
  e = kw_e
  {
   let (bo, tt, bc) = t in
     Atom (b, Some tt, None, b, bs, e) 
  }

/*
|	x = atom_(KW_BEGIN_xxx, KW_END_xxx)
  { x }
*/
atom:
|	x = atom_(KW_BEGIN_ALGORITHM, KW_END_ALGORITHM)
  { x }
|	x = atom_(KW_BEGIN_CODE, KW_END_CODE)
  { x }
|	x = atom_(KW_BEGIN_COROLLARY, KW_END_COROLLARY)
  { x }
|	x = atom_(KW_BEGIN_COSTSPEC, KW_END_COSTSPEC)
  { x }
|	x = atom_(KW_BEGIN_DEFINITION, KW_END_DEFINITION)
  { x }
|	x = atom_(KW_BEGIN_EXAMPLE, KW_END_EXAMPLE)
  { x }
|	x = atom_(KW_BEGIN_EXERCISE, KW_END_EXERCISE)
  { x }
|	x = atom_(KW_BEGIN_HINT, KW_END_HINT)
  { x }
|	x = atom_(KW_BEGIN_IMPORTANT, KW_END_IMPORTANT)
  { x }
|	x = atom_(KW_BEGIN_LEMMA, KW_END_LEMMA)
  { x }
|	x = atom_(KW_BEGIN_NOTE, KW_END_NOTE)
  { x }
|	x = atom_(KW_BEGIN_PARAGRAPH, KW_END_PARAGRAPH)
  { x }
|	x = atom_(KW_BEGIN_PREAMBLE, KW_END_PREAMBLE)
  { x }
|	x = atom_(KW_BEGIN_PROBLEM, KW_END_PROBLEM)
  { x }
|	x = atom_(KW_BEGIN_PROOF, KW_END_PROOF)
  { x }
|	x = atom_(KW_BEGIN_PROPOSITION, KW_END_PROPOSITION)
  { x }
|	x = atom_(KW_BEGIN_REMARK, KW_END_REMARK)
  { x }
|	x = atom_(KW_BEGIN_SOLUTION, KW_END_SOLUTION)
  { x }
|	x = atom_(KW_BEGIN_SYNTAX, KW_END_SYNTAX)
  { x }
|	x = atom_(KW_BEGIN_TEACHASK, KW_END_TEACHASK)
  { x }
|	x = atom_(KW_BEGIN_THEOREM, KW_END_THEOREM)
  { x }

