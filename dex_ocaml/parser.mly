%{
open Core
open Printf
open Ast

let parse_error s = printf "Parse Error: %s"
let kw_atom_definition = "definition"
let set_option (r, vo) = 
  match vo with 
  |	None -> ()
  |	Some v -> (r:=v; ())
%}	


%token EOF

%token <string> ATOM
%token <string> WORD

%token <string> BACKSLASH
%token <string> O_CURLY C_CURLY	
%token <string> O_SQ_BRACKET C_SQ_BRACKET
%token <string> COMMENT_LINE

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
%token <string> KW_BEGIN_GRAM KW_END_GRAM
%token <string> KW_BEGIN_HINT KW_END_HINT
%token <string> KW_BEGIN_IMPORTANT KW_END_IMPORTANT
%token <string> KW_BEGIN_LEMMA KW_END_LEMMA
%token <string> KW_BEGIN_NOTE KW_END_NOTE
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

%token <string> KW_BEGIN_GROUP KW_END_GROUP
	
%start chapter
%{ open Atoms %}

%type <Ast.chapter> chapter
%type <Ast.section> section
%type <Ast.subsection> subsection
%type <Ast.subsubsection> subsubsection
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
| x = COMMENT_LINE
  {x}
| bone = BACKSLASH btwo = BACKSLASH  /* latex special: // */
  {bone ^ btwo}
| b = BACKSLASH s = O_SQ_BRACKET  /* latex special: \[ */
  {b ^ s}
| b = BACKSLASH s = C_SQ_BRACKET /* latex special: \] */
  {b ^ s}
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
 ** BEGIN: Comments
 **********************************************************************/
comment:
  x = COMMENT_LINE
  {x}
| x = COMMENT_LINE; y = comment
  {x ^ y}

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
mk_heading(kw_heading):
  hc = kw_heading; b = curly_box 
  {let (bo, bb, bc) = b in (hc ^ bo ^ bb ^ bc, bb) }

mk_sections(my_section):
| s = my_section; {[s]}
| ss = mk_sections(my_section); s = my_section
  {List.append ss [s]}

mk_section(kw_section, nested_section):
  h = mk_heading(kw_section); 
  l = option(label); 
  bso = option(blocks); 
  sso = option(mk_sections(nested_section));
  {
   let (hc, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let _ = set_option (bs, bso) in
   let _ = set_option (ss, sso) in
     (t, l, hc, !bs, !ss)
  }	  


chapter:
  h = mk_heading(KW_CHAPTER); 
  l = option(label); 
  bso = option(blocks); 
  sso = option(mk_sections(section)); 
  EOF 
  {
   let (hc, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let _ = set_option (bs, bso) in
   let _ = set_option (ss, sso) in
     Ast.Chapter(t, l, hc, !bs, !ss)
  }	


section: 
  args = mk_section(KW_SECTION, subsection)
  {
   let (t, l, hc, bs, ss) = args in
     Ast.Section(t, l, hc, bs, ss)
  }	  

subsection: 
  args = mk_section(KW_SUBSECTION, subsubsection)
  {
   let (t, l, hc, bs, ss) = args in
     Ast.Subsection(t, l, hc, bs, ss)
  }	  
	
subsubsection: 
  args = mk_section(KW_SUBSUBSECTION, paragraph)
  {
   let (t, l, hc, bs, ss) = args in
     Ast.Subsubsection(t, l, hc, bs, ss)
  }	  

paragraph: 
  h = mk_heading(KW_PARAGRAPH); 
  l = option(label); 
  bso = option(blocks); 
  {
   let (hc, t) = h in
   let bs = ref [] in
   let _ = set_option (bs, bso) in
     Ast.Paragraph(t, l, hc, !bs)
  }	  
	

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

begin_group:
  hb = KW_BEGIN_GROUP
  {hb}

begin_group_sq:
  hb = KW_BEGIN_GROUP; b = sq_box
  {let (bo, bb, bc) = b in (hb ^ bo ^ bb ^ bc, bb)}

end_group:
  he = KW_END_GROUP
  {he}

/* There is a shift reduce conflict here but it doesn't shifting does 
   the right thing. 
*/   
group:
| preamble = boxes;   
  hb = KW_BEGIN_GROUP; 
  l = option(label); 
  ats = atoms; 
  he = end_group
  {Ast.Group (preamble, (None, l, hb, ats, he))}

| preamble = boxes; 
  hb = KW_BEGIN_GROUP;
  t = sq_box; 
  l = option(label); 
  ats = atoms; 
  he = end_group;
  {let (bo, tt, bc) = t in
   let title_part = bo ^ tt ^ bc in
     Ast.Group (preamble, (Some tt, l, hb ^ title_part, ats, he))}
/*
| hb = begin_group_sq; l = option(label); ats = atoms; he = end_group
  {let (hb, t) = hb in Ast.Group (Some t, l, hb, ats, he)}
*/

atoms:
  {[]}		
| ats = atoms; a = atom
  { List.append ats [a] }
	

atom_(kw_b, kw_e):
| preamble = boxes;
  b = kw_b;
  l = label;
  bs = boxes; 
  e = kw_e;
  {
   printf "Parsed Atom %s" b;
   Atom (preamble, (b, None, Some l, b, bs, e))
  }

| preamble = boxes;
  b = kw_b;
  t = sq_box; 
  l = label;
  bs = boxes; 
  e = kw_e;
  {
   let (bo, tt, bc) = t in
     printf "Parsed Atom %s title = %s" b tt;
     Atom (preamble, (b, Some tt, Some l, b, bs, e))
  }

| preamble = boxes;
  b = kw_b;
  bs = boxes_start_no_sq; 
  e = kw_e;
  {
   printf "Parsed Atom %s" b;
   Atom (preamble, (b, None, None, b, bs, e)) 
  }

| preamble = boxes;
  b = kw_b;
  t = sq_box; 
  bs = boxes; 
  e = kw_e;
  {
   let (bo, tt, bc) = t in
     printf "Parsed Atom %s title = %s" b tt;
     Atom (preamble, (b, Some tt, None, b, bs, e))
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
|	x = atom_(KW_BEGIN_DATASTR, KW_END_DATASTR)
  { x }
|	x = atom_(KW_BEGIN_DATATYPE, KW_END_DATATYPE)
  { x }
|	x = atom_(KW_BEGIN_COSTSPEC, KW_END_COSTSPEC)
  { x }
|	x = atom_(KW_BEGIN_DEFINITION, KW_END_DEFINITION)
  { x }
|	x = atom_(KW_BEGIN_EXAMPLE, KW_END_EXAMPLE)
  { x }
|	x = atom_(KW_BEGIN_EXERCISE, KW_END_EXERCISE)
  { x }
|	x = atom_(KW_BEGIN_GRAM, KW_END_GRAM)
  { x }
|	x = atom_(KW_BEGIN_HINT, KW_END_HINT)
  { x }
|	x = atom_(KW_BEGIN_IMPORTANT, KW_END_IMPORTANT)
  { x }
|	x = atom_(KW_BEGIN_LEMMA, KW_END_LEMMA)
  { x }
|	x = atom_(KW_BEGIN_NOTE, KW_END_NOTE)
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

/*

section: 
  h = mk_heading(KW_SECTION); 
  l = option(label); 
  bso = option(blocks); 
  sso = option(mk_sections(subsection));
  {
   let (hc, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let _ = set_option (bs, bso) in
   let _ = set_option (ss, sso) in
     Ast.Section(t, l, hc, !bs, !ss)
  }	  

subsection: 
  h = mk_heading(KW_SUBSECTION); 
  l = option(label); 
  bso = option(blocks); 
  sso = option(mk_sections(subsubsection));
  {
   let (hc, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let _ = set_option (bs, bso) in
   let _ = set_option (ss, sso) in
     Ast.Subsection(t, l, hc, !bs, !ss)
  }	  
	

subsubsection: 
  h = mk_heading(KW_SUBSUBSECTION); 
  l = option(label); 
  bso = option(blocks); 
  {
   let (hc, t) = h in
   let bs = ref [] in
   let _ = set_option (bs, bso) in
     Ast.Subsubsection(t, l, hc, !bs)
  }	  

*/