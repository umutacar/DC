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

let set_option_with_intertext (r, vo) = 
  match vo with 
  |	None -> ""
  |	Some (v, it) -> (r:=v; it)
%}	


%token EOF

%token <string> WORD

%token <string> BACKSLASH
%token <string> PERCENT  /* latex special \% */
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
%token <string> KW_BEGIN_TEACHNOTE KW_END_TEACHNOTE
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


%inline algorithm: { "algorithm" }
%inline code: { "code" }
%inline corollary: { "corollary" }
%inline costspec: { "costspec" }
%inline datastr: { "datastr" }
%inline datatype: { "datatype" }
%inline definition: { "definition" }
%inline example: { "example" }
%inline exercise: { "exercise" }
%inline gram: { "gram" }
%inline hint: { "hint" }
%inline important: { "important" }
%inline lemma: { "lemma" }
%inline note: { "note" }
%inline preamble: { "preamble" }
%inline problem: { "problem" }
%inline proof: { "proof" }
%inline proposition: { "proposition" }
%inline remark: { "remark" }
%inline solution: { "solution" }
%inline syntax: { "syntax" }
%inline teachask: { "teachask" }
%inline teachnote: { "teachnote" }
%inline theorem: { "theorem" }


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
| l = KW_LABEL; b = curly_box  /* label can appear inside bodies */
  {let (bo, bb, bc) = b in 
     l ^ bo ^ bb ^ bc
  }
| b = BACKSLASH w = WORD
  {b ^ w}

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
/* Return full text "\label{label_string}  \n" plus label */
label:
  l = KW_LABEL; b = curly_box 
  {let (bo, bb, bc) = b in 
   let h = l ^ bo ^ bb ^ bc in
     Ast.Label(h, bb)}


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
  bso = option(blocks_and_intertext);
  sso = option(mk_sections(nested_section));
  {
   let (heading, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let it = set_option_with_intertext (bs, bso) in
   let _ = set_option (ss, sso) in
     (heading, t, l, !bs, it, !ss)
  }	  

mk_sections(my_section):
| s = my_section; {[s]}
| ss = mk_sections(my_section); s = my_section
  {List.append ss [s]}


chapter:
  h = mk_heading(KW_CHAPTER); 
  l = label; 
  bso = option(blocks_and_intertext); 
  sso = option(mk_sections(section)); 
  EOF 
  {
   let (heading, t) = h in
   let bs = ref [] in
   let ss = ref [] in
   let it = set_option_with_intertext (bs, bso) in
   let _ = set_option (ss, sso) in
     Ast.Chapter(heading, t, l, !bs, it, !ss)
  }	


section: 
  desc = mk_section(KW_SECTION, subsection)
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
  bso = option(blocks_and_intertext); 
  {
   let (heading, t) = h in
   let bs = ref [] in
   let it = set_option_with_intertext (bs, bso) in
     Ast.Paragraph (heading, t, l,!bs, it)
  }	  
	

/**********************************************************************
 ** BEGIN: Latex Sections
 **********************************************************************/

/* BEGIN: Blocks
 * A blocks is a sequence of groups and atoms 
 */

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
  {(bs, intertext)} 
/* END: Blocks */

			
/* BEGIN: Groups and atoms */

begin_group:
  hb = KW_BEGIN_GROUP
  {hb}

/* Return the pair of full heading and title. */
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

atoms:
  {[]}		
| ats = atoms; a = atom
  { List.append ats [a] }

/* Drop intertext */
atoms_and_intertext:
  ats = atoms; it = boxes;
  {(ats, it)}		
	

mk_atom(kind, kw_b, kw_e):
| kind = kind
  preamble = boxes;
  h_b = kw_b;
  l = label;
  bs = boxes; 
  h_end = kw_e;
  {
   let h_begin = h_b in
     printf "Parsed Atom %s!" h_begin;
     Atom (preamble, (kind, h_begin, None, Some l, bs, h_end))
  }

| kind = kind
  preamble = boxes;
  h_b = kw_b;
  t = sq_box; 
  l = label;
  bs = boxes; 
  h_end = kw_e;
  {
   let (bo, tt, bc) = t in
   let h_begin = h_b ^ bo ^ tt ^ bc in   
     printf "Parsed Atom %s title = %s" h_begin tt;
     Atom (preamble, (kind, h_begin, Some tt, Some l, bs, h_end))
  }

| kind = kind
  preamble = boxes;
  h_b = kw_b;
  bs = boxes_start_no_sq; 
  h_end = kw_e;
  {
   let h_begin = h_b in
     printf "Parsed Atom %s" h_begin;
     Atom (preamble, (kind, h_begin, None, None, bs, h_end)) 
  }

| kind = kind
  preamble = boxes;
  h_b = kw_b;
  t = sq_box; 
  bs = boxes; 
  h_end = kw_e;
  {
   let (bo, tt, bc) = t in
   let h_begin = h_b ^ bo ^ tt ^ bc in   
     printf "Parsed Atom %s title = %s" h_begin tt;
     Atom (preamble, (kind, h_begin, Some tt, None, bs, h_end))
  }

atom:
|	x = mk_atom(algorithm, KW_BEGIN_ALGORITHM, KW_END_ALGORITHM)
  { x }
|	x = mk_atom(code, KW_BEGIN_CODE, KW_END_CODE)
  { x }
|	x = mk_atom(corollary, KW_BEGIN_COROLLARY, KW_END_COROLLARY)
  { x }
|	x = mk_atom(datastr, KW_BEGIN_DATASTR, KW_END_DATASTR)
  { x }
|	x = mk_atom(datatype, KW_BEGIN_DATATYPE, KW_END_DATATYPE)
  { x }
|	x = mk_atom(costspec, KW_BEGIN_COSTSPEC, KW_END_COSTSPEC)
  { x }
|	x = mk_atom(definition, KW_BEGIN_DEFINITION, KW_END_DEFINITION)
  { x }
|	x = mk_atom(example, KW_BEGIN_EXAMPLE, KW_END_EXAMPLE)
  { x }
|	x = mk_atom(exercise, KW_BEGIN_EXERCISE, KW_END_EXERCISE)
  { x }
|	x = mk_atom(gram, KW_BEGIN_GRAM, KW_END_GRAM)
  { x }
|	x = mk_atom(hint, KW_BEGIN_HINT, KW_END_HINT)
  { x }
|	x = mk_atom(important, KW_BEGIN_IMPORTANT, KW_END_IMPORTANT)
  { x }
|	x = mk_atom(lemma, KW_BEGIN_LEMMA, KW_END_LEMMA)
  { x }
|	x = mk_atom(note, KW_BEGIN_NOTE, KW_END_NOTE)
  { x }
|	x = mk_atom(preamble, KW_BEGIN_PREAMBLE, KW_END_PREAMBLE)
  { x }
|	x = mk_atom(problem, KW_BEGIN_PROBLEM, KW_END_PROBLEM)
  { x }
|	x = mk_atom(proof, KW_BEGIN_PROOF, KW_END_PROOF)
  { x }
|	x = mk_atom(proposition, KW_BEGIN_PROPOSITION, KW_END_PROPOSITION)
  { x }
|	x = mk_atom(remark, KW_BEGIN_REMARK, KW_END_REMARK)
  { x }
|	x = mk_atom(solution, KW_BEGIN_SOLUTION, KW_END_SOLUTION)
  { x }
|	x = mk_atom(syntax, KW_BEGIN_SYNTAX, KW_END_SYNTAX)
  { x }
|	x = mk_atom(teachask, KW_BEGIN_TEACHASK, KW_END_TEACHASK)
  { x }
|	x = mk_atom(teachnote, KW_BEGIN_TEACHNOTE, KW_END_TEACHNOTE)
  { x }
|	x = mk_atom(theorem, KW_BEGIN_THEOREM, KW_END_THEOREM)
  { x }

