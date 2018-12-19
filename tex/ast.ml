type preamble = string
type atom_kind = string
type atom_body = string
type label_string = string
type title = string
type intertext = string
type keyword = string
(* Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", 
                "\begin{example}[title]  \n"
                "\end{example}  \n\n\n"



                  

   Because we don't care about white space, they might have whitespace in them.
 *)

(* Label is heading and the label string *)
type label = Label of keyword * label_string

type atom = Atom of preamble * (keyword * atom_kind * title option * label option * atom_body * keyword) 

type group = 
  Group of preamble 
           * (keyword * title option * label option * 
              atom list * intertext * keyword) 


type chapter = 
  Chapter of (keyword * title * label
              * block list * intertext * section list) 

and section = 
  Section of (keyword * title * label option
              * block list * intertext * subsection list)

and subsection = 
  Subsection of (keyword * title * label option
                 * block list * intertext * subsubsection list)

and subsubsection = 
  Subsubsection of (keyword * title * label option
                    * block list * intertext * paragraph list)

and paragraph = 
  Paragraph of (keyword * title * label option
                * block list * intertext)
and block = 
  | Block_Group of group
  | Block_Atom of atom

let map_concat f xs = 
  let xs_s = List.map f xs in
  let result = List.fold_left (fun result x -> result ^ x) "" xs_s in
    result

(**********************************************************************
 ** BEGIN: AST To String
*********************************************************************)
let ostrToStr os = 
  match os with 
  |  None -> ""
  |  Some s -> s

let labelToString (Label(h, label_string)) = h

let labelOptionToString lo = 
  let r = match lo with 
              |  None -> ""
              |  Some l -> labelToString l  in
     r

let titleOptionToString topt = 
  let r = match topt with 
              |  None -> ""
              |  Some s -> s in
     r

(**********************************************************************
 ** END: AST To String
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST To LaTeX
 **********************************************************************)
let atomToTex (Atom(preamble, (h_begin, kind, topt, lo, body, h_end))) = 
  let label = labelOptionToString lo in
    preamble ^ h_begin ^ label ^ body ^ h_end
      

let groupToTex (Group(preamble, (h_begin, topt, lo, ats, it, h_end))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptionToString lo in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ it ^ 
    h_end


let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a


let paragraphToTex (Paragraph (heading, t, lo, bs, it)) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptionToString lo in
    heading ^ label ^ blocks ^ it 

let subsubsectionToTex (Subsubsection (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat paragraphToTex ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let subsectionToTex (Subsection (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let sectionToTex (Section (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ nesteds

let chapterToTex (Chapter (heading, t, l, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let sections = map_concat sectionToTex ss in
  let label = labelToString l in
    heading ^ label ^ 
    blocks ^ it ^ 
    sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)





(**********************************************************************
 ** BEGIN: AST To XML
 **********************************************************************)
let atomToXml (Atom(preamble, (h_begin, kind, topt, lopt, body, h_end))) = 
  let label_xml = xml.from_label_opt lopt in
  let title_xml = xml.from_tex_title_opt topt in
  let body_xml = xml.from_tex_body body in
  let r = xml.mk_block (kind, [label_xml, title_xml, body_xml])
  
     
let groupToXml (Group(preamble, (h_begin, topt, lo, ats, it, h_end))) = 
  let atoms = map_concat atomToXml ats in
  let label = labelOptionToString lo in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ it ^ 
    h_end


let blockToXml b = 
  match b with
  | Block_Group g -> groupToXml g
  | Block_Atom a -> atomToXml a


let paragraphToXml (Paragraph (heading, t, lo, bs, it)) =
  let blocks = map_concat blockToXml bs in
  let label = labelOptionToString lo in
    heading ^ label ^ blocks ^ it 

let subsubsectionToXml (Subsubsection (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToXml bs in
  let nesteds = map_concat paragraphToXml ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let subsectionToXml (Subsection (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToXml bs in
  let nesteds = map_concat subsubsectionToXml ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let sectionToXml (Section (heading, t, lo, bs, it, ss)) =
  let blocks = map_concat blockToXml bs in
  let nesteds = map_concat subsectionToXml ss in
  let label = labelOptionToString lo in
    heading ^ label ^ 
    blocks ^ it ^ nesteds

let chapterToXml (Chapter (heading, t, l, bs, it, ss)) =
  let blocks = map_concat blockToXml bs in
  let sections = map_concat sectionToXml ss in
  let label = labelToString l in
    heading ^ label ^ 
    blocks ^ it ^ 
    sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)




