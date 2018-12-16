type preamble = string
type atom_kind = string
type atom_body = string
type title = string
type keyword = string

(* Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", 
                "\begin{example}  \n"
                "\end{example}  \n\n\n"
                  

   Because we don't care about white space, they might have whitespace in them.
 *)

type label = Label of keyword * string * keyword 
type atom = Atom of preamble * (atom_kind * title option * label option * keyword * atom_body * keyword) 
type group = Group of preamble * (title option * label option * keyword * atom list * keyword)

type chapter = 
  Chapter of  title * label option * keyword * block list * section list

and section = 
  Section of preamble * (title * label option * keyword * block list * subsection list)

and subsection = 
  Subsection of preamble * (title * label option * keyword * block list * subsubsection list)

and subsubsection = 
  Subsubsection of preamble * (title * label option * keyword * block list * paragraph list)

and paragraph = 
  Paragraph of preamble * (title * label option * keyword * block list)

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
let labelToString (Label(hb, label_string, he)) = 
  hb ^  label_string ^ he

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

let atomToString (Atom(preamble, (kind, topt, lo, hb, ab, he))) = 
  let label = "label: " ^ labelOptionToString lo in
  let heading =  match topt with
                 | None -> "Atom:" ^ hb 
                 | Some t -> "Atom:" ^ hb ^ "[" ^ t ^ "]" 
  in
    preamble ^ heading ^ label ^ ab ^ he

let groupToString (Group(preamble, (topt, lo, hb, ats, he))) = 
  let atoms = map_concat atomToString ats in
  let label = "label: " ^ labelOptionToString lo in
  let heading = match topt with
                | None -> hb
                | Some t -> hb ^ "title:" ^ t in
    preamble ^ heading ^ label ^ atoms ^ he

let blockToString b = 
  match b with
  | Block_Group g -> groupToString g
  | Block_Atom a -> atomToString a

let paragraphToString (Paragraph (preamble, (t, lo, h, bs))) =
  let blocks = map_concat blockToString bs in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    preamble ^ "*paragraph:" ^ title ^ label ^ h ^ blocks

let subsubsectionToString (Subsubsection (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat paragraphToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    preamble ^ "*subsubsection:" ^ title ^ label ^ h ^ blocks ^ nesteds

let subSectionToString (Subsection (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat subsubsectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
   preamble ^  "*subsection:" ^ title ^ label ^ h ^ blocks ^ nesteds

let sectionToString (Section (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat subSectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*section:" ^ title ^ label ^ h ^ blocks ^ nesteds

let chapterToString (Chapter (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToString bs in
  let sections = map_concat sectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*chapter:" ^ title ^ label ^ h ^ blocks ^ sections

(**********************************************************************
 ** END: AST To String
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST To LaTeX
 **********************************************************************)
let atomToTex (Atom(preamble, (kind, topt, lo, hb, ab, he))) = 
  let label = labelOptionToString lo in
  let heading =  match topt with
                 | None -> "Atom:" ^ hb 
                 | Some t -> "Atom:" ^ hb ^ "[" ^ t ^ "]" 
  in
    preamble ^ heading ^ label ^ ab ^ he
      

let groupToTex (Group(preamble, (topt, lo, hb, ats, he))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptionToString lo in
  let heading = match topt with
                | None -> hb
                | Some t -> hb ^ t in
    preamble ^ heading ^ label ^ atoms ^ he

let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a


let paragraphToTex (Paragraph (preamble, (t, lo, h, bs))) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptionToString lo in
    preamble ^ h ^ label ^ blocks

let subsubsectionToTex (Subsubsection (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat paragraphToTex ss in
  let label = labelOptionToString lo in
    preamble ^ h ^ label ^ blocks ^ nesteds

let subsectionToTex (Subsection (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptionToString lo in
    preamble ^ h ^ label ^ blocks ^ nesteds

let sectionToTex (Section (preamble, (t, lo, h, bs, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptionToString lo in
    preamble ^ h ^ label ^ blocks ^ nesteds

let chapterToTex (Chapter (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToTex bs in
  let sections = map_concat sectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ blocks ^ sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)




