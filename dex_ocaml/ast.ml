type preamble = string
type atom_kind = string
type atom_body = string
type title = string
type intertext = string option
type keyword = string

(* Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", 
                "\begin{example}  \n"
                "\end{example}  \n\n\n"
                  

   Because we don't care about white space, they might have whitespace in them.
 *)

type label = Label of keyword * string * keyword 
type atom = Atom of preamble * (atom_kind * title option * label option * keyword * atom_body * keyword) 

type group = 
  Group of preamble 
           * (title option * label option * keyword 
              * atom list * intertext * keyword) 


type chapter = 
  Chapter of (title * label * keyword 
              * block list * intertext * section list) 

and section = 
  Section of (title * label option * keyword 
              * block list * intertext * subsection list)

and subsection = 
  Subsection of (title * label option * keyword 
                 * block list * intertext * subsubsection list)

and subsubsection = 
  Subsubsection of (title * label option * keyword 
                    * block list * intertext * paragraph list)

and paragraph = 
  Paragraph of (title * label option * keyword 
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

let groupToString (Group(preamble, (topt, lo, hb, ats, it, he))) = 
  let atoms = map_concat atomToString ats in
  let label = "label: " ^ labelOptionToString lo in
  let heading = match topt with
                | None -> hb
                | Some t -> hb ^ "title:" ^ t in
    preamble ^ 
    heading ^ label ^ 
    atoms ^ (ostrToStr it) ^
    he  


let blockToString b = 
  match b with
  | Block_Group g -> groupToString g
  | Block_Atom a -> atomToString a

let paragraphToString (Paragraph (t, lo, h, bs, it)) =
  let blocks = map_concat blockToString bs in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*paragraph:" ^ title ^ label ^ h ^
    blocks ^ (ostrToStr it) 


let subsubsectionToString (Subsubsection (t, lo, h, bs, it, ss)) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat paragraphToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*subsubsection:" ^ title ^ label ^ h 
                      ^ blocks ^ (ostrToStr it) 
                      ^ nesteds 

let subSectionToString (Subsection (t, lo, h, bs, it, ss)) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat subsubsectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
   "*subsection:" ^ title ^ label ^ h 
                  ^ blocks ^ (ostrToStr it) 
                  ^ nesteds

let sectionToString (Section (t, lo, h, bs, it, ss)) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat subSectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*section:" ^ title ^ label ^ h 
                ^ blocks ^ (ostrToStr it) 
                ^ nesteds

let chapterToString (Chapter (t, l, h, bs, it, ss)) =
  let blocks = map_concat blockToString bs in
  let sections = map_concat sectionToString ss in
  let title = "title: " ^ t in
  let label = labelToString l in
    "*chapter:" ^ title ^ label ^ h 
                ^ blocks ^ (ostrToStr it) 
                ^ sections

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
      

let groupToTex (Group(preamble, (topt, lo, hb, ats, it, he))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptionToString lo in
  let heading = match topt with
                | None -> hb
                | Some t -> hb ^ t in
    preamble ^
    heading ^ label ^ 
    atoms ^ (ostrToStr it) ^ 
    he  


let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a


let paragraphToTex (Paragraph ((t, lo, h, bs, it))) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptionToString lo in
    h ^ label ^ blocks ^ (ostrToStr it)

let subsubsectionToTex (Subsubsection ((t, lo, h, bs, it, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat paragraphToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ 
    blocks ^ (ostrToStr it) ^ 
    nesteds

let subsectionToTex (Subsection ((t, lo, h, bs, it, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ 
    blocks ^ (ostrToStr it) ^ 
    nesteds

let sectionToTex (Section ((t, lo, h, bs, it, ss))) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ 
    blocks ^ (ostrToStr it) ^ nesteds

let chapterToTex (Chapter ((t, l, h, bs, it, ss))) =
  let blocks = map_concat blockToTex bs in
  let sections = map_concat sectionToTex ss in
  let label = labelToString l in
    h ^ label ^ 
    blocks ^ (ostrToStr it) ^ 
    sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)




