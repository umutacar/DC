type atom_kind = string
type atom_body = string
type title = string
type keyword = string

(* Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", etc

   Because we don't care about white space, they might have whitespace in them.
 *)

type label = Label of keyword * string * keyword 
type atom = Atom of atom_kind * title option * label option * keyword * atom_body * keyword
type group = Group of title option * label option * keyword * atom list * keyword

type chapter = 
  Chapter of  title * label option * keyword * block list * section list

and section = 
  Section of title * label option * keyword * block list * subsection list

and subsection = 
  Subsection of title * label option * keyword * block list * subsubsection list

and subsubsection = 
  Subsubsection of title * label option * keyword * block list

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

let atomToString (Atom(kind, topt, lo, hb, ab, he)) = 
  let label = "label: " ^ labelOptionToString lo in
    match topt with
    | None -> hb ^ label ^ ab ^ he
    | Some t -> hb ^ "kind:" ^ kind ^ "title:" ^ t ^ label ^ ab ^ he

let groupToString (Group(topt, lo, hb, ats, he)) = 
  let atoms = map_concat atomToString ats in
  let label = "label: " ^ labelOptionToString lo in
    match topt with
    | None -> hb ^ label ^ atoms ^ he
    | Some t -> hb ^ "title:" ^ t ^ label ^ atoms ^ he

let blockToString b = 
  match b with
  | Block_Group g -> groupToString g
  | Block_Atom a -> atomToString a

let subsubsectionToString (Subsubsection (t, lo, h, bs)) =
  let blocks = map_concat blockToString bs in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*subsubsection:" ^ title ^ label ^ h ^ blocks

let subSectionToString (Subsection (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToString bs in
  let nesteds = map_concat subsubsectionToString ss in
  let title = "title: " ^ t in
  let label = "label: " ^ labelOptionToString lo in
    "*subsection:" ^ title ^ label ^ h ^ blocks ^ nesteds

let sectionToString (Section (t, lo, h, bs, ss)) =
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
let atomToTex (Atom(kind, topt, lo, hb, ab, he)) = 
  let label = labelOptionToString lo in
    hb ^ label ^ ab ^ he

let groupToTex (Group(topt, lo, hb, ats, he)) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptionToString lo in
    hb ^ label ^ atoms ^ he

let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a

let subsubsectionToTex (Subsubsection (t, lo, h, bs)) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptionToString lo in
    h ^ label ^ blocks

let subsectionToTex (Subsection (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ blocks ^ nesteds

let sectionToTex (Section (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ blocks ^ nesteds

let chapterToTex (Chapter (t, lo, h, bs, ss)) =
  let blocks = map_concat blockToTex bs in
  let sections = map_concat sectionToTex ss in
  let label = labelOptionToString lo in
    h ^ label ^ blocks ^ sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)




