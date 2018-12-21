(**********************************************************************
 ** BEGIN: AST Data Types
*********************************************************************)
type t_preambly = string
type t_atom_kind = string
type t_atom_body = string
type t_label_val = string
type t_title = string
type t_intertext = string
type t_keyword = string
(* T_Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", 
                "\begin{example}[title]  \n"
                "\end{example}  \n\n\n"
                  
   Because we don't care about white space, they might have whitespace in them.
 *)

(* Label is heading and the label string *)
type t_label = Label of t_keyword * t_label_val

type atom = Atom of t_preambly * (t_atom_kind * t_keyword * t_title option * t_label option * t_atom_body * t_keyword) 

type group = 
  Group of t_preambly 
           * (t_keyword * t_title option * t_label option * 
              atom list * t_intertext * t_keyword) 


type chapter = 
  Chapter of (t_keyword * t_title * t_label
              * block list * t_intertext * section list) 

and section = 
  Section of (t_keyword * t_title * t_label option
              * block list * t_intertext * subsection list)

and subsection = 
  Subsection of (t_keyword * t_title * t_label option
                 * block list * t_intertext * subsubsection list)

and subsubsection = 
  Subsubsection of (t_keyword * t_title * t_label option
                    * block list * t_intertext * paragraph list)

and paragraph = 
  Paragraph of (t_keyword * t_title * t_label option
                * block list * t_intertext)
and block = 
  | Block_Group of group
  | Block_Atom of atom

(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)

let map_concat f xs = 
  let xs_s = List.map f xs in
  let result = List.fold_left (fun result x -> result ^ x) "" xs_s in
    result

let newline = "\n"

let index = ref 0
let mk_index () = 
  let r = string_of_int !index in
  let _ = index := !index + 1 in
    r

(**********************************************************************
 ** END Utilities
 *********************************************************************)

(**********************************************************************
 ** BEGIN: AST To String
*********************************************************************)
let ostrToStr os = 
  match os with 
  |  None -> ""
  |  Some s -> s

let labelToString (Label(h, label_string)) = h
let labelToVal (Label(h, label_string)) = label_string

let labelOptToString lopt = 
  let r = match lopt with 
              |  None -> ""
              |  Some l -> labelToString l  in
     r

let labelOptToStringOpt lopt = 
  let r = match lopt with 
              |  None -> None
              |  Some l -> Some (labelToVal l)  in
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
let atomToTex (Atom(preamble, (kind, h_begin, topt, lopt, body, h_end))) = 
  let label = labelOptToString lopt in
    preamble ^ h_begin ^ label ^ body ^ h_end
      

let groupToTex (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptToString lopt in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ it ^ 
    h_end


let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a


let paragraphToTex (Paragraph (heading, t, lopt, bs, it)) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptToString lopt in
    heading ^ label ^ blocks ^ it 

let subsubsectionToTex (Subsubsection (heading, t, lopt, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat paragraphToTex ss in
  let label = labelOptToString lopt in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let subsectionToTex (Subsection (heading, t, lopt, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptToString lopt in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let sectionToTex (Section (heading, t, lopt, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptToString lopt in
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
let body_is_single_par = false
let title_is_single_par = true


let label_title tex2html lopt t = 
  let lsopt = labelOptToStringOpt lopt in
  let t_xml = tex2html (mk_index()) t title_is_single_par in
    (lsopt, t_xml)

let label_title_opt tex2html lopt topt = 
  let lsopt = labelOptToStringOpt lopt in
  let t_xml_opt = match topt with 
                  | None -> None
                  | Some t -> Some (tex2html (mk_index()) t title_is_single_par)
  in
    (lsopt, t_xml_opt)


let atomToXml tex2html
              (Atom(preamble, (kind, h_begin, topt, lopt, body, h_end))) = 
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
  let r = XmlSyntax.mk_atom ~kind:kind 
                            ~topt:topt ~t_xml_opt:t_xml_opt
                            ~lopt:lsopt 
                            ~body_src:body
                            ~body_xml:body_xml
          in
    r
     
let groupToXml tex2html
               (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let atoms = map_concat (atomToXml tex2html) ats in
  let r = XmlSyntax.mk_group ~topt:topt ~t_xml_opt:t_xml_opt 
                             ~lopt:lsopt ~body:atoms in
    r

let blockToXml tex2html b = 
  match b with
  | Block_Group g -> groupToXml tex2html g
  | Block_Atom a -> atomToXml  tex2html a

let paragraphToXml  tex2html (Paragraph (heading, t, lopt, bs, it)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let r = XmlSyntax.mk_paragraph ~title:t ~title_xml:t_xml 
                                 ~lopt:lsopt ~body:blocks in
    r

let subsubsectionToXml  tex2html (Subsubsection (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (paragraphToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsubsection ~title:t ~title_xml:t_xml
                                     ~lopt:lsopt ~body:body in
    r

let subsectionToXml  tex2html (Subsection (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsubsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsection ~title:t ~title_xml:t_xml
                                  ~lopt:lsopt ~body:body in
    r

let sectionToXml  tex2html (Section (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_section ~title:t ~title_xml:t_xml
                               ~lopt:lsopt ~body:body in
    r

let chapterToXml  tex2html (Chapter (heading, t, l, bs, it, ss)) =
  let label = labelToVal l in 
  let t_xml = tex2html (mk_index()) t title_is_single_par in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~title:t ~title_xml:t_xml ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)




