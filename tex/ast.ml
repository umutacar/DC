open Core
open Utils

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)

let points_correct = 1.0
let points_incorrect = 0.0

(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
*********************************************************************)
type t_preamble = string
type t_atom_kind = string
type t_atom_body = string
type t_item_kind = string
type t_item_body = string
type t_ilist_body = string
type t_ilist_kind = string
type t_ilist_item = string
type t_intertext = string
type t_keyword = string
type t_label_val = string
type t_title = string


(* T_Keywords are used to capture the "beginning" and "end" of the commands.
   For example, "    \label   {"  and "}    \n", 
                "\begin{example}[title]  \n"
                "\end{example}  \n\n\n"
                  
   Because we don't care about white space, they might have whitespace in them.
 *)

(* Label is heading and the label string 
 * Example Label(\label{this}, this)
 *)

type t_label = Label of t_keyword * t_label_val
type t_point_val = float

type item = Item of t_keyword *  t_point_val option * t_item_body

type ilist = IList of t_preamble  
                      * (t_ilist_kind * t_keyword * t_point_val option *
                         item list * t_keyword) 

type atom = Atom of t_preamble  
                    * (t_atom_kind * t_keyword * t_title option * t_label option * 
                       t_atom_body * ilist option * t_keyword) 

type group = 
  Group of t_preamble 
           * (t_keyword * t_title option * t_label option * 
              atom list * t_intertext * t_keyword) 


type chapter = 
  Chapter of t_preamble
             *  (t_keyword * t_title * t_label * 
                 superblock list * t_intertext * section list) 

and section = 
  Section of (t_keyword * t_title * t_label option *
              superblock list * t_intertext * subsection list)

and subsection = 
  Subsection of (t_keyword * t_title * t_label option *
                 superblock list * t_intertext * subsubsection list)

and subsubsection = 
  Subsubsection of (t_keyword * t_title * t_label option *
                    superblock list * t_intertext * paragraph list)

and paragraph = 
  Paragraph of (t_keyword * t_title * t_label option *
                superblock list * t_intertext)

and cluster = 
  Cluster of t_preamble
             * (t_keyword * t_title option * t_label option * 
                block list * t_intertext * t_keyword)

and superblock = 
  | Superblock_Block of block
  | Superblock_Cluster of cluster

and block = 
  | Block_Group of group
  | Block_Atom of atom

(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)

let map_concat f xs: string = 
  let xs_s: string list = List.map xs f in
  let result = List.fold_left xs_s  ~init:"" ~f:(fun result x -> result ^ x) in
    result

let newline = "\n"

let index = ref 0
let mk_index () = 
  let r = string_of_int !index in
  let _ = index := !index + 1 in
    r

let contains_substring search target =
  let _ = d_printf "contains_substring: search = %s target = %s\n" search target in
  let found = String.substr_index ~pos:0 ~pattern:search target in
  let res = 
    match found with 
    | None -> let _ = d_printf "contains_substring: found none\n" in false
    | Some x -> let _ = d_printf "contains_substring: found match %d\n" x in true 
  in
    res


let pval_opt_to_string_opt pval = 
  match pval with 
  | None -> None
  | Some x -> Some (Float.to_string x)

(**********************************************************************
 ** END Utilities
 *********************************************************************)

(**********************************************************************
 ** BEGIN: Makers
 **********************************************************************)

let item_keyword_to_point keyword = 
  let _ = d_printf "item_keyword_to_point: keyword = %s\n" keyword in
  let pval = 
    if contains_substring "correct" keyword then
      points_correct
    else if contains_substring "Correct" keyword then
      points_correct    
    else
      points_incorrect
  in
  let _ = d_printf "item_keyword_to_point: pval = %f\n" pval in
    pval
  
let mk_item (keyword, body) = 
  let pval = item_keyword_to_point keyword in
    Item (keyword, Some pval, body)
(**********************************************************************
 ** BEGIN: AST To LaTeX
 **********************************************************************)
let mktex_optarg x = 
  "[" ^ x ^ "]"

let labelToTex (Label(h, label_string)) = h
let labelOptToTex lopt = 
  let r = match lopt with 
              |  None -> ""
              |  Some Label(heading, l) -> heading  in
     r

let pointvalToTex p = 
  mktex_optarg (Float.to_string p)

let itemToTex (Item(keyword, pval, body)) = 
  match pval with 
  | None -> keyword ^ body
  | Some p -> keyword ^ (pointvalToTex p) ^ body

let ilistToTex (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end))) = 
  let il = List.map itemslist itemToTex in
  let ils = String.concat ~sep:"" il in
    preamble ^ h_begin ^ ils ^ h_end
      
let atomToTex (Atom(preamble, (kind, h_begin, topt, lopt, body, ilist_opt, h_end))) = 
  let label = labelOptToTex lopt in
    match ilist_opt with 
    | None -> 
      let _ = d_printf "atomToTex: h_begin = %s" h_begin in         
      let r =  preamble ^ h_begin ^ label ^ body ^ h_end in 
(*      let _  = d_printf "atomToTex: atom =  %s" r in *)
        r
    | Some il ->
      let ils = ilistToTex il in 
        preamble ^ h_begin ^ label ^ body ^ ils ^ h_end      

let groupToTex (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptToTex lopt in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ it ^ 
    h_end

let blockToTex b = 
  match b with
  | Block_Group g -> groupToTex g
  | Block_Atom a -> atomToTex a

let clusterToTex (Cluster(preamble, (h_begin, topt, lopt, bs, it, h_end))) = 
  let _ = d_printf "clusterToTex" in
  let blocks = map_concat blockToTex bs in
  let label = labelOptToTex lopt in
    preamble ^
    h_begin ^ label ^ 
    blocks ^ it ^ 
    h_end

let superblockToTex x = 
  let _ = d_printf "superblockToTex" in
  let r = 
    match x with
    | Superblock_Cluster c -> clusterToTex c
    | Superblock_Block b -> blockToTex b
  in
  let _ = d_printf ("ast.superblockToTex: %s\n") r  in
    r

let paragraphToTex (Paragraph (heading, t, lopt, bs, it)) =
  let superblocks = map_concat superblockToTex bs in
  let label = labelOptToTex lopt in
    heading ^ label ^ superblocks ^ it 

let subsubsectionToTex (Subsubsection (heading, t, lopt, bs, it, ss)) =
  let superblocks = map_concat superblockToTex bs in
  let nesteds = map_concat paragraphToTex ss in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    superblocks ^ it ^ 
    nesteds

let subsectionToTex (Subsection (heading, t, lopt, bs, it, ss)) =
  let superblocks = map_concat superblockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    superblocks ^ it ^ 
    nesteds

let sectionToTex (Section (heading, t, lopt, bs, it, ss)) =
  let superblocks = map_concat superblockToTex bs in
(*  let _ = d_printf "sectionToTex: blocks = %s" blocks in *)
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    superblocks ^ it ^ nesteds

let chapterToTex (Chapter (preamble, (heading, t, l, sbs, it, ss))) =
  let superblocks = map_concat superblockToTex sbs in
  let sections = map_concat sectionToTex ss in
  let _ = d_printf "ast.chapterToTex: superblocks = [begin: superblocks] %s... [end: superblocks] " superblocks in
  let label = labelToTex l in
    preamble ^ 
    heading ^ label ^ 
    superblocks ^ it ^ 
    sections

(**********************************************************************
 ** END: AST To LaTeX
 **********************************************************************)


(**********************************************************************
 ** BEGIN: AST To XML
 **********************************************************************)

(* *_single_par flags are used for html post-processing:
 *  "true" means that this was a single paragraph and the
 * <p> </p> annotations must be removed.
 *) 
let body_is_single_par = false
let title_is_single_par = true


let extract_label lopt = 
  let r = match lopt with 
              |  None -> None
              |  Some Label(heading, v) -> Some v  in
     r

let title_opt tex2html topt = 
  let t_xml_opt = match topt with 
                  | None -> None
                  | Some t -> Some (tex2html (mk_index()) t title_is_single_par)
  in
    t_xml_opt

let label_title tex2html lopt t = 
  let lsopt = extract_label lopt in
  let t_xml = tex2html (mk_index()) t title_is_single_par in
    (lsopt, t_xml)


let label_title_opt tex2html lopt topt = 
  let lsopt = extract_label lopt in
  let t_xml_opt = match topt with 
                  | None -> None
                  | Some t -> Some (tex2html (mk_index()) t title_is_single_par)
  in
    (lsopt, t_xml_opt)

let itemToXml tex2html (Item (kind, pval_opt, body)) = 
  let _ = d_printf "itemToXml: kind = %s\n" kind in 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
    XmlSyntax.mk_item ~pval:pval_str_opt ~body_src:body ~body_xml:body_xml

let ilistToXml tex2html
              (IList (preamble, (kind, h_begin, pval_opt, itemslist, h_end))) = 
  let _ = d_printf "IListToXml: kind = %s\n" kind in 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in

  let items_xml = map_concat  (itemToXml tex2html) itemslist in
  let r = XmlSyntax.mk_ilist ~kind:kind 
                             ~pval:pval_str_opt
                             ~body:items_xml
  in
    r


let atomToXml tex2html
              (Atom(preamble, (kind, h_begin, topt, lopt, body, ilist, h_end))) = 
  let _ = d_printf "AtomToXml: kind = %s\n" kind in 
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
  let ilist_xml =   
    match ilist with  
    | None -> ""
    | Some l -> ilistToXml tex2html l 
  in
  let r = XmlSyntax.mk_atom ~kind:kind 
                            ~topt:topt ~t_xml_opt:t_xml_opt
                            ~lopt:lsopt 
                            ~body_src:body
                            ~body_xml:body_xml
                            ~ilist:ilist_xml
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

let clusterToXml tex2html (Cluster(preamble, (h_begin, topt, lopt, bs, it, h_end))) = 
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let blocks = map_concat (blockToXml tex2html) bs in
  let r = XmlSyntax.mk_cluster ~topt:topt ~t_xml_opt:t_xml_opt 
                               ~lopt:lsopt ~body:blocks in
    r

let superblockToXml tex2html x = 
  match x with
  | Superblock_Block b -> blockToXml tex2html b
  | Superblock_Cluster c -> clusterToXml  tex2html c

let paragraphToXml  tex2html (Paragraph (heading, t, lopt, bs, it)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let superblocks = map_concat (superblockToXml  tex2html) bs in
  let r = XmlSyntax.mk_paragraph ~title:t ~title_xml:t_xml 
                                 ~lopt:lsopt ~body:superblocks in
    r

let subsubsectionToXml  tex2html (Subsubsection (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let superblocks = map_concat (superblockToXml  tex2html) bs in
  let nesteds = map_concat (paragraphToXml  tex2html) ss in
  let body = superblocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsubsection ~title:t ~title_xml:t_xml
                                     ~lopt:lsopt ~body:body in
    r

let subsectionToXml  tex2html (Subsection (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let superblocks = map_concat (superblockToXml  tex2html) bs in
  let nesteds = map_concat (subsubsectionToXml  tex2html) ss in
  let body = superblocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsection ~title:t ~title_xml:t_xml
                                  ~lopt:lsopt ~body:body in
    r

let sectionToXml  tex2html (Section (heading, t, lopt, bs, it, ss)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let superblocks = map_concat (superblockToXml  tex2html) bs in
  let nesteds = map_concat (subsectionToXml  tex2html) ss in
  let body = superblocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_section ~title:t ~title_xml:t_xml
                               ~lopt:lsopt ~body:body in
    r

let chapterToXml  tex2html (Chapter (preamble, (heading, t, l, bs, it, ss))) =
  let Label(heading, label) = l in 
  let _ = d_printf "chapter label, heading = %s  = %s\n" heading label in
  let t_xml = tex2html (mk_index()) t title_is_single_par in
  let superblocks = map_concat (superblockToXml  tex2html) bs in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = superblocks ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~title:t ~title_xml:t_xml ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)





