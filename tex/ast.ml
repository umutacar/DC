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
type t_refsol = string 

type item = Item of t_keyword *  t_point_val option * t_item_body

type ilist = IList of t_preamble  
                      * (t_ilist_kind * t_keyword * t_point_val option *
                         item list * t_keyword) 

type atom = Atom of t_preamble  
                    * (t_atom_kind * t_keyword * t_point_val option * t_title option * 
                       t_label option * 
                       t_atom_body * ilist option * t_refsol option * t_keyword) 

type group = 
  Group of t_preamble 
           * (t_keyword * t_title option * t_label option * 
              atom list * t_intertext * t_keyword) 

type chapter = 
  Chapter of t_preamble
             *  (t_keyword * t_title * t_label * 
                 block list * t_intertext * section list) 

and section = 
  Section of (t_keyword * t_title * t_label option *
              block list * t_intertext * subsection list)

and subsection = 
  Subsection of (t_keyword * t_title * t_label option *
                 block list * t_intertext * subsubsection list)

and subsubsection = 
  Subsubsection of (t_keyword * t_title * t_label option *
                    block list * t_intertext)

and cluster = 
  Cluster of t_preamble
             * (t_keyword * t_point_val option * t_title option * t_label option * 
                element list * t_intertext * t_keyword)

and block = 
  | Block_Block of element
  | Block_Cluster of cluster

and element = 
  | Element_Group of group
  | Element_Atom of atom

(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)
let mk_pval pvalopt = 
  match pvalopt with 
  |  None -> 0.0
  |  Some x -> x
 
let map f xs = 
  List.map xs f

let map_concat f xs: string = 
  let xs_s: string list = List.map xs f in
  let result = List.fold_left xs_s  ~init:"" ~f:(fun result x -> result ^ x) in
    result

let map_and_sum_pts f xs =   
  let pvals_and_xs = map f xs in
  let pvals = map fst pvals_and_xs in
  let xs = map snd pvals_and_xs in
  let pts_total = 
    match List.reduce pvals (fun x y -> x +. y) with 
    | None -> 0.0
    | Some r -> r
  in
    (pts_total, xs)  

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
  | Some x -> 
    let f = Float.to_string x in
    let _ = d_printf ("pval_opt_to_string_opt: points = %f\n") x in
      Some f

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

let refsolOptToTex refsol_opt = 
  let heading = "\\solution" in
  let r = match refsol_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r


let itemToTex (Item(keyword, pval, body)) = 
  match pval with 
  | None -> keyword ^ body
  | Some p -> keyword ^ (pointvalToTex p) ^ body

let ilistToTex (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end))) = 
  let il = List.map itemslist itemToTex in
  let ils = String.concat ~sep:"" il in
    preamble ^ h_begin ^ ils ^ h_end
      
let atomToTex (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist_opt, refsol_opt, h_end))) = 
  let label = labelOptToTex lopt in
  let refsol = refsolOptToTex refsol_opt in
    match ilist_opt with 
    | None -> 
      let _ = d_printf "atomToTex: h_begin = %s" h_begin in         
      let r =  preamble ^ h_begin ^ label ^ body ^ refsol ^ h_end in 
(*      let _  = d_printf "atomToTex: atom =  %s" r in *)
        r
    | Some il ->
      let ils = ilistToTex il in 
        preamble ^ h_begin ^ label ^ body ^ ils ^ refsol ^ h_end      

let groupToTex (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let atoms = map_concat atomToTex ats in
  let label = labelOptToTex lopt in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ it ^ 
    h_end

let elementToTex b = 
  match b with
  | Element_Group g -> groupToTex g
  | Element_Atom a -> atomToTex a

let clusterToTex (Cluster(preamble, (h_begin, pval_opt, topt, lopt, bs, it, h_end))) = 
  let _ = d_printf "clusterToTex" in
  let elements = map_concat elementToTex bs in
  let label = labelOptToTex lopt in
    preamble ^
    h_begin ^ label ^ 
    elements ^ it ^ 
    h_end

let blockToTex x = 
  let _ = d_printf "blockToTex" in
  let r = 
    match x with
    | Block_Cluster c -> clusterToTex c
    | Block_Block b -> elementToTex b
  in
  let _ = d_printf ("ast.blockToTex: %s\n") r  in
    r

let subsubsectionToTex (Subsubsection (heading, t, lopt, bs, it)) =
  let blocks = map_concat blockToTex bs in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    blocks ^ it

let subsectionToTex (Subsection (heading, t, lopt, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    blocks ^ it ^ 
    nesteds

let sectionToTex (Section (heading, t, lopt, bs, it, ss)) =
  let blocks = map_concat blockToTex bs in
(*  let _ = d_printf "sectionToTex: elements = %s" elements in *)
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    blocks ^ it ^ nesteds

let chapterToTex (Chapter (preamble, (heading, t, l, sbs, it, ss))) =
  let blocks = map_concat blockToTex sbs in
  let sections = map_concat sectionToTex ss in
  let _ = d_printf "ast.chapterToTex: blocks = [begin: blocks] %s... [end: blocks] " blocks in
  let label = labelToTex l in
    preamble ^ 
    heading ^ label ^ 
    blocks ^ it ^ 
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
let refsol_is_single_par = false
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
              (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist, refsol, h_end))) = 
  let _ = d_printf "AtomToXml: kind = %s\n" kind in 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
  let _ = 
    match refsol with 
    | None -> d_printf "AtomToXml: refsol = None\n" 
    | Some x -> d_printf "AtomToXml: refsol = %s\n" x 
  in
  let refsol_src = refsol in
  let refsol_xml = 
    match refsol with 
    | None -> None
    | Some x -> Some (tex2html (mk_index ()) x refsol_is_single_par)
  in
  let ilist_xml =   
    match ilist with  
    | None -> ""
    | Some l -> ilistToXml tex2html l 
  in
  let r = XmlSyntax.mk_atom ~kind:kind 
                            ~pval:pval_str_opt
                            ~topt:topt ~t_xml_opt:t_xml_opt
                            ~lopt:lsopt 
                            ~body_src:body
                            ~body_xml:body_xml
                            ~ilist:ilist_xml
                            ~refsol_xml_opt:refsol_xml
                            ~refsol_src_opt:refsol_src
   in
     r
     
let groupToXml tex2html
               (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let atoms = map_concat (atomToXml tex2html) ats in
  let r = XmlSyntax.mk_group ~topt:topt ~t_xml_opt:t_xml_opt 
                             ~lopt:lsopt ~body:atoms in
    r

let elementToXml tex2html b = 
  match b with
  | Element_Group g -> groupToXml tex2html g
  | Element_Atom a -> atomToXml  tex2html a

let clusterToXml tex2html (Cluster(preamble, (h_begin, pval_opt, topt, lopt, bs, it, h_end))) = 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let (lsopt, t_xml_opt) = label_title_opt tex2html lopt topt in
  let elements = map_concat (elementToXml tex2html) bs in
  let r = XmlSyntax.mk_cluster ~pval:pval_str_opt 
                               ~topt:topt ~t_xml_opt:t_xml_opt 
                               ~lopt:lsopt ~body:elements in
    r

let blockToXml tex2html x = 
  match x with
  | Block_Block b -> elementToXml tex2html b
  | Block_Cluster c -> clusterToXml  tex2html c

let subsubsectionToXml  tex2html (Subsubsection (heading, t, lopt, bs, it)) =
  let (lsopt, t_xml) = label_title tex2html lopt t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let body = blocks in
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

let chapterToXml  tex2html (Chapter (preamble, (heading, t, l, bs, it, ss))) =
  let Label(heading, label) = l in 
  let _ = d_printf "chapter label, heading = %s  = %s\n" heading label in
  let t_xml = tex2html (mk_index()) t title_is_single_par in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~title:t ~title_xml:t_xml ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)


(**********************************************************************
 ** BEGIN: AST ELEBORATION
 **********************************************************************)

(* Identity function *)
let labelEl l = l

(* Identity function *)
let labelOptEl lopt = 
  match lopt with
  | None -> None
  | Some l -> Some (labelEl l) 

(* Identity function *)
let refsolOptEl refsol_opt = 
  refsol_opt

(* Identity function *)
let itemEl (Item(keyword, pval, body)) = 
  Item (keyword, pval, body)

(* Locally identity function *)
let ilistOptEl ilist_opt = 
  match ilist_opt with 
  | None -> None
  | Some (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end))) ->
      let itemslist = List.map itemslist itemEl in
        Some (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end)))
            
let atomEl (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist_opt, refsol_opt, h_end))) = 
  let lopt = labelOptEl lopt in
  let refsol_opt = refsolOptEl refsol_opt in
  let ilist_opt = ilistOptEl ilist_opt in
  let pval = mk_pval pval_opt  in
    (pval, Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist_opt, refsol_opt, h_end)))

let groupEl (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let (pvalsum, ats) = map_and_sum_pts atomEl ats in
  let lopt = labelOptEl lopt in
    (pvalsum, Group (preamble, (h_begin, topt, lopt, ats, it, h_end)))

let elementEl b = 
  match b with
  | Element_Group g -> 
    let (pval, g) = groupEl g in 
      (pval, Element_Group g)
  | Element_Atom a -> 
    let (pval, a) = atomEl a in
      (pval, Element_Atom a)

let clusterEl (Cluster(preamble, (h_begin, pval_opt, topt, lopt, es, it, h_end))) = 
  let _ = d_printf "clusterEl" in
  let (pts, es) = map_and_sum_pts elementEl es in
  let lopt = labelOptEl lopt in
    Cluster (preamble, (h_begin, Some pts, topt, lopt, es, it, h_end))

let blockEl x = 
  let _ = d_printf "blockEl" in
    match x with
    | Block_Cluster c -> Block_Cluster (clusterEl c)
    | Block_Block b -> 
      let (_, b) = elementEl b in
        Block_Block b 

let subsubsectionEl (Subsubsection (heading, t, lopt, bs, it)) =
  let bs = map blockEl bs in
  let lopt = labelOptEl lopt in
    Subsubsection (heading, t, lopt, bs, it)

let subsectionEl (Subsection (heading, t, lopt, bs, it, ss)) =
  let bs = map blockEl bs in
  let ss = map subsubsectionEl ss in
  let lopt = labelOptEl lopt in
    Subsection (heading, t, lopt, bs, it, ss)

let sectionEl (Section (heading, t, lopt, bs, it, ss)) =
  let bs = map blockEl bs in
(*  let _ = d_printf "sectionEl: elements = %s" elements in *)
  let ss = map subsectionEl ss in
  let lopt = labelOptEl lopt in
    Section (heading, t, lopt, bs, it, ss)

let chapterEl (Chapter (preamble, (heading, t, l, bs, it, ss))) =
  let bs = map blockEl bs in
  let ss = map sectionEl ss in
  let l = labelEl l in
    (Chapter (preamble, (heading, t, l, bs, it, ss)))

(**********************************************************************
 ** END: AST ELABORATION
 **********************************************************************)



(**********************************************************************
 ** BEGIN: AST  TRAVERSAL
 **********************************************************************)

(* Identity function *)
let labelTR l = l

(* Identity function *)
let labelOptTR lopt = 
  match lopt with
  | None -> None
  | Some l -> Some (labelTR l) 

(* Identity function *)
let refsolOptTR refsol_opt = 
  refsol_opt

(* Identity function *)
let itemTR (Item(keyword, pval, body)) = 
  Item (keyword, pval, body)

(* Locally identity function *)
let ilistOptTR ilist_opt = 
  match ilist_opt with 
  | None -> None
  | Some (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end))) ->
      let itemslist = List.map itemslist itemTR in
        Some (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end)))
            
let atomTR (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist_opt, refsol_opt, h_end))) = 
  let lopt = labelOptTR lopt in
  let refsol_opt = refsolOptTR refsol_opt in
  let ilist_opt = ilistOptTR ilist_opt in
    Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, body, ilist_opt, refsol_opt, h_end))

let groupTR (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let ats = map atomTR ats in
  let lopt = labelOptTR lopt in
    Group (preamble, (h_begin, topt, lopt, ats, it, h_end))

let elementTR b = 
  match b with
  | Element_Group g -> Element_Group (groupTR g)
  | Element_Atom a -> Element_Atom (atomTR a)

let clusterTR (Cluster(preamble, (h_begin, pval_opt, topt, lopt, es, it, h_end))) = 
  let _ = d_printf "clusterTR" in
  let es = map elementTR es in
  let lopt = labelOptTR lopt in
    Cluster (preamble, (h_begin, pval_opt, topt, lopt, es, it, h_end))

let blockTR x = 
  let _ = d_printf "blockTR" in
    match x with
    | Block_Cluster c -> Block_Cluster (clusterTR c)
    | Block_Block b -> Block_Block (elementTR b)

let subsubsectionTR (Subsubsection (heading, t, lopt, bs, it)) =
  let bs = map blockTR bs in
  let lopt = labelOptTR lopt in
    Subsubsection (heading, t, lopt, bs, it)

let subsectionTR (Subsection (heading, t, lopt, bs, it, ss)) =
  let bs = map blockTR bs in
  let ss = map subsubsectionTR ss in
  let lopt = labelOptTR lopt in
    Subsection (heading, t, lopt, bs, it, ss)

let sectionTR (Section (heading, t, lopt, bs, it, ss)) =
  let bs = map blockTR bs in
(*  let _ = d_printf "sectionTR: elements = %s" elements in *)
  let ss = map subsectionTR ss in
  let lopt = labelOptTR lopt in
    Section (heading, t, lopt, bs, it, ss)

let chapterTR (Chapter (preamble, (heading, t, l, bs, it, ss))) =
  let bs = map blockTR bs in
  let ss = map sectionTR ss in
  let l = labelTR l in
    (Chapter (preamble, (heading, t, l, bs, it, ss)))

(**********************************************************************
 ** END: AST TRAVERSAL
 **********************************************************************)
