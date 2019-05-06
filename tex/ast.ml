open Core
open Utils

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)

let points_correct = 1.0
let points_incorrect = 0.0
let space = " "

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
type t_depend = Depend of t_keyword * t_label_val list 
type t_point_val = float
type t_exp = string 
type t_hint = string 
type t_refsol = string 

type item = Item of t_keyword *  t_point_val option * t_item_body

type ilist = IList of t_preamble  
                      * (t_ilist_kind * t_keyword * t_point_val option *
                         item list * t_keyword) 

type atom = Atom of t_preamble  
                    * (t_atom_kind * t_keyword * t_point_val option * t_title option * 
                       t_label option * t_depend option * 
                       t_atom_body * 
                       ilist option * 
                       t_hint option * t_refsol option * t_exp option *   
                       t_keyword) 

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

and block = 
  | Block_Block of element
  | Block_Cluster of cluster

and cluster = 
  Cluster of t_preamble
             * (t_keyword * t_point_val option * t_title option * t_label option * 
                element list * t_intertext * t_keyword)

and element = 
  | Element_Group of group
  | Element_Atom of atom

(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)
let mk_pval pvalopt= 
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


let pval_opt_to_string pval = 
  match pval with 
  | None -> "None"
  | Some x -> 
    let f = Float.to_string x in
    let _ = d_printf ("pval_opt_to_string: points = %f\n") x in
      "Some" ^ f

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
  
let mk_item (keyword, pvalopt, body) = 
  let pval = match pvalopt with
             | None -> item_keyword_to_point keyword
             | Some pts -> pts
  in
    Item (keyword, Some pval, body)
(**********************************************************************
 ** BEGIN: AST To LaTeX
 **********************************************************************)
let mktex_optarg x = 
  "[" ^ x ^ "]"

let mktex_begin block_name pvalopt topt = 
  let b = "\\begin{" ^ block_name ^ "}" in
  let p = match pvalopt with 
          | None -> ""
          | Some pts -> mktex_optarg (Float.to_string pts)
  in 
  let t = match topt with 
          | None -> ""
          | Some t -> mktex_optarg t
  in
    b ^ p ^ t ^ "\n"

let dependOptToTex dopt = 
  let r = match dopt with 
              |  None -> ""
              |  Some Depend(heading, ls) -> heading ^ (String.concat ~sep:", " ls) ^ "}" ^ "\n" in
     r


let labelToTex (Label(h, label_string)) = h
let labelOptToTex lopt = 
  let r = match lopt with 
              |  None -> ""
              |  Some Label(heading, l) -> heading  in
     r

let pointvalToTex p = 
  mktex_optarg (Float.to_string p)

let pointvalOptToTex p = 
  match p with 
  | None -> ""
  | Some pts -> mktex_optarg (Float.to_string pts)

let hintOptToTex hint_opt = 
  let heading = "\\hint" in
  let r = match hint_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r

let expOptToTex exp_opt = 
  let heading = "\\explain" in
  let r = match exp_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r

let refsolOptToTex refsol_opt = 
  let heading = "\\solution" in
  let r = match refsol_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r


let itemToTex (Item(keyword, pval, body)) = 
  match pval with 
  | None -> keyword ^ body
  | Some p -> keyword ^ (pointvalToTex p) ^ space ^ body

let ilistToTex (IList(preamble, (kind, h_begin, point_val_opt, itemslist, h_end))) = 
  let h_begin = mktex_begin kind point_val_opt None in
  let il = List.map itemslist itemToTex in
  let ils = String.concat ~sep:"" il in
    preamble ^ h_begin ^ ils ^ h_end
      
let atomToTex (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, h_end))) = 
  let label = labelOptToTex lopt in
  let depend = dependOptToTex dopt in
  let hint = hintOptToTex hint_opt in
  let refsol = refsolOptToTex refsol_opt in
  let exp = expOptToTex exp_opt in
    match ilist_opt with 
    | None -> 
      let _ = d_printf "atomToTex: h_begin = %s" h_begin in         
      let r =  preamble ^ h_begin ^ label ^ depend ^ body ^ hint ^ refsol ^ exp ^ h_end in 
(*      let _  = d_printf "atomToTex: atom =  %s" r in *)
        r
    | Some il ->
      let ils = ilistToTex il in 
        preamble ^ h_begin ^ label ^ depend ^ body ^ ils ^ hint ^ refsol ^ exp ^ h_end      

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
  let _ = d_printf "clusterToTex, points = %s\n" (pval_opt_to_string pval_opt) in
  let h_begin = mktex_begin "cluster" pval_opt topt in
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
let explain_is_single_par = false
let hint_is_single_par = false
let refsol_is_single_par = false
let title_is_single_par = true


let extract_label lopt = 
  let r = match lopt with 
              |  None -> None
              |  Some Label(heading, v) -> Some v  in
     r

let extract_depend dopt = 
  let r = match dopt with 
              |  None -> None
              |  Some Depend(heading, ls) -> Some (String.concat ~sep:", " ls)  in
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

let titleToXml tex2html t = 
  let t_xml = tex2html (mk_index()) t title_is_single_par in
    t_xml

let fieldOptToXml tex2html is_single_par xopt =
   match xopt with 
   | None -> None 
   | Some x -> let x_xml = tex2html (mk_index()) x is_single_par in
                 Some (x_xml, x)

let titleOptToXml tex2html topt = 
  fieldOptToXml tex2html title_is_single_par topt

let refsolOptToXml tex2html refsol_opt = 
  let _ = 
    match refsol_opt with 
    | None -> d_printf "ast.refsolOptToXml = None\n" 
    | Some x -> d_printf "ast.refsolOptToXml: refsol = %s\n" x 
  in
    fieldOptToXml tex2html refsol_is_single_par refsol_opt


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
              (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist, hint, refsol_opt, exp_opt, h_end))) = 
  let _ = d_printf "AtomToXml: kind = %s\n" kind in 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let dsopt = extract_depend dopt in
  let title_opt = titleOptToXml tex2html topt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
  let ilist_xml_opt =   
    match ilist with  
    | None -> None
    | Some l -> Some (ilistToXml tex2html l) 
  in
  let hint_src = hint in
  let hint_xml = 
    match hint with 
    | None -> None
    | Some x -> Some (tex2html (mk_index ()) x hint_is_single_par)
  in
(*
  let refsol_src = refsol in
  let refsol_xml = 
    match refsol with 
    | None -> None
    | Some x -> Some (tex2html (mk_index ()) x refsol_is_single_par)
  in
*)
  let refsols_opt = refsolOptToXml tex2html refsol_opt in
  let r = XmlSyntax.mk_atom ~kind:kind 
                            ~pval:pval_str_opt
                            ~topt:title_opt
                            ~lopt:lsopt ~dopt:dsopt 
                            ~body_src:body
                            ~body_xml:body_xml
                            ~ilist_opt:ilist_xml_opt
                            ~refsol_opt:refsols_opt
   in
     r
     
let groupToXml tex2html
               (Group(preamble, (h_begin, topt, lopt, ats, it, h_end))) = 
  let lsopt = extract_label lopt in
  let title_opt = titleOptToXml tex2html topt in
  let atoms = map_concat (atomToXml tex2html) ats in
  let r = XmlSyntax.mk_group ~topt:title_opt
                             ~lopt:lsopt ~body:atoms in
    r

let elementToXml tex2html b = 
  match b with
  | Element_Group g -> groupToXml tex2html g
  | Element_Atom a -> atomToXml  tex2html a

let clusterToXml tex2html (Cluster(preamble, (h_begin, pval_opt, topt, lopt, bs, it, h_end))) = 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let title_opt = titleOptToXml tex2html topt in
  let elements = map_concat (elementToXml tex2html) bs in
  let r = XmlSyntax.mk_cluster ~pval:pval_str_opt 
                               ~topt:title_opt
                               ~lopt:lsopt ~body:elements in
    r

let blockToXml tex2html x = 
  match x with
  | Block_Block b -> elementToXml tex2html b
  | Block_Cluster c -> clusterToXml tex2html c

let subsubsectionToXml  tex2html (Subsubsection (heading, t, lopt, bs, it)) =
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let body = blocks in
  let r = XmlSyntax.mk_subsubsection ~title:t ~title_xml:t_xml
                                     ~lopt:lsopt ~body:body in
    r

let subsectionToXml  tex2html (Subsection (heading, t, lopt, bs, it, ss)) =
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsubsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsection ~title:t ~title_xml:t_xml
                                  ~lopt:lsopt ~body:body in
    r

let sectionToXml  tex2html (Section (heading, t, lopt, bs, it, ss)) =
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_section ~title:t ~title_xml:t_xml
                               ~lopt:lsopt ~body:body in
    r

let chapterToXml  tex2html (Chapter (preamble, (heading, t, l, bs, it, ss))) =
  let Label(heading, label) = l in 
  let _ = d_printf "chapter label, heading = %s  = %s\n" heading label in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~title:t ~title_xml:t_xml ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)


(**********************************************************************
 ** BEGIN: AST ELABORATION
 ** What does elaboration do? 
 **********************************************************************)

(* Identity function *)
let dependEl d = d

(* Identity function *)
let dependOptEl dopt = 
  match dopt with
  | None -> None
  | Some l -> Some (dependEl l) 

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
let expOptEl exp_opt = 
  exp_opt

(* Identity function *)
let hintOptEl hint_opt = 
  hint_opt

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
            
let atomEl (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, h_end))) = 
  let lopt = labelOptEl lopt in
  let dopt = dependOptEl dopt in
  let hint_opt = hintOptEl hint_opt in
  let refsol_opt = refsolOptEl refsol_opt in
  let exp_opt = expOptEl exp_opt in
  let ilist_opt = ilistOptEl ilist_opt in
  let pval = mk_pval pval_opt  in
    (pval, Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, h_end)))

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
  let _ = d_printf "clusterEl: points = %s" (Float.to_string pts) in
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
let dependTR d = d

(* Identity function *)
let dependOptTR dopt = 
  match dopt with
  | None -> None
  | Some d -> Some (dependTR d) 


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
let expOptTR exp_opt = 
  exp_opt

(* Identity function *)
let hintOptTR hint_opt = 
  hint_opt


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
            
let atomTR (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, h_end))) = 
  let dopt = dependOptTR dopt in
  let lopt = labelOptTR lopt in
  let hint_opt = hintOptTR hint_opt in
  let refsol_opt = refsolOptTR refsol_opt in
  let exp_opt = expOptTR exp_opt in

  let ilist_opt = ilistOptTR ilist_opt in
    Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, h_end))

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
