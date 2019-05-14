open Core
open Utils

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)

let points_correct = 1.0
let points_incorrect = 0.0
let space = " "
let correct_choice_indicator = "*"

(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
*********************************************************************)
type t_preamble = string
type t_atom_kind = string
type t_atom_body = string
type t_group_kind = string
type t_item_kind = string
type t_item_body = string
type t_ilist_body = string
type t_ilist_kind = string
type t_ilist_item = string
type t_tailtext = string
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
type t_rubric = string 

type item = Item of t_keyword *  t_point_val option * t_item_body

type ilist = IList of t_preamble  
                      * (t_ilist_kind * t_keyword * t_point_val option *
                         item list * t_keyword) 

type atom = Atom of t_preamble  
                    * (t_atom_kind * t_keyword * t_point_val option * t_title option * 
                       t_label option * t_depend option * 
                       t_atom_body * 
                       ilist option * 
                       t_hint option * t_refsol option * t_exp option * t_rubric option *   
                       t_keyword) 

type group = 
  Group of t_preamble 
           * (t_group_kind * t_keyword * t_point_val option * t_title option * t_label option * 
              atom list * t_tailtext * t_keyword) 

type problem_cluster = 
  ProblemCluster of t_preamble 
           * (t_keyword * t_title option * t_label option * 
              atom list * t_tailtext * t_keyword) 

type chapter = 
  Chapter of t_preamble
             *  (t_keyword * t_point_val option * t_title * t_label * 
                 block * paragraph list * section list) 

and section = 
  Section of (t_keyword * t_point_val option * t_title * t_label option *
              block * paragraph list * subsection list)

and subsection = 
  Subsection of (t_keyword * t_point_val option * t_title * t_label option *
                 block * paragraph list * subsubsection list)

and subsubsection = 
  Subsubsection of (t_keyword * t_point_val option * t_title * t_label option *
                    block * paragraph list)

and paragraph = 
  Paragraph of (t_keyword * t_point_val option * t_title * t_label option * block)

and block = Block of (element list * t_tailtext)

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

let mk_pval_str pvalopt= 
  match pvalopt with 
  |  None -> "0.0"
  |  Some x -> Float.to_string x
 
let fix_pval pval_opt pvalsum_opt = 
  (* If no pval is declared, pass the sum out. 
   * Otherwise, use the declared point value.
   *)  
    let _ = d_printf "fix_pval: pval_opt = %s pval_sum_opt = %s" 
                     (mk_pval_str pval_opt) 
                     (mk_pval_str pvalsum_opt) 
    in
      match (pval_opt, pvalsum_opt) with
      | (Some p, _) -> p
      | (None, None) -> 0.0
      | (None, Some p) -> p

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
  let pts_total_opt = List.reduce pvals (fun x y -> x +. y) in
    (pts_total_opt, xs)  

let pval_opts_sum pv_a pv_b = 
  match (pv_a, pv_b) with 
  | (None, None) -> None
  | (None, Some p) -> Some p
  | (Some p, None) -> Some p
  | (Some p_a, Some p_b) -> Some (p_a +. p_b)

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
    if contains_substring correct_choice_indicator keyword then
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

let mktex_section_heading name pvalopt t = 
  let b = "\\" ^ name in
  let p = match pvalopt with 
          | None -> ""
          | Some pts -> if pts = 0.0 then ""
                        else mktex_optarg (Float.to_string pts)
  in 
    b ^ p ^ "{" ^ t ^ "}" ^ "\n"

let mktex_begin block_name pvalopt topt = 
  let b = "\\begin{" ^ block_name ^ "}" in
  let p = match pvalopt with 
          | None -> ""
          | Some pts -> if pts = 0.0 then ""
                        else mktex_optarg (Float.to_string pts)
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
  let heading = "\\help" in
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

let rubricOptToTex rubric_opt = 
  let heading = "\\rubric" in
  let r = match rubric_opt with 
              |  None -> ""
              |  Some x -> 
                 (d_printf "rubricOptToTex: rubric = %s" x; 
                  heading ^ "\n" ^ x)
  in
     r

let itemToTex (Item(keyword, pval, body)) = 
  match pval with 
  | None -> keyword ^ body
  | Some p -> keyword ^ (pointvalToTex p) ^ space ^ body

let ilistToTex (IList(preamble, (kind, h_begin, pval_opt, itemslist, h_end))) = 
  let h_begin = mktex_begin kind pval_opt None in
  let il = List.map itemslist itemToTex in
  let ils = String.concat ~sep:"" il in
    preamble ^ h_begin ^ ils ^ h_end
      
let atomToTex (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))) = 
  let label = labelOptToTex lopt in
  let depend = dependOptToTex dopt in
  let hint = hintOptToTex hint_opt in
  let refsol = refsolOptToTex refsol_opt in
  let exp = expOptToTex exp_opt in
  let rubric = rubricOptToTex rubric_opt in
    match ilist_opt with 
    | None -> 
      let _ = d_printf "atomToTex: h_begin = %s" h_begin in         
      let r =  preamble ^ h_begin ^ label ^ depend ^ body ^ hint ^ refsol ^ exp ^ rubric ^ h_end in 
(*      let _  = d_printf "atomToTex: atom =  %s" r in *)
        r
    | Some il ->
      let ils = ilistToTex il in 
        preamble ^ h_begin ^ label ^ depend ^ body ^ ils ^ hint ^ refsol ^ exp ^ rubric ^ h_end      

let groupToTex (Group(preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end))) = 
  let h_begin = mktex_begin kind pval_opt topt in
  let atoms = map_concat atomToTex ats in
  let label = labelOptToTex lopt in
    preamble ^
    h_begin ^ label ^ 
    atoms ^ tt ^ 
    h_end

let elementToTex b = 
  match b with
  | Element_Group g -> groupToTex g
  | Element_Atom a -> atomToTex a

let blockToTex (Block(es, tt)) = 
  let _ = d_printf "blockToTex" in
  let elements = map_concat elementToTex es in
    elements ^ tt

let paragraphToTex (Paragraph(heading, pval_opt, t, lopt, b)) = 
  let _ = d_printf "paragraphToTex, points = %s\n" (pval_opt_to_string pval_opt) in
  let heading = mktex_section_heading TexSyntax.kw_paragraph pval_opt t in
  let block = blockToTex b in
  let label = labelOptToTex lopt in
    heading ^ label ^ 
    block

let paragraphsToTex ps = 
  map_concat paragraphToTex ps

let subsubsectionToTex (Subsubsection (heading, pval_opt, t, lopt, b, ps)) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let label = labelOptToTex lopt in
  let heading = mktex_section_heading TexSyntax.kw_subsubsection pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs

let subsectionToTex (Subsection (heading, pval_opt, t, lopt, b, ps, ss)) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptToTex lopt in
  let heading = mktex_section_heading TexSyntax.kw_subsection pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs ^ nesteds

let sectionToTex (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptToTex lopt in
  let heading = mktex_section_heading TexSyntax.kw_section pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs ^ nesteds

let chapterToTex (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let sections = map_concat sectionToTex ss in
  let _ = d_printf "ast.chapterToTex: block = [begin: block] %s... [end: block] " block in
  let label = labelToTex l in
  let heading = mktex_section_heading TexSyntax.kw_chapter pval_opt t in
    preamble ^ 
    heading ^ label ^ 
    block ^ paragraphs ^ sections

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
let rubric_is_single_par = false
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

let explainOptToXml tex2html exp_opt = 
  fieldOptToXml tex2html explain_is_single_par exp_opt

let rubricOptToXml tex2html rubric_opt = 
  fieldOptToXml tex2html rubric_is_single_par rubric_opt

let titleOptToXml tex2html topt = 
  fieldOptToXml tex2html title_is_single_par topt


let hintOptToXml tex2html hint_opt = 
  fieldOptToXml tex2html hint_is_single_par hint_opt

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

let ilistOptToXml tex2html ilist_opt = 
  match ilist_opt with 
  | None -> None 
  | Some (IList (preamble, (kind, h_begin, pval_opt, itemslist, h_end))) ->
    let _ = d_printf "IListToXml: kind = %s\n" kind in 
    let pval_str_opt = pval_opt_to_string_opt pval_opt in
    let items_xml = map_concat  (itemToXml tex2html) itemslist in
    let r = XmlSyntax.mk_ilist ~kind:kind 
                               ~pval:pval_str_opt
                               ~body:items_xml
    in
      Some r


let atomToXml tex2html
              (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))) = 
  let _ = d_printf "AtomToXml: kind = %s\n" kind in 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let dsopt = extract_depend dopt in
  let title_opt = titleOptToXml tex2html topt in
  let body_xml = tex2html (mk_index ()) body body_is_single_par in
  let ilist_xml_opt = ilistOptToXml tex2html ilist_opt in
  let hints_opt = hintOptToXml tex2html hint_opt in
  let refsols_opt = refsolOptToXml tex2html refsol_opt in
  let exps_opt = explainOptToXml tex2html exp_opt in
  let rubric_opt = rubricOptToXml tex2html rubric_opt in
  let r = XmlSyntax.mk_atom ~kind:kind 
                            ~pval:pval_str_opt
                            ~topt:title_opt
                            ~lopt:lsopt ~dopt:dsopt 
                            ~body_src:body
                            ~body_xml:body_xml
                            ~ilist_opt:ilist_xml_opt
                            ~hints_opt:hints_opt
                            ~refsols_opt:refsols_opt
                            ~explains_opt:exps_opt
                            ~rubric_opt:rubric_opt
   in
     r
     
let groupToXml tex2html
               (Group(preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end))) = 
  let lsopt = extract_label lopt in
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let title_opt = titleOptToXml tex2html topt in
  let atoms = map_concat (atomToXml tex2html) ats in
  let r = XmlSyntax.mk_group ~kind:kind ~pval:pval_str_opt ~topt:title_opt
                             ~lopt:lsopt ~body:atoms in
    r

let elementToXml tex2html b = 
  match b with
  | Element_Group g -> groupToXml tex2html g
  | Element_Atom a -> atomToXml  tex2html a


let paragraphToXml tex2html (Paragraph(heading, pval_opt, t, lopt, es, tt)) = 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let elements = map_concat (elementToXml tex2html) es in
  let r = XmlSyntax.mk_paragraph ~pval:pval_str_opt 
                                 ~title:t ~title_xml:t_xml
                                 ~lopt:lsopt ~body:elements in
    r

let blockToXml tex2html x = 
  match x with
  | Block_Element b -> elementToXml tex2html b
  | Block_Paragraph c -> paragraphToXml tex2html c

let subsubsectionToXml  tex2html (Subsubsection (heading, pval_opt, t, lopt, bs, tt)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let body = blocks in
  let r = XmlSyntax.mk_subsubsection ~pval:pval_str_opt
                                     ~title:t ~title_xml:t_xml
                                     ~lopt:lsopt ~body:body in
    r

let subsectionToXml  tex2html (Subsection (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsubsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsection ~pval:pval_str_opt 
                                  ~title:t ~title_xml:t_xml
                                  ~lopt:lsopt ~body:body in
    r

let sectionToXml  tex2html (Section (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let nesteds = map_concat (subsectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ nesteds in
  let r = XmlSyntax.mk_section ~pval:pval_str_opt 
                               ~title:t ~title_xml:t_xml
                               ~lopt:lsopt ~body:body in
    r

let chapterToXml  tex2html (Chapter (preamble, (heading, pval_opt, t, l, bs, tt, ss))) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let Label(heading, label) = l in 
  let _ = d_printf "chapter label, heading = %s  = %s\n" heading label in
  let t_xml = titleToXml tex2html t in
  let blocks = map_concat (blockToXml  tex2html) bs in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = blocks ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~pval:pval_str_opt 
                               ~title:t ~title_xml:t_xml 
                               ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)


(**********************************************************************
 ** BEGIN: AST ELABORATION
 ** Elaboration is currently very basic. 
 ** It traverses the code and calculates point scores for groups.
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
let rubricOptEl rubric_opt = 
  rubric_opt

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
  | Some (IList(preamble, (kind, h_begin, pval_opt, itemslist, h_end))) ->
      let itemslist = List.map itemslist itemEl in
        Some (IList(preamble, (kind, h_begin, pval_opt, itemslist, h_end)))
            
let atomEl (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))) = 
  let lopt = labelOptEl lopt in
  let dopt = dependOptEl dopt in
  let hint_opt = hintOptEl hint_opt in
  let refsol_opt = refsolOptEl refsol_opt in
  let exp_opt = expOptEl exp_opt in
  let rubric_opt = rubricOptEl rubric_opt in
  let ilist_opt = ilistOptEl ilist_opt in
  let pval = mk_pval pval_opt  in
    (pval, Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end)))


let groupEl (Group(preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end))) = 
  let _ = d_printf "groupEl: points = %s" (mk_pval_str pval_opt) in
  let (pvalsum_opt, ats) = map_and_sum_pts atomEl ats in
  let _ = d_printf "groupEl: points summed = %s" (mk_pval_str pvalsum_opt) in
  let lopt = labelOptEl lopt in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
    (pvalnew, Group (preamble, (kind, h_begin, Some pvalnew, topt, lopt, ats, tt, h_end)))

let elementEl b = 
  match b with
  | Element_Group g -> 
    let (pval, g) = groupEl g in 
      (pval, Element_Group g)
  | Element_Atom a -> 
    let (pval, a) = atomEl a in
      (pval, Element_Atom a)

let paragraphEl (Paragraph (heading, pval_opt, topt, lopt, es, tt)) = 
  let _ = d_printf "paragraphEl" in
  let (pvalsum_opt, es) = map_and_sum_pts elementEl es in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let _ = d_printf "paragraphEl: points = %s" (mk_pval_str pval_opt) in
  let lopt = labelOptEl lopt in
    (pvalnew, Paragraph  (heading, Some pvalnew, topt, lopt, es, tt))

let blockEl x = 
  let _ = d_printf "blockEl" in
    match x with
    | Block_Paragraph c -> 
      let (pval, b) = paragraphEl c in
        (pval, Block_Paragraph b)
    | Block_Element b -> 
      let (pval, b) = elementEl b in
        (pval, Block_Element b) 

let subsubsectionEl (Subsubsection (heading, pval_opt, t, lopt, bs, tt)) =
  let (pvalsum_opt, bs) = map_and_sum_pts blockEl bs in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let lopt = labelOptEl lopt in
    (pvalnew, Subsubsection (heading, Some pvalnew, t, lopt, bs, tt))

let subsectionEl (Subsection (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let (pval_1_opt, bs) = map_and_sum_pts blockEl bs in
  let (pval_2_opt, ss) = map_and_sum_pts subsubsectionEl ss in
  let pvalsum_opt = pval_opts_sum pval_1_opt pval_2_opt in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let lopt = labelOptEl lopt in
    (pvalnew, Subsection (heading, Some pvalnew, t, lopt, bs, tt, ss))

let sectionEl (Section (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let (pval_1_opt, bs) = map_and_sum_pts blockEl bs in
(*  let _ = d_printf "sectionEl: elements = %s" elements in *)
  let (pval_2_opt, ss) = map_and_sum_pts subsectionEl ss in
  let pvalsum_opt = pval_opts_sum pval_1_opt pval_2_opt in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let lopt = labelOptEl lopt in
    (pvalnew, Section (heading, Some pvalnew, t, lopt, bs, tt, ss))

let chapterEl (Chapter (preamble, (heading, pval_opt, t, l, bs, tt, ss))) =
  let (pval_1_opt, bs) = map_and_sum_pts blockEl bs in
  let (pval_2_opt, ss) = map_and_sum_pts sectionEl ss in
  let pvalsum_opt = pval_opts_sum pval_1_opt pval_2_opt in
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let l = labelEl l in
    (Chapter (preamble, (heading, Some pvalnew, t, l, bs, tt, ss)))

(**********************************************************************
 ** END: AST ELABORATION
 **********************************************************************)



(**********************************************************************
 ** BEGIN: AST  TRAVERSAL
 ** Unused, left here as a skeleton.  Elaboration above is an instance
 ** of traversal.
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
let rubricOptTR rubric_opt = 
  rubric_opt

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
  | Some (IList(preamble, (kind, h_begin, pval_opt, itemslist, h_end))) ->
      let itemslist = List.map itemslist itemTR in
        Some (IList(preamble, (kind, h_begin, pval_opt, itemslist, h_end)))
            
let atomTR (Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))) = 
  let dopt = dependOptTR dopt in
  let lopt = labelOptTR lopt in
  let hint_opt = hintOptTR hint_opt in
  let refsol_opt = refsolOptTR refsol_opt in
  let exp_opt = expOptTR exp_opt in
  let rubric_opt = rubricOptTR rubric_opt in
  let ilist_opt = ilistOptTR ilist_opt in
    Atom (preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))

let groupTR (Group(preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end))) = 
  let ats = map atomTR ats in
  let lopt = labelOptTR lopt in
    Group (preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end))

let elementTR b = 
  match b with
  | Element_Group g -> Element_Group (groupTR g)
  | Element_Atom a -> Element_Atom (atomTR a)

let paragraphTR (Paragraph(heading, pval_opt, t, lopt, es, tt)) = 
  let _ = d_printf "paragraphTR" in
  let es = map elementTR es in
  let lopt = labelOptTR lopt in
    Paragraph (heading, pval_opt, t, lopt, es, tt)

let blockTR x = 
  let _ = d_printf "blockTR" in
    match x with
    | Block_Paragraph c -> Block_Paragraph (paragraphTR c)
    | Block_Element b -> Block_Element (elementTR b)

let subsubsectionTR (Subsubsection (heading, pval_opt, t, lopt, bs, tt)) =
  let bs = map blockTR bs in
  let lopt = labelOptTR lopt in
    Subsubsection (heading, pval_opt, t, lopt, bs, tt)

let subsectionTR (Subsection (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let bs = map blockTR bs in
  let ss = map subsubsectionTR ss in
  let lopt = labelOptTR lopt in
    Subsection (heading, pval_opt, t, lopt, bs, tt, ss)

let sectionTR (Section (heading, pval_opt, t, lopt, bs, tt, ss)) =
  let bs = map blockTR bs in
(*  let _ = d_printf "sectionTR: elements = %s" elements in *)
  let ss = map subsectionTR ss in
  let lopt = labelOptTR lopt in
    Section (heading, pval_opt, t, lopt, bs, tt, ss)

let chapterTR (Chapter (preamble, (heading, pval_opt, t, l, bs, tt, ss))) =
  let bs = map blockTR bs in
  let ss = map sectionTR ss in
  let l = labelTR l in
    (Chapter (preamble, (heading, pval_opt, t, l, bs, tt, ss)))

(**********************************************************************
 ** END: AST TRAVERSAL
 **********************************************************************)
