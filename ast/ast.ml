open Core
open Utils
module Tex = Tex_syntax

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)
let newline = Tex.newline
let space = Tex.space
let correct_choice_indicator = Tex.correct_choice_indicator

let points_correct = 1.0
let points_incorrect = 0.0

(**********************************************************************
 ** END: Constants
 **********************************************************************)

(**********************************************************************
 ** BEGIN: AST Data Types
*********************************************************************)


type t_label = Label of string
type t_depend = Depend of string list 
type t_point_val = float

module Atom  = 
struct
	type t = 
			{	kind: string;
				point_val: string option;
				title: string option;
				label: string option; 
				depend: string list option;
				body: string
			} [@@deriving fields]
end
type atom = Atom.t

module Group =
struct
	type t = 
			{	kind: string;
				point_val: string option;
				title: string option;
				label: string option; 
				depend: string list option;
				atoms: atom list
			} [@@deriving fields]
end

type group = Group.t

type element = 
  | Element_Group of atom
  | Element_Atom of group


module Segment =
struct
	type t = 
			{	kind: string;
				point_val: string option;
 				title: string option;
				label: string option; 
				block: element list;
				subsegments: t list
			} [@@deriving fields]
end
type segment = Segment.t

type ast = 
		Ast_Segment of segment 
	| Ast_Group of group
	| Ast_Atom of atom



(**********************************************************************
 ** END: AST Data Types
*********************************************************************)


(**********************************************************************
 ** BEGIN: Utilities
*********************************************************************)

let map f xs = 
  List.map xs f


let index = ref 0
let mk_index () = 
  let r = string_of_int !index in
  let _ = index := !index + 1 in
    r

(**********************************************************************
 ** END Utilities
 *********************************************************************)


let is_wellformed ast = 
  match ast with 
	| Ast_Atom a -> true
	| Ast_Group g -> true
	| Ast_Segment s -> 
			let wf = 
				List.reduce 
					(Segment.subsegments s) 
					(fun ss -> Tex.segment_is_nested ss s) 
			in
			match wf with 
				None -> true
			| Some flag -> flag


(*

(**********************************************************************
 ** BEGIN: AST To LaTeX
 **********************************************************************)

      
let atom_to_tex atom = 
	match atom with
 	{	kind: string;
		point_val: string option;
		title: string option;
    label: string option; 
		depend: string list option;
		body: string
	}

(Atom(preamble, (kind, h_begin, pval_opt, topt, lopt, dopt, body, ilist_opt, hint_opt, refsol_opt, exp_opt, rubric_opt, h_end))) = 
  let h_begin = mktex_header_atom kind pval_opt topt in
  let h_end = mktex_end kind in
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
  let h_end = mktex_end kind in
  let atoms = map_concat atomToTex ats in
  let label = labelOptToTex lopt in
    preamble ^ newline ^ (* extra newline for readability *)
    h_begin ^ label ^ 
    atoms ^ tt ^   (* Tailtext comes after atoms *)
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
  let heading = mktex_section_heading Tex.kw_paragraph pval_opt t in
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
  let heading = mktex_section_heading Tex.kw_subsubsection pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs

let subsectionToTex (Subsection (heading, pval_opt, t, lopt, b, ps, ss)) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let nesteds = map_concat subsubsectionToTex ss in
  let label = labelOptToTex lopt in
  let heading = mktex_section_heading Tex.kw_subsection pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs ^ nesteds

let sectionToTex (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let nesteds = map_concat subsectionToTex ss in
  let label = labelOptToTex lopt in
  let heading = mktex_section_heading Tex.kw_section pval_opt t in
    heading ^ label ^ 
    block ^ paragraphs ^ nesteds

let chapterToTex (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let block = blockToTex b in
  let paragraphs = paragraphsToTex ps in
  let sections = map_concat sectionToTex ss in
  let _ = d_printf "ast.chapterToTex: block = [begin: block] %s... [end: block] " block in
  let label = labelToTex l in
  let heading = mktex_section_heading Tex.kw_chapter pval_opt t in
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
let body_is_single_par = Tex2html.Generic false
let explain_is_single_par = Tex2html.Generic false
let hint_is_single_par = Tex2html.Generic false
let refsol_is_single_par = Tex2html.Generic false
let rubric_is_single_par = Tex2html.Generic false
let title_is_single_par = Tex2html.Generic true
let atom_is_code lang_opt arg_opt = Tex2html.Code (lang_opt, arg_opt)

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
  let (topt, lang_opt, atom_arg_opt) = process_title kind topt in
  let title_opt = titleOptToXml tex2html topt in
  let body_xml = 
    if kind = Tex.kw_code then
      tex2html (mk_index ()) body (atom_is_code lang_opt atom_arg_opt)
    else
      tex2html (mk_index ()) body body_is_single_par 
  in
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


let blockToXml tex2html (Block(es, tt)) =   
  let elements = map_concat (elementToXml tex2html) es in
    elements

let paragraphToXml tex2html (Paragraph(heading, pval_opt, t, lopt, b)) = 
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let block = blockToXml tex2html b in
  let body = block in
  let r = XmlSyntax.mk_paragraph ~pval:pval_str_opt 
                                 ~title:t ~title_xml:t_xml
                                 ~lopt:lsopt ~body:body in
    r


let paragraphsToXml tex2html ps = 
  map_concat (paragraphToXml tex2html) ps

let subsubsectionToXml  tex2html (Subsubsection (heading, pval_opt, t, lopt, b, ps)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let block = (blockToXml  tex2html) b in
  let paragraphs = paragraphsToXml tex2html ps in
  let body = block ^ newline ^ paragraphs in
  let r = XmlSyntax.mk_subsubsection ~pval:pval_str_opt
                                     ~title:t ~title_xml:t_xml
                                     ~lopt:lsopt ~body:body in
    r

let subsectionToXml  tex2html (Subsection (heading, pval_opt, t, lopt, b, ps, ss)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let block = (blockToXml  tex2html) b in
  let paragraphs = paragraphsToXml tex2html ps in
  let nesteds = map_concat (subsubsectionToXml  tex2html) ss in
  let body = block ^ newline ^ paragraphs ^ newline ^ nesteds in
  let r = XmlSyntax.mk_subsection ~pval:pval_str_opt 
                                  ~title:t ~title_xml:t_xml
                                  ~lopt:lsopt ~body:body in
    r

let sectionToXml  tex2html (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let lsopt = extract_label lopt in
  let t_xml = titleToXml tex2html t in
  let block = (blockToXml  tex2html) b in
  let paragraphs = paragraphsToXml tex2html ps in
  let nesteds = map_concat (subsectionToXml  tex2html) ss in
  let body = block ^ newline ^ paragraphs ^ newline ^ nesteds in
  let r = XmlSyntax.mk_section ~pval:pval_str_opt 
                               ~title:t ~title_xml:t_xml
                               ~lopt:lsopt ~body:body in
    r

let chapterToXml  tex2html (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let pval_str_opt = pval_opt_to_string_opt pval_opt in
  let Label(heading, label) = l in 
  let _ = d_printf "chapter label, heading = %s  = %s\n" heading label in
  let t_xml = titleToXml tex2html t in
  let block = (blockToXml  tex2html) b in
  let paragraphs = paragraphsToXml tex2html ps in
  let sections = map_concat (sectionToXml  tex2html) ss in
  let body = block ^ newline ^ paragraphs ^ newline ^ sections in
  let r = XmlSyntax.mk_chapter ~pval:pval_str_opt 
                               ~title:t ~title_xml:t_xml 
                               ~label:label ~body:body in
    r

(**********************************************************************
 ** END: AST To XML
 **********************************************************************)


(**********************************************************************
 ** BEGIN: AST ELABORATION
 ** Traverses the AST and 
 ** 1) calculates point scores for all nodes
 ** 2) makes sure that atoms are not orphans.
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
    (* This is an orphan atom.  Elaborate first, and then create a group for it. *)
    let (pval, a) = atomEl a in
    (* Create empty group. *)
    let preamble = "" in
    let kind = "cluster" in
    let pval_opt = Some pval in
    let topt = None in
    let h_begin = (mktex_begin kind pval_opt topt) in
    let lopt = None in  
    let ats = [a] in
    let tt = "" in
    let h_end = "\\end{cluster}" in
    let g = Group(preamble, (kind, h_begin, pval_opt, topt, lopt, ats, tt, h_end)) in
      (pval, Element_Group g)

let blockEl (Block (es, tt)) = 
  let _ = d_printf "blockEl" in
  let (pvalsum_opt, es_el) = map_and_sum_pts elementEl es in
    (pvalsum_opt, Block (es_el, tt))

let paragraphEl (Paragraph (heading, pval_opt, topt, lopt, b)) = 
  let _ = d_printf "paragraphEl" in
  let (pvalsum_opt, b) = blockEl b in   
  let pvalnew = fix_pval pval_opt pvalsum_opt in 
  let _ = d_printf "paragraphEl: points = %s" (mk_pval_str pval_opt) in
  let lopt = labelOptEl lopt in
    (pvalnew, Paragraph  (heading, Some pvalnew, topt, lopt, b))

let paragraphsEl ps = 
  map_and_sum_pts paragraphEl ps 

let subsubsectionEl (Subsubsection (heading, pval_opt, t, lopt, b, ps)) =
  let (pvalsum_opt_b, b) = blockEl b in
  let (pvalsum_opt_ps, ps) = paragraphsEl ps in 
  let pvalnew = section_pval pval_opt pvalsum_opt_b pvalsum_opt_ps in 
  let lopt = labelOptEl lopt in
    (pvalnew, Subsubsection (heading, Some pvalnew, t, lopt, b, ps))

let subsectionEl (Subsection (heading, pval_opt, t, lopt, b, ps, ss)) =
  let (pvalsum_opt_b, b) = blockEl b in
  let (pvalsum_opt_ps, ps) = paragraphsEl ps in 
  let (pvalsum_opt_ss, ss) = map_and_sum_pts subsubsectionEl ss in
  let pvalsum_opt_nested = pval_opts_sum pvalsum_opt_ps pvalsum_opt_ss in
  let pvalnew = section_pval pval_opt pvalsum_opt_b pvalsum_opt_nested in 
  let lopt = labelOptEl lopt in
    (pvalnew, Subsection (heading, Some pvalnew, t, lopt, b, ps, ss))

let sectionEl (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
  let (pvalsum_opt_b, b) = blockEl b in
  let (pvalsum_opt_ps, ps) = paragraphsEl ps in 
  let (pvalsum_opt_ss, ss) = map_and_sum_pts subsectionEl ss in
  let pvalsum_opt_nested = pval_opts_sum pvalsum_opt_ps pvalsum_opt_ss in
  let pvalnew = section_pval pval_opt pvalsum_opt_b pvalsum_opt_nested in 
  let lopt = labelOptEl lopt in
    (pvalnew, Section (heading, Some pvalnew, t, lopt, b, ps, ss))

let chapterEl (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let (pvalsum_opt_b, b) = blockEl b in
  let (pvalsum_opt_ps, ps) = paragraphsEl ps in 
  let (pvalsum_opt_ss, ss) = map_and_sum_pts sectionEl ss in
  let pvalsum_opt_nested = pval_opts_sum pvalsum_opt_ps pvalsum_opt_ss in
  let pvalnew = section_pval pval_opt pvalsum_opt_b pvalsum_opt_nested in 
  let l = labelEl l in
    (Chapter (preamble, (heading, Some pvalnew, t, l, b, ps, ss)))

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

let blockTR (Block(es, tt)) =
  let _ = d_printf "blockTR" in
  let es = map elementTR es in 
    Block(es, tt)

let paragraphTR (Paragraph(heading, pval_opt, t, lopt, b)) = 
  let _ = d_printf "paragraphTR\n" in
  let b = blockTR b in
  let lopt = labelOptTR lopt in
    Paragraph (heading, pval_opt, t, lopt, b)

let paragraphsTR ps = 
  map paragraphTR ps 

let subsubsectionTR (Subsubsection (heading, pval_opt, t, lopt, b, ps)) =
  let b = blockTR b in
  let ps = paragraphsTR ps in
  let lopt = labelOptTR lopt in
    Subsubsection (heading, pval_opt, t, lopt, b, ps)

let subsectionTR (Subsection (heading, pval_opt, t, lopt, b, ps, ss)) =
  let b = blockTR b in
  let ps = paragraphsTR ps in
  let ss = map subsubsectionTR ss in
  let lopt = labelOptTR lopt in
    Subsection (heading, pval_opt, t, lopt, b, ps, ss)

let sectionTR (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
  let b = blockTR b in
  let ps = paragraphsTR ps in
  let ss = map subsectionTR ss in
  let lopt = labelOptTR lopt in
    Section (heading, pval_opt, t, lopt, b, ps, ss)

let chapterTR (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let b = blockTR b in
  let ps = paragraphsTR ps in
  let ss = map sectionTR ss in
  let l = labelTR l in
    (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss)))

(**********************************************************************
 ** END: AST TRAVERSAL
 **********************************************************************)

*)
