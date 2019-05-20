open Core
open Utils

(**********************************************************************
 ** BEGIN: Constants
 *********************************************************************)
let newline = TexSyntax.newline
let space = TexSyntax.space
let correct_choice_indicator = TexSyntax.correct_choice_indicator

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
let str_match_prefix pattern s = 
  Str.string_match pattern s 0 

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

let section_pval pval_opt pvalsum_opt_b pvalsum_opt_ps = 
  (* If no pval is declared, pass the sum out. 
   * Otherwise, use the declared point value.
   *)  
    let _ = d_printf "section_pval: pval_opt = %s pval_sum_opt_b = %s pvalsum_opt_ps = %s" 
                     (mk_pval_str pval_opt) 
                     (mk_pval_str pvalsum_opt_b) 
                     (mk_pval_str pvalsum_opt_ps) 
    in
      match (pval_opt, pvalsum_opt_b, pvalsum_opt_ps) with
      | (Some p, _, _) -> p
      | (None, None, None) -> 0.0
      | (None, None, Some p_ps) -> p_ps
      | (None, Some p_b, None) -> p_b
      | (None, Some p_b, Some p_ps) -> p_b +. p_ps

let map f xs = 
  List.map xs f

let map_concat f xs: string = 
  let xs_s: string list = List.map xs f in
  let result = List.fold_left xs_s  ~init:"" ~f:(fun result x -> result ^ x) in
    result

let strListToStr (xs: string list): string = 
  String.concat ~sep:", " xs
(*
  let result = List.fold_left xs  ~init:"" ~f:(fun result x -> result ^ x) in
    result
*)

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

(* Take the title argument and returns
 *  title option * code language option * all the arguments in markdown
 *)
let process_title kind topt =   

  (* split the string of the form "key = value" to a (key, value) pair.
     strip white spaces around keys and values.
   *)
  let string_to_key_value kv = 
        let _ = d_printf "ast.extract_key_value input = %s\n" kv in
        (* split "key = value" pairs *)
        let k_and_v = Str.split (Str.regexp ("[ ]*=[ ]*")) kv in
          match k_and_v with 
          | x::[] -> (printf "FATAL ERROR in LaTeX to html translation: case 1 x = %s \n" x;
                      exit ErrorCode.parse_error_arg_expecting_key_value)
          | k::v::[] -> (d_printf "ast.extract_key_value: %s = %s\n" k v;
                         (String.strip k, String.strip v)) 
          | _ -> (printf "FATAL ERROR in LaTeX to html translation case 3\n";
                  exit ErrorCode.parse_error_arg_expecting_key_value)
  in
  let find_lang (kv: (string * string) list): string option = 
    try let (_, lang) = List.find_exn kv ~f:(fun (k, v) -> k = TexSyntax.language) in
          Some lang
    with Caml.Not_found -> None
  in
  let title_and_args (kvs: string list): string option * string option * string option = 

        (* Make a list of key value pairs *)
        let k_and_v_all = List.map kvs string_to_key_value in

        (* Language if any *)
        let lang_opt = find_lang k_and_v_all in  

        (* Split into title and other arguments *)
        let key_is_title (key, value) = (key = TexSyntax.kw_title) in
        let (title, args) =  List.partition_tf k_and_v_all key_is_title in

        (* Title *)
        let t_opt =  match title with 
                     | [] -> None 
                     | (k_title, v_title)::[] -> 
                       let _ = d_printf "ast.process_title: title = %s\n" v_title in
                         Some v_title
                     | _ -> (printf "FATAL ERROR in LaTeX to html translation\n";
                             exit ErrorCode.parse_error_multiple_titles)
        in
        (* Translate the rest of the arguments to markdown *)
        let arg_to_md (key, value) = 
          if (key = TexSyntax.language) then
            MdSyntax.mk_code_block_arg_indicate value
          else if (key = TexSyntax.numbers) then
            if value = TexSyntax.none then 
              ""
            else
              (* in Markdown we only indicate that lines will be numbered *)
              MdSyntax.mk_code_block_arg_indicate MdSyntax.numbers
          else if (key = TexSyntax.firstline) then
            MdSyntax.mk_code_block_arg MdSyntax.firstline value
          else 
            (* Pass the other arguments along *)
            MdSyntax.mk_code_block_arg key value
        in
        let arg_opt =            
          match args with 
          | [] -> None
          | kvs -> 
            (* translate argument to markdown *)
            let args = List.map kvs arg_to_md in
            let args = String.concat ~sep:" " args in
            let _ = d_printf "ast.process_title: new args = %s\n" args in
              Some args
         in
           (t_opt, lang_opt, arg_opt)
  in
  (* takes string of the form s = "part_a , part_b,   part_c"
   * splits the string into its parts
   * extracts key-value pairs from each part,
   * splits them into a title and other arguments
   *)
  let process_parts s = 
    let tokens = Str.split (Str.regexp ("[ ]*,[ ]*"))  s in
      (* splits the string at comma-space* 's.  
         if none is found, returns the whole string.
        *)
      match tokens with 
      | [] -> (printf "ast.process_title: FATAL ERROR in LaTeX to html translation.\n";
               exit ErrorCode.parse_error_arg_expecting_nonempty_string)
      | title::[] -> (d_printf "!ast.process_title: title only";
                      (Some title, None, None))
      | _ -> let _ = d_printf ("!ast.process_title: title has multiple parts") in
             let (topt, lang_opt, arg_opt) =  title_and_args tokens in
               (topt, lang_opt, arg_opt)
  in
    match topt with 
    | None -> (None, None, None)
    | Some t -> 
        if kind = TexSyntax.kw_code then
          let (topt, lang_opt, arg_opt) = process_parts t in 
            (topt, lang_opt, arg_opt)      
        else
          (topt, None, None)

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
              |  Some Depend(heading, ls) -> heading ^ (String.concat ~sep:", " ls) ^ "}" ^ "\n" 
  in
    r


(* Drop heading and consruct from label string *)
let labelToTex (Label(h, label_string)) = 
  ((TexSyntax.mkLabel label_string) ^ newline)

let labelOptToTex lopt = 
  let r = match lopt with 
              |  None -> ""
              |  Some label -> labelToTex label
  in
     r 

let pointvalToTex p = 
  mktex_optarg (Float.to_string p)

let pointvalOptToTex p = 
  match p with 
  | None -> ""
  | Some pts -> mktex_optarg (Float.to_string pts)

let hintOptToTex hint_opt = 
  let heading = TexSyntax.com_hint in
  let r = match hint_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r

let expOptToTex exp_opt = 
  let heading = TexSyntax.com_explain in
  let r = match exp_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r

let refsolOptToTex refsol_opt = 
  let heading = TexSyntax.com_solution in
  let r = match refsol_opt with 
              |  None -> ""
              |  Some x -> heading ^ "\n" ^ x  in
     r

let rubricOptToTex rubric_opt = 
  let heading = TexSyntax.com_rubric in
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
    if kind = TexSyntax.kw_code then
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
 ** BEGIN: AST LABELING
 **********************************************************************)

(* Assuming that the label has of the form 
   (prefix as e.g., [ch | sec | cl ]) (separator as [:]) label_name
   delete the prefix before the separator
 *)

let mkLabelPrefix label = 
  let tokens = Str.split (Str.regexp (":")) label in
  if List.length tokens <= 1 then
    (* label does not have a kind prefixer *)
    label
  else
    (* Split into two at the colon 
     * kind has the form Str.Delim "xyz[:]+"
     * rest has the form Str.Text  "xyz..." 
     *)

    let (Str.Delim kind)::(Str.Text rest)::nil = Str.bounded_full_split (Str.regexp "[A-Za-z]+[:]+") label 2 in
      if str_match_prefix TexSyntax.regexp_ch_prefix kind ||
         str_match_prefix TexSyntax.regexp_sec_prefix kind ||
         str_match_prefix TexSyntax.regexp_gr_prefix kind then
         rest
      else
        label

let findWord s = 
  (* Delete all latex commands *)
  let s = Str.global_replace (Str.regexp "\\\\[A-Za-z]+") "" s in
  (* Replace all non-alpha-numeric letters with space *)
  let s = Str.global_replace (Str.regexp "[^0-9^A-Z^a-z]+") " " s in

  (* Now split *)
  let tokens = Str.split (Str.regexp ("[ ]+")) s in
      (* splits the string at space* 's.  
         if none is found, returns the whole string.
        *)
  let tokens = List.map tokens  String.lowercase in 
  let _ = d_printf "findwork: tokens = %s" (strListToStr tokens) in
    match tokens with 
    | h::nil -> h
    | h:: t -> h

let addLabel table label = 
      try let _ = Hashtbl.find_exn table label  in
            (d_printf "ast.addLabel: Label = %s found in the table.\n" label;
             false)
      with Caml.Not_found -> 
        match Hashtbl.add table ~key:label ~data:() with
        | `Duplicate -> 
                    (printf "ast.addLabel: FATAL ERROR in Labeling.\n";
                     exit ErrorCode.labeling_error_hash_table_corrupted)
        | `Ok -> true

let createLabel kind prefix s = 
  let label = kind ^ TexSyntax.label_seperator ^ prefix ^ TexSyntax.label_seperator ^ s  in
  let heading = TexSyntax.mkLabel label in
    (heading, label) par

let labelSection table prefix (Section (heading, pval_opt, t, lopt, b, ps, ss)) =
(*
  let b = blockTR b in
  let ps = paragraphsTR ps in
  let ss = map subsectionTR ss in
*)
  let () = () in   
    match lopt with 
    | Some _ -> Section (heading, pval_opt, t, lopt, b, ps, ss)
    | None -> 
      let r = findWord t in
      let (heading, label) = createLabel TexSyntax.label_prefix_section prefix r in
      let () = 
        if addLabel table label then 
          (d_printf "ast.addLabel: Label = %s added to  the table.\n" label;
           ())
        else
          (d_printf "ast.addLabel: Label = %s found in the table.\n" label;
           ())
      in
      let lopt_new = Some (Label (heading, label)) in
        Section (heading, pval_opt, t, lopt_new, b, ps, ss)
      
let labelChapter (Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss))) =
  let labelTable = Hashtbl.create (module String) in

  let Label (_, ls) = l in

  let () = 
      try let _ = Hashtbl.find_exn labelTable ls  in
            (printf "ast.labelTable: FATAL ERROR in Labeling.\n";
             exit ErrorCode.labeling_error_hash_table_corrupted)
      with Caml.Not_found -> 
        match Hashtbl.add labelTable ~key:ls ~data:() with
        | `Duplicate -> 
                    (printf "ast.labelTable: FATAL ERROR in Labeling.\n";
                     exit ErrorCode.labeling_error_hash_table_corrupted)
        | `Ok -> () 
  in

  let prefix = mkLabelPrefix ls in
  let folder ss s = 
    let s_new = labelSection labelTable prefix s in
      ss @ [s_new]
  in             
  let ss_new = List.fold_left ss ~init:[] ~f:folder in
     Chapter (preamble, (heading, pval_opt, t, l, b, ps, ss_new))



(**********************************************************************
 ** END: AST LABELING
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
  let _ = d_printf "paragraphTR" in
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

