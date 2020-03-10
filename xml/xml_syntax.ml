(**********************************************************************
 ** xml/xml_syntax.ml
 **********************************************************************)
open Core
open Base
open String
open Utils

(* Turn off all prints *)
let d_printf args = 
    ifprintf stdout args

module C = Xml_constants

(* Tags *) 

let tag_xml_version = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
(* These two are not used but for future reference. *) 
let tag_diderot_begin = "<diderot: xmlns = \"http://umut-acar.org/diderot\">"
let tag_diderot_end = "</diderot>"


let tag_item = "item"
let tag_atom = "atom"
let tag_group = "group"
let tag_segment = "segment"
let tag_field = "field"
let tag_ilist = "ilist"

(* Attributes *)
let attr_name = "name"
let attr_kind = "kind"

(* Attribute values *)
let answer = "answer"
let book = "book"
let chapter = "chapter"
let problem = "problem"
let section = "section"
let select = "select"
let subsection = "subsection"
let subsubsection = "subsubsection"
let ilist = "ilist"
let item = "item"

let algo = "algorithm"
let algorithm = "algorithm"
let authors = "authors"
let body = "body"
let body_src = "body_src"
let body_pop = "body_pop"
let caption = "caption"
let caption_src = "caption_src"
let checkbox = "checkbox"
let choice = "choice"
let choice_src = "choice_src"
let cluster = "cluster"
let code = "code"
let cover = "cover"
let corollary = "corollary"
let cost_spec = "costspec"
let datastr = "datastr"
let datatype = "datatype"
let definition = "definition"
let depend = "depend"
let example = "example"
let exercise = "exercise"
let explain = "explain"
let explain_src = "explain_src"
let friends = "friends"
let gram = "gram"
let group = "group"
let hint = "hint"
let hint_src = "hint_src"
let important = "important"
let label = "label"
let lemma = "lemma"
let lstlisting = "lstlisting"
let no = "no"
let note = "note"
let order = "order"
let page = "page"
let paragraph = "paragraph"
let parents = "parents"
let chooseany = "anychoice"
let chooseone = "xchoice"
let pl = "pl"
let pl_version = "pl_version"
let points = "points"
let point_value = "point_value"
let preamble = "preamble"
let problem = "problem"
let proof = "proof"
let proposition = "proposition"
let radio = "radio"
let rank = "rank"
let refsol = "refsol"
let refsol_src = "refsol_src"
let remark = "remark"
let rubric = "rubric"
let rubric_src = "rubric_src"
let solution = "solution"
let sound = "sound"
let syntax = "syntax"
let task = "task"
let teach_ask = "teachask"
let teach_note = "teachnote"
let theorem = "theorem"
let title = "title"
let title_src = "title_src"
let topics = "topics"
let unique = "unique"

(**********************************************************************
 ** BEGIN: Utilities
 **********************************************************************)
let contains_substring search target =
  let _ = d_printf "contains_substring: search = %s target = %s\n" search target in
  let found = String.substr_index ~pos:0 ~pattern:search target in
  let res = 
    match found with 
    | None -> let _ = d_printf "contains_substring: found none\n" in false
    | Some x -> let _ = d_printf "contains_substring: found match %d\n" x in true 
  in
    res


let mk_comment (s) =
  "<!-- "^ s ^ " -->"

let mk_begin(tag) =
  "<" ^ tag ^ ">"

let mk_attr_val attr_name attr_val = 
  attr_name ^ C.equality ^ C.quote ^ attr_val ^ C.quote

(* TODO: unused/rm *)
let mk_begin_item(name) =
  "<" ^ tag_item ^ C.space ^ (mk_attr_val attr_name name) ^ ">"

let mk_begin_atom(kind) =
  "<" ^ tag_atom ^ C.space ^ (mk_attr_val attr_name kind) ^ ">"

let mk_begin_group(kind) =
  "<" ^ tag_group ^ C.space ^ (mk_attr_val attr_name kind) ^ ">"

let mk_begin_segment(name) =
  "<" ^ tag_segment ^ C.space ^ (mk_attr_val attr_name name) ^ ">"

let mk_begin_segment_with_kind name kind =
  "<" ^ tag_segment ^ C.space ^ (mk_attr_val attr_name name) ^ C.space ^
                              (mk_attr_val attr_kind kind) ^ 
  ">"

let mk_begin_field(name) =
  "<" ^ tag_field ^ C.space ^ (mk_attr_val attr_name name) ^ ">"

let mk_end(tag) =
  "</" ^ tag ^ ">"

let mk_end_atom(kind) =
  "</" ^ tag_atom ^ ">" ^ C.space ^ mk_comment(kind)

let mk_end_group(kind) =
  "</" ^ tag_group ^ ">" ^ C.space ^ mk_comment(kind)

let mk_end_segment(name) =
  "</" ^ tag_segment ^ ">" ^ C.space ^ mk_comment(name)

let mk_end_segment_with_kind name kind =
  "</" ^ tag_segment ^ ">" ^ C.space ^ mk_comment(name ^ "/" ^ kind)

let mk_end_field(name) =
  "</" ^ tag_field ^ ">"^ C.space ^ mk_comment(name)

let mk_cdata(body) =
  C.cdata_begin ^ C.newline ^ String.strip(body) 
                        ^ C.newline ^ 
  C.cdata_end

let ilist_kind_to_xml kind = 
  if contains_substring chooseany kind then
    checkbox
  else if contains_substring chooseone kind then
    radio
  else
    raise (Failure "xmlSyntax: Encountered IList of unknown kind")

let append_opt x_opt l = 
  match x_opt with 
  | None -> l
  | Some x -> List.append l [x]

(** END: Utilities
 **********************************************************************)


(**********************************************************************
 ** BEGIN: Field makers
 **********************************************************************)
let mk_field_generic(name, contents) =
  let b = mk_begin_field(name) in
  let e = mk_end_field(name) in
  (* Strip white space around fields *)
  let contents =   String.strip ~drop:is_space contents in
  let result = b ^ C.newline ^ contents ^ C.newline ^ e in
    result

let mk_authors(x) = 
  mk_field_generic(authors, x)

let mk_body (x) = 
  mk_field_generic(body, mk_cdata x)

let mk_body_src (x) = 
  mk_field_generic(body_src, mk_cdata x)

let mk_body_pop (x) = 
  mk_field_generic(body_pop, mk_cdata(x))

let mk_cover_opt(x) = 
  match x with
  | None -> mk_field_generic(cover, C.no_cover)
  | Some y -> mk_field_generic(cover, y)

let mk_depend_opt(x) = 
  match x with
  | None -> mk_field_generic(depend, C.no_depend)
  | Some y -> mk_field_generic(depend, y)

let mk_pl_opt(x) = 
  match x with
  | None -> mk_field_generic(pl, C.no_pl)
  | Some y -> mk_field_generic(pl, y)

let mk_pl_version_opt(x) = 
  match x with
  | None -> mk_field_generic(pl_version, C.no_pl_version)
  | Some y -> mk_field_generic(pl_version, y)

let mk_explain (x) = 
  mk_field_generic(explain, mk_cdata x)

let mk_explain_src (x) = 
  mk_field_generic(explain_src, mk_cdata x)

let mk_hint (x) = 
  mk_field_generic(hint, mk_cdata x)

let mk_hint_src (x) = 
  mk_field_generic(hint_src, mk_cdata x)

let mk_label(x) = 
  let _ = d_printf "mk_label: %s" x in 
    mk_field_generic(label, x)

let mk_label_opt(x) = 
  match x with
  | None -> mk_field_generic(label, C.no_label)
  | Some y -> mk_field_generic(label, y)

let mk_point_value_opt(x) = 
  match x with
  | None -> mk_field_generic(point_value, C.no_point_value)
  | Some y -> mk_field_generic(point_value, y)

let mk_no(x) = 
  mk_field_generic(no, x)

let mk_parents(x) = 
  let p = String.concat ~sep:", " x in
    mk_field_generic(parents, p)

let mk_solution (x) = 
  mk_field_generic(solution, x)

let mk_refsol (x) = 
  mk_field_generic(refsol, mk_cdata x)

let mk_refsol_src (x) = 
  mk_field_generic(refsol_src, mk_cdata x)

let mk_rubric (x) = 
  mk_field_generic(rubric, mk_cdata x)

let mk_rubric_src (x) = 
  mk_field_generic(rubric_src, mk_cdata x)

let mk_refsols_opt refsol_opt = 
    match refsol_opt with
    | None -> 
        let _ =  d_printf "xml.mk_refsols_opt: refsol = None\n" in       
          []
    | Some (r_xml, r_src) ->
        let _ =  d_printf "xml.mk_refsols_opt: refsol_xml = %s\n" r_xml in       
        let refsol_xml = mk_refsol r_xml in
        let refsol_src = mk_refsol_src r_src in
          [refsol_xml; refsol_src]

let mk_rubrics_opt rubrics_opt = 
    match rubrics_opt with
    | None -> 
        let _ =  d_printf "xml.mk_rubrics_opt: rubric = None\n" in       
          []
    | Some (r_xml, r_src) ->
        let _ =  d_printf "xml.mk_rubrics_opt: rubrics_xml = %s\n" r_xml in       
        let rubric_xml = mk_rubric r_xml in
        let rubric_src = mk_rubric_src r_src in
          [rubric_xml; rubric_src]


let mk_sound_opt(x) = 
  match x with
  | None -> mk_field_generic(sound, C.no_sound)
  | Some y -> mk_field_generic(sound, y)


let mk_title(x) = 
  mk_field_generic(title, mk_cdata x)

let mk_title_src(x) = 
  mk_field_generic(title_src, mk_cdata x)

let mk_title_src_opt(x) = 
  match x with
  | None -> mk_field_generic(title_src, mk_cdata C.no_title)
  | Some y -> mk_field_generic(title_src, mk_cdata y)

let mk_title_opt (x) = 
  match x with
  | None -> 
    [mk_title C.no_title;
     mk_title_src C.no_title]
  | Some (t_xml, t_src) -> 
    [mk_title t_xml;
     mk_title_src t_src]


let mk_caption(x) = 
  mk_field_generic(caption, mk_cdata x)

let mk_caption_src(x) = 
  mk_field_generic(caption_src, mk_cdata x)

let mk_caption_opt x = 
  match x with
  | None -> 
    [mk_caption C.no_caption;
     mk_caption_src C.no_caption]
  | Some (c_xml, c_src) -> 
    [mk_caption c_xml;
     mk_caption_src c_src]

let mk_explains_opt x = 
  match x with
  | None -> [ ]    
  | Some (x_xml, x_src) -> 
    [mk_explain x_xml;
     mk_explain_src x_src]

let mk_hints_opt x = 
  match x with
  | None -> [ ]    
  | Some (x_xml, x_src) -> 
    [mk_hint x_xml;
     mk_hint_src x_src]

let mk_unique(x) = 
  mk_field_generic(unique, x)


(**********************************************************************
 ** END: Field makers
 **********************************************************************)


(**********************************************************************
 ** BEGIN: Segment makers
 **********************************************************************)

let mk_segment_atom kind fields =
  let _ = d_printf "mk_segment_atom: %s" kind in
  let b = mk_begin_atom(kind) in
  let e = mk_end_atom(kind) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None -> b ^ C.newline ^ e ^ C.newline
    | Some r -> b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline


let mk_segment_generic name fields =
  let _ = d_printf "mk_segment_generic: %s" name in
  let b = mk_begin_segment(name) in
  let e = mk_end_segment(name) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None ->  b ^ C.newline ^ e ^ C.newline
    | Some r ->  b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline


let mk_segment_generic_with_kind name kind fields =
  let _ = d_printf "mk_segment_generic_with_kind: %s" name in
  let b = mk_begin_segment_with_kind name kind in
  let e = mk_end_segment_with_kind name kind in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None ->  b ^ C.newline ^ e ^ C.newline
    | Some r ->  b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline

let mk_item ~pval ~body_src ~body_xml = 
  let label_xml = mk_label_opt None in
  let pval_xml = mk_point_value_opt pval in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
    mk_segment_generic item [pval_xml; label_xml; body_xml; body_src] 

let mk_ilist ~kind ~pval ~body = 
  let kind_xml = ilist_kind_to_xml kind in
  let pval_xml = mk_point_value_opt pval in
  let label_xml = mk_label_opt None in
    mk_segment_generic_with_kind ilist kind_xml [pval_xml; label_xml; body]

let mk_cookie ~kind ~pval ~topt ~lopt ~dopt ~body_src ~body_xml = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let fields = [pval_xml] @ titles @ [label_xml; depend_xml; body_xml; body_src] in
    mk_segment_generic kind fields

let mk_prompt ~kind ~pval ~topt ~lopt ~dopt ~body_src ~body_xml = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let fields = [pval_xml] @ titles @ [label_xml; depend_xml; body_xml; body_src] in
    mk_segment_generic kind fields

let mk_problem ~kind ~pval ~topt ~lopt ~dopt ~body_src ~body_xml ~cookies ~prompts = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let fields = [pval_xml] @ titles @ [label_xml; depend_xml; body_src; body_xml; cookies; prompts] in
    mk_segment_generic kind fields

let mk_atom ~kind ~pval ~pl ~pl_version ~topt ~copt ~sopt ~lopt ~dopt ~body_src ~body_xml ~capopt ~problem_xml ~ilist_opt ~hints_opt ~refsols_opt ~explains_opt ~rubric_opt = 
  let pval_xml = mk_point_value_opt pval in
  let pl_xml = mk_pl_opt pl in
  let pl_version_xml = mk_pl_version_opt pl_version in
  let titles = mk_title_opt topt in
  let cover_xml = mk_cover_opt copt in
  let sound_xml = mk_sound_opt sopt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let captions = mk_caption_opt capopt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let fields_base = [pl_xml; pl_version_xml] @ titles @ [cover_xml; sound_xml; label_xml; depend_xml; pval_xml; body_xml; body_src; problem_xml] @ captions in
  (* Now add in optional fields *)
  let hints = mk_hints_opt hints_opt in
  let refsols = mk_refsols_opt refsols_opt in
  let explains = mk_explains_opt explains_opt in
  let rubrics = mk_rubrics_opt rubric_opt in
  let fields = fields_base @ hints @ refsols @ explains @ rubrics in
  let fields = append_opt ilist_opt fields in
    mk_segment_atom kind fields

let mk_group ~kind ~pval ~topt ~lopt ~dopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let fields = [pval_xml] @ titles @ [label_xml; depend_xml; body] in

(* We will use the kind of the group as a segment name.
   Because these are unique this creates no ambiguity.
   We know which segments are groups.
 *)
    mk_segment_generic kind fields

let mk_segment ~kind ~pval ~topt ~lopt ~dopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let fields = [pval_xml] @ titles @ [label_xml; depend_xml; body] in
	mk_segment_generic kind fields

let mk_paragraph ~pval ~title ~title_xml ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
  let fields = [pval_xml] @ [title_src; title_xml; label_xml; body] in
    mk_segment_generic paragraph fields

let mk_subsubsection ~pval ~title ~title_xml ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
  let fields = [pval_xml] @ [title_xml; title_src; label_xml; body] in
    mk_segment_generic subsubsection fields 

let mk_subsection ~pval ~title ~title_xml ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
  let fields = [pval_xml] @ [title_xml; title_src; label_xml; body] in
    mk_segment_generic subsection fields

let mk_section ~pval ~title ~title_xml ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
  let fields = [pval_xml] @ [title_xml; title_src; label_xml; body] in
    mk_segment_generic section fields

let mk_chapter ~pval ~title ~title_xml ~label ~body = 
  let pval_xml = mk_point_value_opt pval in
  let title_src:string = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml:string = mk_label label in
  let fields = [pval_xml] @ [title_xml; title_src; label_xml; body] in
  let chapter_xml = mk_segment_generic chapter fields in 
    tag_xml_version ^ C.newline ^ chapter_xml

let mk_standalone xml = 
  tag_xml_version ^ C.newline ^ xml

(**********************************************************************
 ** END: Segment makers
 **********************************************************************)

(**********************************************************************
 ** Latex Translation 
 **********************************************************************)
let from_tex source = 
  source
