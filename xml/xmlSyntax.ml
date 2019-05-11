(**********************************************************************
 ** xml/xmlSyntax.ml
 **********************************************************************)
open Core
open Base
open String
open Utils


module C = XmlConstants

(* Tags *) 

let tag_xml_version = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
(* These two are not used but for future reference. *) 
let tag_diderot_begin = "<diderot: xmlns = \"http://umut-acar.org/diderot\">"
let tag_diderot_end = "</diderot>"


let tag_item = "item"
let tag_atom = "atom"
let tag_group = "group"
let tag_block = "block"
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
let checkbox = "checkbox"
let choice = "choice"
let choice_src = "choice_src"
let cluster = "cluster"
let code = "code"
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
let no = "no"
let note = "note"
let order = "order"
let page = "page"
let paragraph = "gram"
let parents = "parents"
let chooseany = "anychoice"
let chooseone = "xchoice"
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
let solution = "solution"
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

let mk_begin_block(name) =
  "<" ^ tag_block ^ C.space ^ (mk_attr_val attr_name name) ^ ">"

let mk_begin_block_with_kind name kind =
  "<" ^ tag_block ^ C.space ^ (mk_attr_val attr_name name) ^ C.space ^
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

let mk_end_block(name) =
  "</" ^ tag_block ^ ">" ^ C.space ^ mk_comment(name)

let mk_end_block_with_kind name kind =
  "</" ^ tag_block ^ ">" ^ C.space ^ mk_comment(name ^ "/" ^ kind)

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

let mk_depend_opt(x) = 
  match x with
  | None -> mk_field_generic(depend, C.no_depend)
  | Some y -> mk_field_generic(depend, y)


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


let mk_points_opt(x) = 
  match x with
  | None -> mk_field_generic(points, C.no_points)
  | Some y -> mk_field_generic(points, y)

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
 ** BEGIN: Block makers
 **********************************************************************)

let mk_block_atom kind fields =
  let _ = d_printf "mk_block_atom: %s" kind in
  let b = mk_begin_atom(kind) in
  let e = mk_end_atom(kind) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None -> b ^ C.newline ^ e ^ C.newline
    | Some r -> b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline

let mk_block_group kind fields =
  let _ = d_printf "mk_block_group: %s" kind in
  let b = mk_begin_group(kind) in
  let e = mk_end_group(kind) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None -> b ^ C.newline ^ e ^ C.newline
    | Some r -> b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline

let mk_block_generic name fields =
  let _ = d_printf "mk_block_generic: %s" name in
  let b = mk_begin_block(name) in
  let e = mk_end_block(name) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None ->  b ^ C.newline ^ e ^ C.newline
    | Some r ->  b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline

let mk_block_generic_with_kind name kind fields =
  let _ = d_printf "mk_block_generic: %s" name in
  let b = mk_begin_block_with_kind name kind in
  let e = mk_end_block_with_kind name kind in  
  let result = List.reduce fields (fun x -> fun y -> x ^ C.newline ^ y) in
    match result with 
    | None ->  b ^ C.newline ^ e ^ C.newline
    | Some r ->  b ^ C.newline ^ r ^ C.newline ^ e ^ C.newline

let mk_item ~pval ~body_src ~body_xml = 
  let label_xml = mk_label_opt None in
  let pval_xml = mk_point_value_opt pval in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
    mk_block_generic item [label_xml; pval_xml; body_xml; body_src] 

let mk_ilist ~kind ~pval ~body = 
  let kind_xml = ilist_kind_to_xml kind in
  let pval_xml = mk_point_value_opt pval in
  let label_xml = mk_label_opt None in
    mk_block_generic_with_kind ilist kind_xml [label_xml; pval_xml; body]

let mk_atom ~kind ~pval ~topt ~lopt ~dopt ~body_src ~body_xml ~ilist_opt ~hints_opt ~refsols_opt ~explains_opt= 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let label_xml = mk_label_opt lopt in
  let depend_xml = mk_depend_opt dopt in
  let fields_base = titles @ [label_xml; depend_xml; pval_xml; body_xml; body_src] in

  (* Now add in optional fields *)
  let hints = mk_hints_opt hints_opt in
  let refsols = mk_refsols_opt refsols_opt in
  let explains = mk_explains_opt explains_opt in
  let fields = fields_base @ hints @ refsols @ explains in
  let fields = append_opt ilist_opt fields in
    mk_block_atom kind fields

let mk_group ~kind ~pval ~topt ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let fields = titles @ [label_xml; pval_xml; body] in
    mk_block_group kind fields

let mk_cluster ~pval ~topt ~lopt ~body = 
  let pval_xml = mk_point_value_opt pval in
  let titles = mk_title_opt topt in
  let label_xml = mk_label_opt lopt in
  let fields = titles @ [label_xml; pval_xml; body] in
    mk_block_generic cluster fields


let mk_paragraph ~title ~title_xml ~lopt ~body = 
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
    mk_block_generic paragraph [title_xml; title_src; label_xml; body]

let mk_subsubsection ~title ~title_xml ~lopt ~body = 
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
    mk_block_generic subsubsection [title_xml; title_src; label_xml; body]

let mk_subsection ~title ~title_xml ~lopt ~body = 
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
    mk_block_generic subsection [title_xml; title_src; label_xml; body]

let mk_section ~title ~title_xml ~lopt ~body = 
  let title_src = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml = mk_label_opt lopt in
    mk_block_generic section [title_xml; title_src; label_xml; body]

let mk_chapter ~title ~title_xml ~label ~body = 
  let title_src:string = mk_title_src title in
  let title_xml = mk_title title_xml in
  let label_xml:string = mk_label label in
  let chapter_xml = mk_block_generic chapter [title_xml; title_src; label_xml; body] in 
    tag_xml_version ^ C.newline ^ chapter_xml


(**********************************************************************
 ** END: Block makers
 **********************************************************************)

(**********************************************************************
 ** Latex Translation 
 **********************************************************************)
let from_tex source = 
  source
