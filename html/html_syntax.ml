(**********************************************************************
 ** html/html_syntax.ml
 **********************************************************************)
open Core
open Base
open String
open Utils

(* Turn off all prints *)
let d_printf args = 
    ifprintf stdout args

module C = Xml_constants


let h1 = "h1"
let h2 = "h2"
let h3 = "h3"
let h4 = "h4"
let h5 = "h5"
let h6 = "h6"
let strong = "strong"

(* Tags *) 


let tag_html_version = ""
(* These two are not used but for future reference. *) 
let tag_body_begin = "<body>"
let tag_body_end = "</body>"

let tag_div = "div"
let tag_section = "section"
let tag_segment = "segment"

(* Attributes *)
let attr_class = "class"
let attr_name = "name"
let attr_kind = "kind"
let attr_id = "id"

(* Attribute values *)
let level1 = "level1"
let level2 = "level2"
let level3 = "level3"
let level4 = "level4"
let level5 = "level5"
let level6 = "level6"
let level7 = "level7"

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
let item_tag = "tag"
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
let refsol_fillin_ask = "refsol_fillin_ask"
let refsol_fillin_sol = "refsol_fillin_sol"
let remark = "remark"
let rubric = "rubric"
let rubric_src = "rubric_src"
let solution = "solution"
let sound = "sound"
let strategy = "nestingmode"
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

let mk_tag_begin(tag, attributes) =
  let attributes = List.reduce attributes (fun x -> fun y -> x ^ C.space ^ y) in
  match attributes with 
	| None -> "<" ^ tag ^ ">"  
  | Some x -> "<" ^ tag ^ C.space ^ x ^ ">"  

let mk_end(tag) =
  "</" ^ tag ^ ">"

let mk_tag_end_named (tag, name) =
  "</" ^ tag ^ ">" ^ C.space ^ mk_comment(name)

let mk_tag(tag, attributes, body) =
  mk_tag_begin (tag, attributes) ^ body ^ mk_end(tag)

let mk_tag_named(tag, name, attributes, body) =
  mk_tag_begin (tag, attributes) ^ body ^ mk_tag_end_named(tag, name)


(**********************************************************************
 ** BEGIN: Field makers
 **********************************************************************)
let mk_field_generic(name, contents) =
  (* Strip white space around fields *)
  let contents =   String.strip ~drop:is_space contents in
  let result = name ^ "=" ^ C.dquote ^ contents ^ C.dquote  in
    result

let mk_field_id(label) =
  mk_field_generic(attr_id, label)

let mk_authors(x) = 
  mk_field_generic(authors, x)

let mk_body (x) = 
  mk_field_generic(body, x)

let mk_body_src (x) = 
  mk_field_generic(body_src, x)

let mk_body_pop (x) = 
  mk_field_generic(body_pop, x)

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
  mk_field_generic(explain, x)

let mk_explain_src (x) = 
  mk_field_generic(explain_src,  x)

let mk_hint (x) = 
  mk_field_generic(hint,  x)

let mk_hint_src (x) = 
  mk_field_generic(hint_src,  x)

let mk_label(x) = 
  let _ = d_printf "mk_label: %s" x in 
    mk_field_generic(attr_id, x)

let mk_label_opt(x) = 
  match x with
  | None -> mk_label(C.no_label)
  | Some y -> mk_label(y)

let mk_point_value_opt(x) = 
  match x with
  | None -> mk_field_generic(point_value, C.no_point_value)
  | Some y -> mk_field_generic(point_value, y)

let mk_strategy_opt(x) = 
  match x with
  | None -> mk_field_generic(strategy, C.no_strategy)
  | Some y -> mk_field_generic(strategy, y)

let mk_item_tag_opt(x) = 
  match x with
  | None -> mk_field_generic(item_tag, C.no_item_tag)
  | Some y -> mk_field_generic(item_tag, y)


let mk_no(x) = 
  mk_field_generic(no, x)

let mk_parents(x) = 
  let p = String.concat ~sep:", " x in
    mk_field_generic(parents, p)

let mk_solution (x) = 
  mk_field_generic(solution, x)

let mk_refsol (x) = 
  mk_field_generic(refsol,  x)

let mk_refsol_src (x) = 
  mk_field_generic(refsol_src,  x)

let mk_rubric (x) = 
  mk_field_generic(rubric,  x)

let mk_rubric_src (x) = 
  mk_field_generic(rubric_src,  x)

let mk_refsols_opt refsol_opt = 
    match refsol_opt with
    | None -> 
        let _ =  d_printf "html.mk_refsols_opt: refsol = None\n" in       
          []
    | Some (r_html, r_src) ->
        let _ =  d_printf "html.mk_refsols_opt: refsol_html = %s\n" r_html in       
        let refsol_html = mk_refsol r_html in
        let refsol_src = mk_refsol_src r_src in
          [refsol_html; refsol_src]

let mk_rubrics_opt rubrics_opt = 
    match rubrics_opt with
    | None -> 
        let _ =  d_printf "html.mk_rubrics_opt: rubric = None\n" in       
          []
    | Some (r_html, r_src) ->
        let _ =  d_printf "html.mk_rubrics_opt: rubrics_html = %s\n" r_html in       
        let rubric_html = mk_rubric r_html in
        let rubric_src = mk_rubric_src r_src in
          [rubric_html; rubric_src]


let mk_sound_opt(x) = 
  match x with
  | None -> mk_field_generic(sound, C.no_sound)
  | Some y -> mk_field_generic(sound, y)


let mk_title(x) = 
  mk_field_generic(title, x)

let mk_title_src(x) = 
  mk_field_generic(title_src, x)

let mk_title_src_opt(x) = 
  match x with
  | None -> mk_field_generic(title_src, C.no_title)
  | Some y -> mk_field_generic(title_src, y)

let mk_title_opt (x) = 
  match x with
  | None -> (C.no_title, C.no_title)
  | Some (t_html, t_src) -> (t_html, t_src)

let mk_caption(x) = 
  mk_field_generic(caption,  x)


let mk_caption_src(x) = 
  mk_field_generic(caption_src,  x)

let mk_caption_opt_html(x) = 
  match x with
  | None -> mk_field_generic(caption, C.no_caption)
  | Some (c_html, c_src) -> mk_field_generic(caption, c_html)

let mk_captions_opt x = 
  match x with
  | None -> 
    [mk_caption C.no_caption;
     mk_caption_src C.no_caption]
  | Some (c_html, c_src) -> 
    [mk_caption c_html;
     mk_caption_src c_src]

let mk_explains_opt x = 
  match x with
  | None -> [ ]    
  | Some (x_html, x_src) -> 
    [mk_explain x_html;
     mk_explain_src x_src]

let mk_hints_opt x = 
  match x with
  | None -> [ ]    
  | Some (x_html, x_src) -> 
    [mk_hint x_html;
     mk_hint_src x_src]

let mk_unique(x) = 
  mk_field_generic(unique, x)


(**********************************************************************
 ** END: Field makers
 **********************************************************************)

let level_of_segment (name) = 
	match name with 
	| "chapter" -> level1
	| "section" -> level2
	| "subsection" -> level3
	| "subsubsection" -> level4
	| "paragraph" -> level5
  | x -> (printf "Fatal Error: Unknown segment: %s" x; exit (1)) 

let mk_section_heading(name, title) = 
  match title with 
	| None -> ""
	| Some title -> 
			match name with 
			| "chapter" -> mk_tag (h1, [], title)
			| "section" -> mk_tag (h2, [], title)
			| "subsection" -> mk_tag (h3, [], title)
			| "subsubsection" -> mk_tag (h4, [], title)
			| "paragraph" -> mk_tag (h5, [], title)
      |  _ -> mk_tag (tag_div, [], title)

let mk_div_heading(title) = 
  match title with 
	| None -> ""
	| Some title -> mk_tag (strong, [], title)

let mk_begin_segment name topt fields =
  "<" ^ tag_segment ^ C.space ^ name ^ C.space ^ fields ^
  ">"

let mk_begin_segment_with_kind name kind =
  "<" ^ tag_segment ^ C.space ^ (mk_field_generic (attr_name, name)) ^ C.space ^
                              (mk_field_generic (attr_kind, kind)) ^ 
  ">"

let append_opt x_opt l = 
  match x_opt with 
  | None -> l
  | Some x -> List.append l [x]

(** END: Utilities
 **********************************************************************)


(**********************************************************************
 ** BEGIN: Segment makers
 **********************************************************************)

let mk_section name title fields body =
  let html_title = mk_section_heading(name, title) in
  let field_class = mk_field_generic (attr_class, level_of_segment name) in
  let fields = field_class::fields in
  let html_begin = mk_tag_begin (tag_section, fields)  in
  let html_end = mk_tag_end_named (tag_section, name) in
  html_begin ^ C.newline ^ html_title ^ C.newline ^ body ^ html_end

let mk_div kind title fields body =
  let html_title = mk_div_heading(title) in
  let field_class = mk_field_generic (attr_class, kind) in
  let fields = field_class::fields in
  let b = mk_tag_begin (tag_div, fields) in
  let e = mk_tag_end_named (tag_div, kind) in  
    b ^ C.newline ^ html_title ^ C.newline ^ body ^ C.newline ^ e ^ C.newline

let mk_cookie ~kind ~pval ~topt ~lopt ~tagopt ~dopt ~body_src ~body_html = 
  let pval_html = mk_point_value_opt pval in
  let label_html = mk_label_opt lopt in
  let tag_html = mk_item_tag_opt tagopt in
  let depend_html = mk_depend_opt dopt in
  let (title_html, title_src) = mk_title_opt topt in 
  let topt = Some title_html in
  let fields = [pval_html; label_html; tag_html; depend_html] in
    mk_div kind topt fields body_html

let mk_prompt ~kind ~pval ~topt ~lopt ~tagopt ~dopt ~body_src ~body_html ~cookies = 
  let pval_html = mk_point_value_opt pval in
  let label_html = mk_label_opt lopt in
  let tag_html = mk_item_tag_opt tagopt in
  let depend_html = mk_depend_opt dopt in
  let (title_html, title_src) = mk_title_opt topt in 
  let topt = Some title_html in
  let fields = [label_html; pval_html; tag_html; depend_html] in
  let body_and_cookies = body_html ^ C.newline ^ cookies in
    mk_div kind topt fields body_and_cookies

let mk_atom ~kind ~pval ~pl ~pl_version ~topt ~copt ~sopt ~lopt ~dopt ~body_src ~body_html ~capopt ~prompts = 
  let pval_html = mk_point_value_opt pval in
  let pl_html = mk_pl_opt pl in
  let pl_version_html = mk_pl_version_opt pl_version in
  let cover_html = mk_cover_opt copt in
  let sound_html = mk_sound_opt sopt in
  let label_html = mk_label_opt lopt in
  let depend_html = mk_depend_opt dopt in
  let (title_html, title_src) = mk_title_opt topt in 
  let topt = Some title_html in
  let fields = [label_html; pval_html; depend_html; cover_html; sound_html; pl_html; pl_version_html] in
  let body_and_prompts = body_html ^ C.newline ^ prompts in
    mk_div kind topt fields body_and_prompts

let mk_cluster ~kind ~topt fields ~body = 
  mk_div kind topt  fields body

let mk_segment ~kind ~pval ~topt ~strategy ~lopt ~dopt ~body = 
  let pval_html = mk_point_value_opt pval in
  let strategy_html = mk_strategy_opt strategy in
  let label_html = mk_label_opt lopt in
  let depend_html = mk_depend_opt dopt in
  let (title_html, title_src) = mk_title_opt topt in 
  let topt = Some title_html in
  let fields = [label_html; pval_html; strategy_html; depend_html] in
	match kind with 
	| "chapter" -> mk_section kind topt fields body
	| "section" -> mk_section kind topt fields body
	| "subsection" -> mk_section kind topt fields body
	| "subsubsection" -> mk_section kind topt fields body
	| "paragraph" -> mk_section kind topt fields body
  | _  -> mk_cluster kind topt fields body

let mk_standalone html = 
  tag_body_begin ^ C.newline ^ html ^ C.newline ^ tag_body_end

(**********************************************************************
 ** END: Segment makers
 **********************************************************************)

(**********************************************************************
 ** Latex Translation 
 **********************************************************************)
let from_tex source = 
  source
