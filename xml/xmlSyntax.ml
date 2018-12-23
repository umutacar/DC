(**********************************************************************
 ** xml/xmlSyntax.ml
 **********************************************************************)
open Core
open String
 
let cdata_begin = "<![CDATA["
let cdata_end = "]]>"
let equality = "="
let newline = "\n"
let quote = "'"
let space = " "
let empty_string = ""

(* Tags *) 

let tag_xml_version = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
(* These two are not used but for future reference. *) 
let tag_diderot_begin = "<diderot: xmlns = \"http://umut-acar.org/diderot\">"
let tag_diderot_end = "</diderot>"


let tag_atom = "atom"
let tag_block = "block"
let tag_field = "field"


(* Attributes *)
let attr_name = "name"

(* Attribute values *)
let answer = "answer"
let book = "book"
let chapter = "chapter"
let problem = "problem"
let section = "section"
let select = "select"
let subsection = "subsection"
let subsubsection = "subsubsection"

let algo = "algorithm"
let algorithm = "algorithm"
let authors = "authors"
let body = "body"
let body_src = "body_src"
let body_pop = "body_pop"
let choice = "choice"
let choice_src = "choice_src"
let code = "code"
let corollary = "corollary"
let cost_spec = "costspec"
let datastr = "datastr"
let datatype = "datatype"
let definition = "definition"
let example = "example"
let exercise = "exercise"
let friends = "friends"
let gram = "gram"
let group = "group"
let hint = "hint"
let important = "important"
let label = "label"
let lemma = "lemma"
let no = "no"
let note = "note"
let order = "order"
let page = "page"
let paragraph = "gram"
let parents = "parents"
let points = "points"
let preamble = "preamble"
let problem = "problem"
let proof = "proof"
let proposition = "proposition"
let rank = "rank"
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
let mk_comment (s) =
  "<!-- "^ s ^ " -->"

let mk_begin(tag) =
  "<" ^ tag ^ ">"

let mk_attr_val attr_name attr_val = 
  attr_name ^ equality ^ quote ^ attr_val ^ quote

let mk_begin_atom(kind) =
  "<" ^ tag_atom ^ space ^ (mk_attr_val attr_name kind) ^ quote ^ ">"

let mk_begin_block(kind) =
  "<" ^ tag_block ^ space ^ (mk_attr_val attr_name kind) ^ quote ^ ">"

let mk_begin_field(kind) =
  "<" ^ tag_field ^ space ^ (mk_attr_val attr_name kind) ^ quote ^ ">"

let mk_end(tag) =
  "</" ^ tag ^ ">"

let mk_end_atom(kind) =
  "</" ^ tag_atom ^ ">" ^ space ^ mk_comment(kind)

let mk_end_block(kind) =
  "</" ^ tag_block ^ ">" ^ space ^ mk_comment(kind)

let mk_end_field(kind) =
  "</" ^ tag_field ^ ">"^ space ^ mk_comment(kind)

let mk_cdata(body) =
  cdata_begin ^ newline ^ String.strip(body) ^ newline ^ cdata_end

(** END: Utilities
 **********************************************************************)


(**********************************************************************
 ** BEGIN: Field makers
 **********************************************************************)
let mk_field_generic(kind, contents) =
  let b = mk_begin_field(kind) in
  let e = mk_end_field(kind) in
  let result = b ^ newline ^ contents ^ newline ^ e in
    result

let mk_authors(x) = 
  mk_field_generic(authors, x)

let mk_body (x) = 
  mk_field_generic(body, mk_cdata x)

let mk_body_src (x) = 
  mk_field_generic(body_src, mk_cdata x)

let mk_body_pop (x) = 
  mk_field_generic(body_pop, mk_cdata(x))

let mk_hint (x) = 
  mk_field_generic(hint, x)

let mk_label(x) = 
  let _ = printf "mk_label: %s" x in 
    mk_field_generic(label, x)

let mk_label_opt(x) = 
  match x with
  | None -> mk_field_generic(label, Constants.no_label)
  | Some y -> mk_field_generic(label, y)

let mk_no(x) = 
  mk_field_generic(no, x)

let mk_parents(x) = 
  let p = String.concat ~sep:", " x in
    mk_field_generic(parents, p)

let mk_points(x) = 
  mk_field_generic(points, x)

let mk_solution (x) = 
  mk_field_generic(solution, x)

let mk_title(x) = 
  mk_field_generic(title, mk_cdata x)

let mk_title_src(x) = 
  mk_field_generic(title_src, mk_cdata x)

let mk_title_src_opt(x) = 
  match x with
  | None -> mk_field_generic(title_src, mk_cdata Constants.no_title)
  | Some y -> mk_field_generic(title_src, mk_cdata y)

let mk_title_opt(x) = 
  match x with
  | None -> mk_field_generic(title, mk_cdata Constants.no_title)
  | Some y -> mk_field_generic(title, mk_cdata y)

let mk_unique(x) = 
  mk_field_generic(unique, x)


(**********************************************************************
 ** END: Field makers
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Block makers
 **********************************************************************)
let mk_block_atom kind fields =
  let _ = printf "mk_block_atom: %s" kind in
  let b = mk_begin_atom(kind) in
  let e = mk_end_atom(kind) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ newline ^ y) in
    match result with 
    | None -> b ^ newline ^ e ^ newline
    | Some r -> b ^ newline ^ r ^ newline ^ e ^ newline


let mk_block_generic kind fields =
  let _ = printf "mk_block_generic: %s" kind in
  let b = mk_begin_block(kind) in
  let e = mk_end_block(kind) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ newline ^ y) in
    match result with 
    | None ->  b ^ newline ^ e ^ newline
    | Some r ->  b ^ newline ^ r ^ newline ^ e ^ newline

let mk_atom ~kind ~topt ~t_xml_opt ~lopt ~body_src ~body_xml = 
  let title_xml = mk_title_opt t_xml_opt in
  let title_src = mk_title_src_opt topt in
  let body_xml = mk_body body_xml in
  let body_src = mk_body_src body_src in
  let label_xml = mk_label_opt lopt in
    mk_block_atom kind [title_xml; title_src; label_xml; body_xml; body_src]

let mk_group ~topt ~t_xml_opt ~lopt ~body = 
  let title_src = mk_title_src_opt topt in
  let title_xml = mk_title_opt t_xml_opt in
  let label_xml = mk_label_opt lopt in
    mk_block_generic group [title_xml; title_src; label_xml; body]

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
    mk_block_generic chapter [title_xml; title_src; label_xml; body]





(**********************************************************************
 ** END: Block makers
 **********************************************************************)

(**********************************************************************
 ** Latex Translation 
 **********************************************************************)
let from_tex source = 
  source
