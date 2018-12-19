(**********************************************************************
 ** xml/syntax.py
 **********************************************************************)
open Core
open String
 
let cdata_begin = "<![CDATA["
let cdata_end = "]]>"
let equality = "="
let missing = "__missing__"
let newline = "\n"
let quote = "'"
let space = " "


(* Tags *) 
let atom = "atom"
let block = "block"
let field = "field"

(* Block tags *)
let answer = "answer"
let book = "book"
let chapter = "chapter"
let problem = "problem"
let section = "section"
let select = "select"
let subsection = "subsection"
let subsubsection = "subsubsection"

(* Attribute values *)
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
let title_missing = "untitled"
let topics = "topics"
let unique = "unique"

(**********************************************************************
 ** BEGIN: Utilities
 **********************************************************************)
let mk_comment (s) =
  "<!-- "^ s ^ " -->"

let mk_str_begin(tag) =
  "<" ^ tag ^ ">"

let mk_str_begin_atom(name) =
  "<" ^ atom ^ space ^ name ^ equality ^ quote ^ name ^ quote ^ ">"

let mk_str_begin_block(name) =
  "<" ^ block ^ space ^ name ^ equality ^ quote ^ name ^ quote ^ ">"

let mk_str_begin_field(name) =
  "<" ^ field ^ space ^ name ^ equality ^ quote ^ name ^ quote ^ ">"

let mk_str_end(tag) =
  "</" ^ tag ^ ">"

let mk_str_end_atom(name) =
  "</" ^ atom ^ ">" ^ space ^ mk_comment(name)

let mk_str_end_block(name) =
  "</" ^ block ^ ">" ^ space ^ mk_comment(name)

let mk_str_end_field(name) =
  "</" ^ field ^ ">"^ space ^ mk_comment(name)

(* TODO = used? *)
let mk_xml_node(tag, text) = 
    mk_str_begin(tag) ^ newline ^ 
            String.strip(text) ^ newline ^ 
            mk_str_end(tag)

let mk_cdata(body) =
  cdata_begin ^ newline ^ String.strip(body) ^ newline ^ cdata_end

let mk_str_block_atom(name, fields) =
  let _ = printf "mk_str_block_atom: %s" name in
  let b = mk_str_begin_atom(name) in
  let e = mk_str_end_atom(name) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ newline ^ y) in
    match result with 
    | None -> b ^ newline ^ e
    | Some r -> b ^ newline ^ r ^ newline ^ e

let mk_str_block_generic(name, fields) =
  let _ = printf "mk_str_block_generic: %s" name in
  let b = mk_str_begin_block(name) in
  let e = mk_str_end_block(name) in  
  let result = List.reduce fields (fun x -> fun y -> x ^ newline ^ y) in
    match result with 
    | None ->  b ^ newline ^ e
    | Some r ->  b ^ newline ^ r ^ newline ^ e

let mk_str_field_generic(name, contents) =
  let b = mk_str_begin_field(name) in
  let e = mk_str_end_field(name) in
  let result = b ^ newline ^ contents ^ newline ^ e in
    result

(** END: Utilities
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Block makers
 **********************************************************************)
(* book maker *)
let mk_str_book (fields) =
  mk_str_block_generic(book, fields)

(* chapter maker *)
let mk_str_chapter (fields) = 
  mk_str_block_generic(chapter, fields)

let mk_str_group (fields) =
  mk_str_block_generic(group, fields)

let mk_str_section (fields) =
  mk_str_block_generic(section, fields)

let mk_str_subsection (fields) =
  mk_str_block_generic(subsection, fields)

let mk_str_subsubsection (fields) =
  mk_str_block_generic(subsubsection, fields)

let mk_str_atom (atom_name, fields) =
  mk_str_block_generic(atom_name, fields)

let mk_str_atom (atom_name, fields) =
  mk_str_block_atom(atom_name, fields)

let mk_str_algo (fields) =
  mk_str_block_generic(algo, fields)

(**********************************************************************
 ** END: Block makers
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Field makers
 **********************************************************************)

let mk_str_authors(x) = 
  mk_str_field_generic(authors, x)

let mk_str_body (x) = 
  mk_str_field_generic(body, mk_cdata(x))

let mk_str_body_src (x) = 
  mk_str_field_generic(body_src, mk_cdata(x))

let mk_str_body_pop (x) = 
  mk_str_field_generic(body_pop, x)

let mk_str_hint (x) = 
  mk_str_field_generic(hint, x)

let mk_str_label(x) = 
(*  print "label:", label *)
  mk_str_field_generic(label, x)

let mk_str_no(x) = 
  mk_str_field_generic(no, x)

let mk_str_parents(x) = 
  let p = String.concat ~sep:", " x in
    mk_str_field_generic(parents, p)

let mk_str_points(x) = 
  mk_str_field_generic(points, x)

let mk_str_solution (x) = 
  mk_str_field_generic(solution, x)

let mk_str_title(x) = 
  mk_str_field_generic(title, mk_cdata(x))

let mk_str_unique(x) = 
  mk_str_field_generic(unique, x)


(**********************************************************************
 ** END: Field makers
 **********************************************************************)

