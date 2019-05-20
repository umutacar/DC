(**********************************************************************
 ** tex/texSyntax.ml
 **********************************************************************)
let newline = "\n"
let space = " "
let colon = ":"
let correct_choice_indicator = "*"
let label_seperator = colon
let label_prefix_section = "sec"

(* BEGIN: lstlisting arguments *)

let firstline = "firstline"
let language = "language"
let none = "none"
let numbers = "numbers"

(* END: lstlisting arguments *)

(* BEGIN: label prefixes *)
let kw_label_prefix_ch = "ch"
let kw_label_prefix_sec = "sec"
let kw_label_prefix_def = "def"

(* END: label prefixes *)

let com_explain = "\\explain"
let com_hint = "\\help"
let com_label = "\\label"
let com_rubric = "\\rubric"
let com_solution = "\\sol"


let kw_title = "title"

let kw_chapter = "chapter"
let kw_section = "section"
let kw_subsection = "subsection"
let kw_subsubsection = "subsubsection"
let kw_paragraph = "paragraph"

let kw_code = "code"

let mkArg arg = 
  "{" ^ arg ^ "}"
let mkLabel label = 
   com_label ^ (mkArg label)