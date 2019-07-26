(**********************************************************************
 ** il/il_syntax.ml
 ** TODO: This needs cleanup.  There is  a lot of unused definitions here.

 **********************************************************************)
open Core
open Utils

exception Il_syntax_error

(* Turn off prints *)
let d_printf args = 
    ifprintf stdout args
let d_printf_strlist x y = 
	()

let newline = "\n"
let space = " "
let correct_choice_indicator = "*"


let segment_chapter = "1"
let segment_section = "2"
let segment_subsection = "3"
let segment_subsubsection = "4"
let segment_paragraph = "5"

let kw_chapter = "#"
let kw_section = "##"
let kw_subsection = "###"
let kw_subsubsection = "####"
let kw_paragraph = "#####"

let kw_cluster = "cluster"
let kw_problem_cluster = "mproblem"
let kw_flex = "flex"

let kw_code = "code"
let kw_algorithm = "algorithm"
let kw_assumption = "assumption"
let kw_corollary = "corollary"
let kw_costspec = "costspec"
let kw_datastr = "datastr"
let kw_datatype = "datatype"
let kw_definition = "definition"
let kw_example = "example"
let kw_figure = "figure"
let kw_exercise = "exercise"
let kw_hint = "hint"
let kw_important = "important"
let kw_lemma = "lemma"
let kw_note = "note"
let kw_gram = "gram"
let kw_preamble = "preamble"
let kw_problem = "problem"
let kw_proof = "proof"
let kw_proposition = "proposition"
let kw_remark = "remark"
let kw_reminder = "reminder"
let kw_slide = "slide"
let kw_solution = "solution"
let kw_syntax = "syntax"
let kw_table = "table"
let kw_task = "task"
let kw_teachask = "teachask"
let kw_teachnote = "teachnote"
let kw_theorem = "theorem"

let kw_one_choice = "\\onechoice"
let kw_any_choice = "\\anychoice"
let kw_free_response = "\\freeresponse"
let kw_short_answer = "\\shortanswer"

let kw_choice = "\\choice"
let kw_choice_correct = "\\choice*"
let kw_part = "\\part"

(* BEGIN: Keywords *)
let com_depend = "\\depend"
let com_explain = "\\explain"
let com_hint = "\\help"
let com_label = "\\label"
let com_notes = "\\notes"
let com_rubric = "\\rubric"
let com_solution = "\\sol"


(* END: Keywords *)

(* BEGIN: lstlisting arguments *)
let firstline = "firstline"
let language = "language"
let none = "none"
let numbers = "numbers"
(* END: lstlisting arguments *)

(* BEGIN: Patterns Regular Expressions *)
let pattern_label_delimiter = "[:_]+"
let pattern_newline = "[ \n\r\x0c]+"
let pattern_ch_prefix = "ch"
let pattern_sec_prefix = "sec"
let pattern_gr_prefix = "gr"
let pattern_whitespace = "[ \n\r\x0c\t]+"
(* END: Regular Expressions *)


let problem_kinds = 
  [
   kw_one_choice, ();
   kw_any_choice, ();
   kw_short_answer, ();
   kw_free_response, ()
  ]

let prompt_kinds = 
  [
   kw_choice, ();
   kw_choice_correct, ();
   kw_part, ()
  ]


let cookie_kinds = 
  [
   com_explain, ();
   com_hint, ();
   com_notes, ();
   com_rubric, ();
   com_solution, ()
  ]

(* Utilities *)
let kind_of_segment kind = 
  if kind = kw_chapter then
		segment_chapter
  else if kind = kw_section then
		segment_section
  else if kind = kw_subsection then
		segment_subsection
  else if kind = kw_subsubsection then
		segment_subsubsection
  else if kind = kw_paragraph then
		segment_paragraph
  else
    raise Il_syntax_error 

(* is subseg nested in segment seg ? *)
let segment_is_nested subseg seg = 
  (int_of_string subseg) > (int_of_string seg)

let is_atom_captionable kind = 
	kind = kw_figure || kind = kw_table		

let is_cookie kind = 
   match List.Assoc.find cookie_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_problem kind = 
   match List.Assoc.find problem_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_prompt kind = 
   match List.Assoc.find prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false
