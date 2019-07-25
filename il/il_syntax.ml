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

let segment_chapter = "0"
let segment_section = "1"
let segment_subsection = "2"
let segment_subsubsection = "3"
let segment_paragraph = "4"

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
  if kind = "#" then
		segment_chapter
  else if kind = "##" then
		segment_section
  else if kind = "###" then
		segment_subsection
  else if kind = "####" then
		segment_subsubsection
  else if kind = "#####" then
		segment_paragraph
  else
    raise Il_syntax_error 

(* is subseg nested in segment seg ? *)
let segment_is_nested subseg seg = 
  (int_of_string subseg) < (int_of_string segh)


