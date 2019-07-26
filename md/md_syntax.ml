(**********************************************************************
 ** md/md_syntax.ml
 ** TODO: This needs cleanup.  There is  a lot of unused definitions here.

 **********************************************************************)
open Core
open Utils

module Il = Tex_syntax

exception Md_syntax_error

(* Turn off prints *)
let d_printf args = 
    ifprintf stdout args
let d_printf_strlist x y = 
	()

let newline = "\n"
let space = " "
let colon = ":"
let correct_choice_indicator = "*"
let label_seperator = colon
let label_nestor = colon ^ colon


let pattern_hs = "[ \t]*"  
let pattern_ws = "[ \t\r\n]*"  
let pattern_begin env =
	"\\\\begin" ^ pattern_hs ^ "{" ^ 
	pattern_hs ^ env ^ pattern_hs ^ "}"

let pattern_end env =
	"\\\\end" ^ pattern_hs ^ "{" ^ 
	pattern_hs ^ env ^ pattern_hs ^ "}"



let pattern_label = "\\\\label" ^ pattern_ws ^ "{" ^ pattern_ws ^ 
	                  "[^} \r\t\n]*" ^ pattern_ws ^ "}"
let pattern_caption = "\\\\caption"

(* BEGIN: Keywords *)
let com_depend = "\\depend"
let com_explain = "\\explain"
let com_hint = "\\help"
let com_label = "\\label"
let com_notes = "\\notes"
let com_rubric = "\\rubric"
let com_solution = "\\sol"


let kw_title = "title"

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

(* END: Keywords *)

(* BEGIN: lstlisting arguments *)

let firstline = "firstline"
let language = "language"
let none = "none"
let numbers = "numbers"

(* END: lstlisting arguments *)

(*
	 (printf "FATAL ERROR: unknown kind encountered kind = %s.\n" kind;
   exit Error_code.labeling_error_unknown_atom)
*)
(* END: label prefixes *)
(* Utilities *)
let il_kind_of_segment kind = 
  if kind = kw_chapter then
		Il.kw_chapter
  else if kind = kw_section then
		Il.kw_section
  else if kind = kw_subsection then
		Il.kw_subsection
  else if kind = kw_subsubsection then
		Il.kw_subsubsection
  else if kind = kw_paragraph then
		Il.kw_paragraph
  else
    raise Md_syntax_error 


let md_of_il_segment s = 
  if s = Il.kw_chapter then
		kw_chapter
  else if s = Il.kw_section then
		kw_section
  else if s = Il.kw_subsection then
		kw_subsection
  else if s = Il.kw_subsubsection then
		kw_subsubsection
  else if s = Il.kw_paragraph then
		kw_paragraph
  else
    raise Md_syntax_error 


let mk_arg arg = 
  "{" ^ arg ^ "}"

let mk_opt_arg x = 
  "[" ^ x ^ "]"

let mk_point_val popt = 
  match popt with 
  |  None -> ""
  |  Some pts -> mk_opt_arg pts

let mk_label lopt = 
  match lopt with 
  |  None -> ""
  |  Some label -> ""

let mk_depend dopt = 
  let heading = com_depend in
	match dopt with 
  |  None -> ""
  |  Some ls -> ""

let mk_explain e = 
  match e with 
  |  None -> ""
  |  Some x -> ""

let mk_hint e = 
  match e with 
  |  None -> ""
  |  Some x -> ""

let mk_notes e = 
  match e with 
  |  None -> ""
  |  Some x -> ""

let mk_rubric e = 
  match e with 
  |  None -> ""
  |  Some x -> ""

let mk_title topt = 
  match topt with 
  |  None -> ""
  |  Some t -> t

let mk_command kind p = 
  ""

let mk_segment_header kind p t = 
  let kw =  md_of_il_segment kind in
    kw ^ space ^ t ^ newline

let mk_begin name p t = 
  ""

let mk_end kind = 
  ""


let mk_hint_opt hint_opt = 
  let heading = com_hint in
  match hint_opt with 
  |  None -> ""
  |  Some x -> ""

let mk_exp_opt exp_opt = 
  let heading = com_explain in
  match exp_opt with 
  |  None -> ""
  |  Some x -> ""

let refsol_opt refsol_opt = 
  let heading = com_solution in
  match refsol_opt with 
  |  None -> ""
  |  Some x -> ""

let rubric_opt rubric_opt = 
  let heading = com_rubric in
  match rubric_opt with 
  |  None -> ""
  |  Some x ->  ""

(**********************************************************************
 ** BEGIN: DEPRACATED
 **********************************************************************)
(** TODO EXTEND THIS TO SOMETHING LIKE
 ** IS_ATOMIC AND compare the kind against the atom keyword.
 ** IF NO MATCH, NOT ATOM.
 **)
(* Given contents string
 * check if contents has the "atomic" form
 * \begin{env} body \end{env}.
 * Atomic means that env does not occur within body.
 * (We don't allow for nesting of atoms.)
 * Return env and body if match occurs, None otherwise.
 *)
(*
let take_single_env contents = 
  match find_all_env contents with 
	| None -> None
	| Some (all_envs, _) ->      
			let uniques = uniques_of_list all_envs in
			let check env =
				let pb = pattern_begin env in
				let pe = pattern_end env in
				let mb = str_match_one_first pb contents in
				match mb with 
				| None -> None
				| Some (_, rest) ->
						let me = str_match_last pe rest in
						match me with 
						| None -> None
						| Some (se, body) ->
								let _ = d_printf "tex_syntax.take_env_body matched body: \n %s" body in
								Some (env, body)
			in
			let rec check_all uniques = 
				match uniques with 
				| [ ] -> None 
				| h::t -> 
						begin
							match check h with 
							| None -> check_all t
							| Some (env, body) -> Some (env, body)
						end
			in
			check_all uniques

*)
(**********************************************************************
 ** END: DEPRACATED
 **********************************************************************)


