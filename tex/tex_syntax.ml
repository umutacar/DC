(**********************************************************************
 ** tex/texSyntax.ml
 **********************************************************************)
open Core
open Utils

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
let com_label = "\\label"


let kw_title = "title"

let kw_chapter = "chapter"
let kw_section = "section"
let kw_subsection = "subsection"
let kw_subsubsection = "subsubsection"
let kw_paragraph = "paragraph"

let kw_cluster = "cluster"
let kw_problem_cluster = "mproblem"
let kw_flex = "flex"

let kw_code = "code"
let kw_algorithm = "algorithm"
let kw_answer = "answer"
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
let kw_observe = "observe"
let kw_observation = "observation"
let kw_preamble = "preamble"
let kw_problem = "problem"
let kw_proof = "proof"
let kw_proposition = "proposition"
let kw_question = "question"
let kw_quote = "quote"
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
let kw_free_response = "\\answer"
let kw_short_answer = "\\ans"
let kw_ask = "\\ask"
let kw_refsol = "\\sol"
let kw_refsol_fillin = "\\solfin"
let kw_refsol_fillin_ask = "\\solfin_ask"
let kw_refsol_fillin_sol = "\\solfin_answer"



let kw_choice = "\\choice"
let kw_choice_correct = "\\choice*"
let kw_part = "\\part"

let kw_cookie_explain = "\\explain"
let kw_cookie_hint = "\\hint"
let kw_cookie_notes = "\\notes"
let kw_cookie_rubric = "\\rubric"

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


(* BEGIN: label prefixes *)
let label_prefix_chapter = "ch"
let label_prefix_section = "sec"
let label_prefix_subsection = "sec"
let label_prefix_subsubsection = "sec"
let label_prefix_paragraph = "sec"

(* All groups the same *)
let label_prefix_cluster = "grp"
let label_prefix_problem_cluster = "grp"
let label_prefix_flex = "grp"

(* Atoms
 * give long prefixes to rare atoms.
 *)
let label_prefix_auto_pre = "_"
let label_prefix_auto_post = "_"

let label_prefix_algorithm = "alg"
let label_prefix_answer = "ans"
let label_prefix_assumption = "asm"
let label_prefix_code = "cd"
let label_prefix_corollary = "crl"
let label_prefix_costspec = "cst"
let label_prefix_datastr = "dtstr"
let label_prefix_datatype = "adt"
let label_prefix_definition = "def"
let label_prefix_example = "xmpl"
let label_prefix_exercise = "xrcs"
let label_prefix_figure = "fig"
let label_prefix_hint = "hint"
let label_prefix_important = "imp"
let label_prefix_lemma = "lem"
let label_prefix_note = "nt"
let label_prefix_gram = "grm"
let label_prefix_observe = "obs"
let label_prefix_observation = "obs"
let label_prefix_preamble = "prmbl"
let label_prefix_problem = "prb"
let label_prefix_proof = "prf"
let label_prefix_proposition = "prop"
let label_prefix_question = "que"
let label_prefix_quote = "qut"
let label_prefix_remark = "rmrk"
let label_prefix_reminder = "rmdr"
let label_prefix_slide = "slide"
let label_prefix_solution = "sol"
let label_prefix_syntax = "syn"
let label_prefix_table = "tbl"
let label_prefix_task = "tsk"
let label_prefix_teachask = "tch"
let label_prefix_teachnote = "tch"
let label_prefix_theorem = "thm"
let label_prefix_one_choice = "prt-one-choice"
let label_prefix_any_choice = "prt-any-choice"
let label_prefix_ask = "prt-ask"
let label_prefix_short_answer = "prt-ans"
let label_prefix_free_response = "prt-answer"
let label_prefix_refsol = "prt-sol"
let label_prefix_choice = "prt-choice"
let label_prefix_cookie_explain = "cki-explain"
let label_prefix_cookie_hint = "cki-hint"
let label_prefix_cookie_notes = "cki-notes"
let label_prefix_cookie_rubric = "cki-rubric"


let label_prefix_of_kind = 
  [
   kw_cluster, label_prefix_cluster;
   kw_flex, label_prefix_flex;
   kw_problem_cluster, label_prefix_problem_cluster
  ]
  @
  [
   kw_chapter, label_prefix_chapter;
   kw_section, label_prefix_section;
   kw_subsection, label_prefix_subsection;
   kw_subsubsection, label_prefix_subsubsection;
   kw_paragraph, label_prefix_paragraph;
  ]
  @
  (* atoms *)
  [kw_algorithm, label_prefix_algorithm;
   kw_answer, label_prefix_answer;
   kw_assumption, label_prefix_assumption;
   kw_code, label_prefix_code;
   kw_corollary, label_prefix_corollary;
   kw_costspec, label_prefix_costspec;
   kw_datastr, label_prefix_datastr;
   kw_datatype, label_prefix_datatype;
   kw_definition, label_prefix_definition;
   kw_example, label_prefix_example;
   kw_exercise, label_prefix_exercise;
   kw_figure, label_prefix_figure;
   kw_hint, label_prefix_hint;
   kw_important, label_prefix_important;
   kw_lemma, label_prefix_lemma;
   kw_note, label_prefix_note;
   kw_gram, label_prefix_gram;
   kw_observe, label_prefix_observe;
   kw_observation, label_prefix_observation;
   kw_preamble, label_prefix_preamble;
   kw_problem, label_prefix_problem;
   kw_proof, label_prefix_proof;
   kw_proposition, label_prefix_proposition;
   kw_quote, label_prefix_quote;
   kw_question, label_prefix_question;
   kw_remark, label_prefix_remark;
   kw_reminder, label_prefix_reminder;
   kw_slide, label_prefix_slide;
   kw_solution, label_prefix_solution;
   kw_syntax, label_prefix_syntax;
   kw_table, label_prefix_table;
   kw_task, label_prefix_task;
   kw_teachask, label_prefix_teachask;
   kw_teachnote, label_prefix_teachnote;
   kw_theorem, label_prefix_theorem
  ] 
  @
  (* Prompts *)
  [
   kw_one_choice, label_prefix_one_choice;
   kw_any_choice, label_prefix_any_choice;
   kw_free_response, label_prefix_free_response;
   kw_ask, label_prefix_ask;
   kw_short_answer, label_prefix_short_answer;
   kw_choice, label_prefix_choice;
   kw_choice_correct, label_prefix_choice;
   kw_refsol, label_prefix_refsol;
   kw_refsol_fillin_ask, label_prefix_refsol;
   kw_refsol_fillin_sol, label_prefix_refsol;
  ]
  @
  (* Cookies *)
  [
   kw_cookie_explain, label_prefix_cookie_explain;
   kw_cookie_hint, label_prefix_cookie_hint;
   kw_cookie_notes, label_prefix_cookie_notes;
   kw_cookie_rubric, label_prefix_cookie_rubric
]

let atom_kinds = 
  [kw_algorithm, ();
   kw_answer, ();
   kw_assumption, ();
   kw_code, ();
   kw_corollary, ();
   kw_costspec, ();
   kw_datastr, ();
   kw_datatype, ();
   kw_definition, ();
   kw_example, ();
   kw_exercise, ();
   kw_figure, ();
   kw_hint, ();
   kw_important, ();
   kw_lemma, ();
   kw_note, ();
   kw_gram, ();
   kw_observe, ();
   kw_observation, ();
   kw_preamble, ();
   kw_problem, ();
   kw_proof, ();
   kw_proposition, ();
   kw_question, ();
   kw_quote, ();
   kw_remark, ();
   kw_reminder, ();
   kw_slide, ();
   kw_solution, ();
   kw_syntax, ();
   kw_table, ();
   kw_task, ();
   kw_teachask, ();
   kw_teachnote, ();
   kw_theorem, ()
  ]

let primary_choice_prompt_kinds = 
  [
   kw_one_choice, ();
   kw_any_choice, ()
  ]

let correct_choice_prompt_kinds = 
  [
   kw_choice_correct, ();
  ]

let secondary_choice_prompt_kinds = 
  [
   kw_choice, ();
   kw_choice_correct, ();
  ]


let primary_prompt_kinds = 
	primary_choice_prompt_kinds
	@
  [
   kw_ask, ();
   kw_short_answer, ();
   kw_free_response, ()
  ]

let scorable_prompt_kinds = 
  [
   kw_choice, ();
   kw_choice_correct, ();
   kw_refsol, ();
   kw_refsol_fillin_ask, ();
  ]

let prompt_kinds = 
  primary_prompt_kinds @
  [
   kw_choice, ();
   kw_choice_correct, ();
   kw_part, ();
   kw_refsol, ();
   kw_refsol_fillin_ask, ();
   kw_refsol_fillin_sol, ();
  ]

let cookie_kinds = 
  [
   kw_cookie_explain, ();
   kw_cookie_hint, ();
   kw_cookie_notes, ();
   kw_cookie_rubric, ();
  ]

let cookie_cost_ratio = 
  [
   kw_cookie_explain, Constants.cookie_cost_explain;
   kw_cookie_hint, Constants.cookie_cost_hint;
   kw_cookie_notes, Constants.zero_cost;
   kw_cookie_rubric, Constants.zero_cost;
  ]

let point_value_of_prompt_kind = 
  [
   kw_refsol, "1.0";
   kw_refsol_fillin_ask, "1.0";
   kw_choice, "0.0";
   kw_choice_correct, "1.0";
  ]

let point_value_of_prompt kind =
   match List.Assoc.find point_value_of_prompt_kind ~equal:String.equal kind with 
   | Some x -> x
   | None -> 
			 let err = Printf.sprintf "Fatal Error: Encountered unknown prompt: %s\n"  kind in
			 raise (Constants.Fatal_Error err) 

(* Given a segment kind, assign a label prefix, e.g.,
 * section -> sec
 * example -> xmpl.

 * If the kind is not found, then use the name of the kind as a prefix, e.g.
 *  defn -> defn.
 *)  
let mk_label_prefix_from_kind kind = 
   match List.Assoc.find label_prefix_of_kind ~equal:String.equal kind with 
   | Some prefix -> prefix
   | None -> kind
(*
	 (printf "FATAL ERROR: unknown kind encountered kind = %s.\n" kind;
   exit Error_code.labeling_error_unknown_atom)
*)
(* END: label prefixes *)


(* Utilities *)
let mk_arg arg = 
  "{" ^ arg ^ "}"

let mk_opt_arg x = 
  "[" ^ x ^ "]"

let mk_opt_arg_normal x = 
	if x = "" then
		""
	else
		"[" ^ x ^ "]"


let mk_point_val popt = 
  match popt with 
  |  None -> ""
  |  Some pts -> mk_opt_arg pts

let mk_kw_args l =   
  let l = List.filter l ~f:(fun (k, v) -> match v with | None -> false | Some _ -> true) in
  let mapper (k, v) = 
    match v with 
    | None -> "FATAL error"
    | Some v -> k ^ " = " ^ v
  in
  if List.length l = 0 then
		""
	else
		let l = List.map l ~f:mapper in
		String.concat ~sep:"; " l

let mk_caption capopt = 
  match capopt with 
	| None -> ""
	| Some body -> 
			let result = "\\caption{" ^ body ^ "}\n" in
			let _ = d_printf "caption = %s" result in
			result

let mk_label lopt = 
  match lopt with 
  |  None -> ""
  |  Some label -> 
			let l = com_label ^ (mk_arg label) in
			l ^ newline

let mk_depend dopt = 
  let heading = com_depend in
	match dopt with 
  |  None -> ""
  |  Some ls -> heading ^ (mk_arg (String.concat ~sep:", " ls)) ^ "\n" 

let mk_explain e = 
  match e with 
  |  None -> ""
  |  Some x ->
			let l = kw_cookie_explain ^ newline ^ x in
			l ^ newline

let mk_hint e = 
  match e with 
  |  None -> ""
  |  Some x ->
			let l = kw_cookie_hint ^ newline ^ x in
			l ^ newline

let mk_notes e = 
  match e with 
  |  None -> ""
  |  Some x ->
			let l = kw_cookie_notes ^ newline ^ x in
			l ^ newline

let mk_rubric e = 
  match e with 
  |  None -> ""
  |  Some x ->
			let l = kw_cookie_rubric ^ newline ^ x in
			l ^ newline

let mk_title topt = 
  match topt with 
  |  None -> ""
  |  Some t -> t

let mk_command kind p = 
  kind ^ p

let mk_segment_header kind p t = 
  let b = "\\" ^ kind in
    b ^ p ^ (mk_arg t) ^ "\n"

let mk_begin name p t = 
  let b = "\\begin" ^ (mk_arg name) in
	b ^  p ^ (mk_opt_arg_normal t) ^ "\n"

let mk_begin_atom name p t kw_args = 
  let b = "\\begin" ^ (mk_arg name)  in  
  if kw_args = "" then
		b ^  p ^  (mk_opt_arg_normal t) ^ (mk_opt_arg_normal kw_args) ^ "\n"
	else
    (* If kw_args is nonempty, title brackets must exist. 
     * Example \begin{atom}[][sound=voice.mpg] 
     *)
		b ^  p ^  (mk_opt_arg t) ^ (mk_opt_arg_normal kw_args) ^ "\n"

let mk_end kind = 
  "\\end" ^ (mk_arg kind) ^ "\n"


let mk_hint_opt hint_opt = 
  let heading = kw_cookie_hint in
  match hint_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  

let mk_exp_opt exp_opt = 
  let heading = kw_cookie_explain in
  match exp_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  

let refsol_opt refsol_opt = 
  let heading = kw_refsol in
  match refsol_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  

let rubric_opt rubric_opt = 
  let heading = kw_cookie_rubric in
  match rubric_opt with 
  |  None -> ""
  |  Some x -> 
      (d_printf "rubricOptToTex: rubric = %s" x; 
       heading ^ "\n" ^ x)

let is_atom_captionable kind = 
	kind = kw_figure || kind = kw_table 		


let is_group kw = 
  kw = kw_cluster ||
  kw = kw_flex ||
  kw = kw_problem_cluster 

(* is subseg nested in segment seg ? *)
let segment_is_nested subseg seg = 
  if seg = kw_chapter then
		subseg = kw_section ||
		subseg = kw_subsection ||
    subseg = kw_subsubsection ||
    subseg = kw_paragraph
  else if seg = kw_section then
		subseg = kw_subsection ||
    subseg = kw_subsubsection ||
    subseg = kw_paragraph
  else if seg = kw_subsection then
    subseg = kw_subsubsection ||
    subseg = kw_paragraph
  else if seg = kw_subsubsection then
    subseg = kw_paragraph 
	else
		false


(* Return the list of environments used in the contents 
   Important: assumes comments are removed.  
	 The function looks inside comments as well.
 *)
let find_all_env contents  =
  let extract_env (m: Re2.Match.t) =
    let source = Re2.Match.get ~sub:(`Name "envname") m in
      match source with 
      | None -> let _ = d_printf "tex_syntax.find_env None" in []
      | Some x -> let _ = d_printf "tex_syntax.find_env Some %s" x in [x]
  in
  (* The quad escape's are due to ocaml's string representation that requires escaping \ *)
  let regex_begin = Re2.create_exn "\\\\begin{(?P<envname>[[:alnum:]]*)}" in
  let regex_end = Re2.create_exn "\\\\end{(?P<envname>[[:alnum:]]*)}" in

  let pattern = Re2.pattern regex_begin in
(*
  let _ = printf "tex_syntax.find_env: Pattern for this regex = %s\n" pattern in 
*)
  let all_begin_ = Re2.get_matches_exn regex_begin contents in
  let all_end_ = Re2.get_matches_exn regex_end contents in
  let all_begin: string list = List.concat_map all_begin_ ~f:extract_env in
  let all_end: string list = List.concat_map all_end_ ~f:extract_env in
  let _ = d_printf_strlist "tex_syntax.find_env: all_begin" all_begin in 
  let _ = d_printf_strlist "tex_syntax.find_env: all_end" all_end in 
	let all_begin = List.sort ~compare:Stdlib.String.compare all_begin in
	let all_end = List.sort ~compare:Stdlib.String.compare all_end in
	try
	let ok = List.fold2_exn all_begin all_end ~init:true ~f:(fun r x y -> r && (x = y)) in
		  (assert ok;
			 Some (all_begin, all_end))
	with
    Invalid_argument x -> (printf "Fatal Error: Internal Error in tex_syntax.find_env: %s\n" x; None) 


let is_label_only contents = 
  let contents = String.strip contents in
  str_match_full pattern_label contents 
  
let is_code kind = 
  kind = kw_code

let is_atom kind = 
   let _ = d_printf "is_atom: kind = %s\n" kind in 
     if String.is_prefix kind ~prefix:"run" then
       let pl = String.chop_prefix_exn kind ~prefix:"run" in
       let _ = d_printf "Found code atom: %s\n" pl in
         Some (kw_code, Some pl)
     else   
       match List.Assoc.find atom_kinds ~equal: String.equal kind with 
       | Some _ -> 
					 let _ = d_printf "Found regular atom: %s\n" kind in
					 Some (kind, None)
       | None ->
					 let _ = d_printf "Not an atom: %s\n" kind in
					 None



let is_primary_prompt kind = 
   match List.Assoc.find primary_prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_primary_choice_prompt kind = 
   match List.Assoc.find primary_choice_prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_correct_choice_prompt kind = 
   match List.Assoc.find correct_choice_prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_secondary_choice_prompt kind = 
   match List.Assoc.find secondary_choice_prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_prompt_refsol_fillin kind =
  String.equal kind kw_refsol_fillin

let is_prompt_refsol_fillin_ask kind =
  String.equal kind kw_refsol_fillin_ask

let is_scorable_prompt kind = 
   match List.Assoc.find scorable_prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false


let is_prompt kind = 
   match List.Assoc.find prompt_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let is_cookie kind = 
   match List.Assoc.find cookie_kinds ~equal: String.equal kind with 
   | Some _ -> true
   | None -> false

let get_cookie_cost_ratio kind = 
   match List.Assoc.find cookie_cost_ratio ~equal: String.equal kind with 
   | Some r -> r
   | None -> Constants.zero_cost

(* Meant for figure and table titles such as [h] [ht] [ht!] *)
let title_is_significant title = 
  String.length title > 4 

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
