(**********************************************************************
 ** tex/texSyntax.ml
 **********************************************************************)
open Core
open Utils

let newline = "\n"
let space = " "
let colon = ":"
let correct_choice_indicator = "*"
let label_seperator = colon
let label_nestor = colon ^ colon


(* BEGIN: Keywords *)
let com_depend = "\\depend"
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
let kw_task = "task"
let kw_theorem = "theorem"

(* END: Keywords *)

(* BEGIN: lstlisting arguments *)

let firstline = "firstline"
let language = "language"
let none = "none"
let numbers = "numbers"

(* END: lstlisting arguments *)

(* BEGIN: Patterns Regular Expressions *)
let pattern_newline = "[ \n\r\x0c]+"
let regexp_ch_prefix = Str.regexp "ch[:]+"
let regexp_sec_prefix = Str.regexp "sec[:]+"
let regexp_gr_prefix = Str.regexp "grp[:]+"
let regexp_whitespace = Str.regexp "[ \n\r\x0c\t]+"
(* END: Regular Expressions *)


(* BEGIN: label prefixes *)
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
let label_prefix_assumption = "asm"
let label_prefix_code = "cd"
let label_prefix_corollary = "crl"
let label_prefix_costspec = "cst"
let label_prefix_datastr = "dtstr"
let label_prefix_datatype = "adt"
let label_prefix_definition = "def"
let label_prefix_example = "xmpl"
let label_prefix_exercise = "xrcs"
let label_prefix_hint = "hint"
let label_prefix_important = "imp"
let label_prefix_lemma = "lem"
let label_prefix_note = "nt"
let label_prefix_gram = "grm"
let label_prefix_preamble = "prmbl"
let label_prefix_problem = "prb"
let label_prefix_proof = "prf"
let label_prefix_proposition = "prop"
let label_prefix_remark = "rmrk"
let label_prefix_reminder = "rmdr"
let label_prefix_slide = "slide"
let label_prefix_solution = "sol"
let label_prefix_syntax = "syn"
let label_prefix_task = "tsk"
let label_prefix_theorem = "thm"


let label_prefix_of_kind = 
  [
   kw_cluster, label_prefix_cluster;
   kw_flex, label_prefix_flex;
   kw_problem_cluster, label_prefix_problem_cluster
  ]
  @
  (* atoms *)
  [kw_algorithm, label_prefix_algorithm;
   kw_assumption, label_prefix_assumption;
   kw_code, label_prefix_code;
   kw_corollary, label_prefix_corollary;
   kw_costspec, label_prefix_costspec;
   kw_datastr, label_prefix_datastr;
   kw_datatype, label_prefix_datatype;
   kw_definition, label_prefix_definition;
   kw_example, label_prefix_example;
   kw_exercise, label_prefix_exercise;
   kw_hint, label_prefix_hint;
   kw_important, label_prefix_important;
   kw_lemma, label_prefix_lemma;
   kw_note, label_prefix_note;
   kw_gram, label_prefix_gram;
   kw_preamble, label_prefix_preamble;
   kw_problem, label_prefix_problem;
   kw_proof, label_prefix_proof;
   kw_proposition, label_prefix_proposition;
   kw_remark, label_prefix_remark;
   kw_reminder, label_prefix_reminder;
   kw_slide, label_prefix_slide;
   kw_solution, label_prefix_solution;
   kw_syntax, label_prefix_syntax;
   kw_task, label_prefix_task;
   kw_theorem, label_prefix_theorem;
  ]

let mk_label_prefix_from_kind kind = 
   match List.Assoc.find label_prefix_of_kind ~equal: String.equal kind with 
   | Some prefix -> prefix
   | None -> (printf "FATAL ERROR: unknown atom encountered.\n";
              exit ErrorCode.labeling_error_unknown_atom)

(* END: label prefixes *)


(* Utilities *)
let mk_arg arg = 
  "{" ^ arg ^ "}"

let mk_opt_arg x = 
  "[" ^ x ^ "]"

let mk_label label = 
   com_label ^ (mk_arg label)

let mk_label_opt lopt = 
  match lopt with 
  |  None -> ""
  |  Some label -> mk_label label

let mk_depend_opt dopt = 
  let heading = com_depend in
	match dopt with 
  |  None -> ""
  |  Some ls -> heading ^ (String.concat ~sep:", " ls) ^ "}" ^ "\n" 

let mk_heading name pvalopt t = 
  let b = "\\" ^ name in
  let p = 
		match pvalopt with 
    | None -> ""
    | Some pts -> 
				if pts = 0.0 then ""
				else mk_opt_arg (Float.to_string pts)
  in 
    b ^ p ^ "{" ^ t ^ "}" ^ "\n"

let mk_begin name pvalopt topt = 
  let b = "\\begin{" ^ name ^ "}" in
  let p = 
		match pvalopt with 
		| None -> ""
    | Some pts -> 
				if pts = 0.0 then ""
				else mk_opt_arg (Float.to_string pts)
  in 
  let t = 
		match topt with 
    | None -> ""
    | Some t -> mk_opt_arg t
  in
  b ^ p ^ t ^ "\n"


let mk_pointval pts = 
  mk_opt_arg (Float.to_string pts)

let mk_pointval_opt pts_opt = 
  match pts_opt with 
  | None -> ""
  | Some pts -> mk_opt_arg (Float.to_string pts)

let mk_hint_opt hint_opt = 
  let heading = com_hint in
  match hint_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  


let mk_exp_opt exp_opt = 
  let heading = com_explain in
  match exp_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  

let refsol_opt refsol_opt = 
  let heading = com_solution in
  match refsol_opt with 
  |  None -> ""
  |  Some x -> heading ^ "\n" ^ x  

let rubric_opt rubric_opt = 
  let heading = com_rubric in
  match rubric_opt with 
  |  None -> ""
  |  Some x -> 
      (d_printf "rubricOptToTex: rubric = %s" x; 
       heading ^ "\n" ^ x)

let is_group kw = 
  kw = kw_cluster ||
  kw = kw_flex ||
  kw = kw_problem_cluster 


(* is subseg nested in segment seg ? *)
let segment_is_nested subseg seg = 
  if seg = kw_section then
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
let find_env contents  =
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
  let _ = printf_strlist "tex_syntax.find_env: all_begin" all_begin in 
  let _ = printf_strlist "tex_syntax.find_env: all_end" all_end in 
	try
	let ok = List.fold2_exn all_begin all_end ~init:true ~f:(fun r x y -> r && (x = y)) in
		  (assert ok;
			 Some (all_begin, all_end))
	with
    Invalid_argument x -> (printf "Fatal Error: Internal Error %s " x; None) 


(**********************************************************************
 ** Tokenization
 **********************************************************************)

let mk_plural s = s ^ "s"

let stopWords = 
  (* English stopwords, from python NLTK package *)
  ["i";
   "me";
   "my";
   "myself";
   "we";
   "our";
   "ours";
   "ourselves";
   "you";
   "your";
   "yours";
   "yourself";
   "yourselves";
   "he";
   "him";
   "his";
   "himself";
   "she";
   "her";
   "hers";
   "herself";
   "it";
   "its";
   "itself";
   "they";
   "them";
   "their";
   "theirs";
   "themselves";
   "what";
   "which";
   "who";
   "whom";
   "this";
   "that";
   "these";
   "those";
   "am";
   "is";
   "are";
   "was";
   "were";
   "be";
   "been";
   "being";
   "have";
   "has";
   "had";
   "having";
   "do";
   "does";
   "did";
   "doing";
   "a";
   "an";
   "the";
   "and";
   "but";
   "if";
   "or";
   "because";
   "as";
   "until";
   "while";
   "of";
   "at";
   "by";
   "for";
   "with";
   "about";
   "against";
   "between";
   "into";
   "through";
   "during";
   "before";
   "after";
   "above";
   "below";
   "to";
   "from";
   "up";
   "down";
   "in";
   "out";
   "on";
   "off";
   "over";
   "under";
   "again";
   "further";
   "then";
   "once";
   "here";
   "there";
   "when";
   "where";
   "why";
   "how";
   "all";
   "any";
   "both";
   "each";
   "few";
   "more";
   "most";
   "other";
   "some";
   "such";
   "no";
   "nor";
   "not";
   "only";
   "own";
   "same";
   "so";
   "than";
   "too";
   "very";
   "s";
   "t";
   "can";
   "will";
   "just";
   "don";
   "should";
   "now"
  ]
  @
  (* Diderot stopwords *)
  [kw_chapter; mk_plural kw_chapter;
   kw_section; mk_plural kw_section;
   kw_subsection; mk_plural kw_subsection;
   kw_subsubsection; mk_plural kw_subsubsection;
   kw_paragraph; mk_plural kw_paragraph;
   kw_flex; mk_plural kw_flex;
   kw_problem_cluster; mk_plural kw_problem_cluster;
   kw_algorithm; mk_plural kw_algorithm;
   kw_assumption; mk_plural kw_assumption;
   kw_code; mk_plural kw_code;
   kw_corollary; mk_plural kw_corollary;
   kw_costspec; mk_plural kw_costspec;
   kw_datastr; mk_plural kw_datastr;
   kw_datatype; mk_plural kw_datatype;
   kw_definition; mk_plural kw_definition;
   kw_example; mk_plural kw_example;
   kw_exercise; mk_plural kw_exercise;
   kw_hint; mk_plural kw_hint;
   kw_important; 
   kw_lemma; mk_plural kw_lemma;
   kw_note; mk_plural kw_note;
   kw_gram; mk_plural kw_gram;
   kw_preamble; mk_plural kw_preamble;
   kw_problem; mk_plural kw_problem;
   kw_proof; mk_plural kw_proof;
   kw_proposition; mk_plural kw_proposition;
   kw_remark; mk_plural kw_remark;
   kw_reminder; mk_plural kw_reminder;
   kw_slide; mk_plural kw_slide;
   kw_solution; mk_plural kw_solution;
   kw_syntax; 
   kw_task; mk_plural kw_task;
   kw_theorem; mk_plural kw_theorem
  ]
  @
  (* quantifers, those not included in the stopwords above *)
  [
   "couple";
   "enough";
   "lots";
   "little";
   "many";
   "much";
   "plenty";
   "several"
  ] 
  @
  (* adverbs *)
  [ 
   "actually";
   "accordingly";
   "across";
   "adjacent";
   "afterward";
   "ahead";
   "along";
   "also";
   "another";
   "background";
   "begin";
   "behind";
   "besides";
   "beyond";
   "briefly";
   "consequence";
   "consequently";
   "contrast";
   "contrary";
   "conversely";
   "currently";
   "directly";
   "fact";
   "finally";
   "first";
   "following";
   "furthermore";
   "gradually";
   "hence";
   "however";
   "last";
   "lastly";
   "later";
   "left";
   "like";
   "manner";
   "meantime";
   "meanwhile";
   "moreover";
   "nearby";
   "next";
   "nevertheless";
   "nonetheless";
   "presently";
   "opposite";
   "result";
   "right";
   "second";
   "short";
   "side";
   "similarly";
   "since";
   "soon";
   "specific";
   "specifically";
   "spite";
   "still";
   "summary";
   "subsequently";
   "thereafter";
   "therefore";
   "thus";
   "top";
   "ultimately";
   "yet"
  ]

let stopWordsTable = 
  let stopWords = List.map stopWords ~f:(fun x -> (x, ())) in
    match Hashtbl.of_alist (module String) stopWords with
    | `Ok t -> t
    | `Duplicate_key x ->
       (printf "Fatal Error: Duplicate entry in the stop words table %s\n"  x;
        exit 1)

let labelGood x =      
  (String.length x > 1) 
  & 
  (try let _ = Hashtbl.find_exn stopWordsTable x  in
          false
    with Caml.Not_found -> true
  )


