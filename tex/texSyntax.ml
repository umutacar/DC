(**********************************************************************
 ** tex/texSyntax.ml
 **********************************************************************)
open Core

let newline = "\n"
let space = " "
let colon = ":"
let correct_choice_indicator = "*"
let label_seperator = colon
let label_nestor = colon ^ colon
let label_prefix_section = "sec"


(* BEGIN: Keywords *)
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
let kw_algorithm = "algorithm"
let kw_assumption = "assumption"
let kw_code = "code"
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

(* BEGIN: Regular Expressions *)
let regexp_ch_prefix = Str.regexp "ch[:]+"
let regexp_sec_prefix = Str.regexp "sec[:]+"
let regexp_gr_prefix = Str.regexp "gr[:]+"
let regexp_whitespace = Str.regexp "[ \n\r\x0c\t]+"
(* END: Regular Expressions *)


(* BEGIN: label prefixes *)
let label_prefix_ch = "ch"
let label_prefix_sec = "sec"
let label_prefix_group = "grp"
let label_prefix_def = "def"
let label_prefix_algorithm = "alg"
let label_prefix_assumption = "asm"
let label_prefix_code = "code"
let label_prefix_corollary = "cor"
let label_prefix_costspec = "cost"
let label_prefix_datastr = "ds"
let label_prefix_datatype = "adt"
let label_prefix_definition = "def"
let label_prefix_example = "ex"
let label_prefix_exercise = "exr"
let label_prefix_hint = "hint"
let label_prefix_important = "imp"
let label_prefix_lemma = "lem"
let label_prefix_note = "note"
let label_prefix_gram = "gram"
let label_prefix_preamble = "gram"
let label_prefix_problem = "prob"
let label_prefix_proof = "proof"
let label_prefix_proposition = "prop"
let label_prefix_remark = "remark"
let label_prefix_reminder = "reminder"
let label_prefix_slide = "slide"
let label_prefix_solution = "sol"
let label_prefix_syntax = "syn"
let label_prefix_task = "task"
let label_prefix_theorem = "thm"


let label_prefix_atoms = 
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

(*   kw_xxx, label_prefix_xxx; *)

let mk_label_prefix_atom kind = 
   match List.Assoc.find label_prefix_atoms ~equal: String.equal kind with 
   | Some prefix -> prefix
   | None -> (printf "FATAL ERROR: unknown atom encountered.\n";
              exit ErrorCode.labeling_error_unknown_atom)

(* END: label prefixes *)


(* Utilities *)
let mkArg arg = 
  "{" ^ arg ^ "}"

let mkLabel label = 
   com_label ^ (mkArg label)


