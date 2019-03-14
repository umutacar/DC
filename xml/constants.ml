(**********************************************************************
 ** Diderot Constants 
 **********************************************************************)

(* Syntactic constants *)
let cdata_begin = "<![CDATA["
let cdata_end = "]]>"
let equality = "="
let newline = "\n"
let quote = "'"
let space = " "



(* Error conditions that can occur as tokens *)
let not_provided = "...NOT.PROVIDED."
let no_answer = not_provided ^ "ANSWER..." 
let no_answers = not_provided ^ "ANSWERS..." 
let no_checkpoint = not_provided ^ "CHECKPOINT..." 
let no_explain = not_provided ^ "EXPLANATION..." 
let no_hint = not_provided ^ "HINT..." 
let no_intro = not_provided ^ "INTRO..." 
let no_info = not_provided ^ "INFO..."
let no_label = not_provided ^ "LABEL..." 
let no_no = "0"  (* has to be a number still *) 
let no_parents = not_provided ^ "PARENTS..." 
let no_points = "0"  (* Still has to be a number *)
let no_point_value = "0.0"  (* Still has to be a number *)
let no_prompt = not_provided ^ "PROMPT..." 
let no_rank = "0"  (* has to be a number still *) 
let no_solution = not_provided ^ "UNKOWN.SOLUTION..." 
let no_title = not_provided ^ "TITLE..." 
let no_topics = not_provided ^ "TOPICS..."
let no_unique = "0"  (* has to be a number still *) 


(* Label prefixes.  These will be useful in strictifacion and decoration. *)
(*
LABEL_PREFIX_ANSWER = "answer"
LABEL_PREFIX_ASSIGNMENT = "assignment"
LABEL_PREFIX_ASSTPROBLEM = "asstproblem"
LABEL_PREFIX_ATOM = "at"
LABEL_PREFIX_BOOK = "book"
LABEL_PREFIX_CHAPTER = "ch"
LABEL_PREFIX_CHOICE = "choice"
LABEL_PREFIX_COURSE = "course"
LABEL_PREFIX_GRAM = "gram"
LABEL_PREFIX_GROUP = "gr"
LABEL_PREFIX_CHECKPOINT = "checkpoint"
LABEL_PREFIX_PROBLEM = "problem"
LABEL_PREFIX_PROBLEM_GROUP = "problem_group"
LABEL_PREFIX_PROBLEM_SET = "problem_set"
LABEL_PREFIX_PROBLEM_FR = "problem_fr"
LABEL_PREFIX_PROBLEM_MA = "problem_ma"
LABEL_PREFIX_PROBLEM_MC = "problem_mc"
LABEL_PREFIX_QUESTION_FR = "question_fr"
LABEL_PREFIX_QUESTION_MA = "question_ma"
LABEL_PREFIX_QUESTION_MC = "question_mc"
LABEL_PREFIX_SECTION = "sec"
LABEL_PREFIX_SELECT = "select"
LABEL_PREFIX_SEMESTER = "semester"
LABEL_PREFIX_SUBSECTION = "subsec"
LABEL_PREFIX_SUBSUBSECTION = "subsubsec"

*)