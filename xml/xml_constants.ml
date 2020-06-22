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
let no_caption = not_provided ^ "CAPTION..." 
let no_checkpoint = not_provided ^ "CHECKPOINT..." 
let no_cover = not_provided ^ "COVER..." 
let no_depend = not_provided ^ "DEPEND..." 
let no_explain = not_provided ^ "EXPLANATION..." 
let no_hint = not_provided ^ "HINT..." 
let no_intro = not_provided ^ "INTRO..." 
let no_info = not_provided ^ "INFO..."
let no_label = not_provided ^ "LABEL..." 
let no_no = "0"  (* has to be a number still *) 
let no_parents = not_provided ^ "PARENTS..." 
let no_pl = not_provided ^ "PL..." 
let no_pl_version = not_provided ^ "PL_VERSION..." 
let no_points = "0"  (* Still has to be a number *)
let no_point_value = "0.0"  (* Still has to be a number *)
let no_prompt = not_provided ^ "PROMPT..." 
let no_rank = "0"  (* has to be a number still *) 
let no_solution = not_provided ^ "UNKOWN.SOLUTION..." 
let no_sound = not_provided ^ "SOUND..." 
let no_strategy = Constants.default_strategy
let no_title = not_provided ^ "TITLE..." 
let no_topics = not_provided ^ "TOPICS..."
let no_unique = "0"  (* has to be a number still *) 


