

let tmp_dir_name = "/tmp"
let diderot_atomic = "diderot_"
let ext_core = "_core"
let ext_markdown = "md"
let ext_tex = "tex"
let ext_xml = "xml"
let space = " " 

let default_points_per_question = "1.0" 
let default_strategy = "all" 
let zero_points = "0.0" 
let zero_weight = "0.0" 
let cookie_weight_explain = "0.1"
let cookie_weight_hint = "0.3"

type t_mode_prompt_rewriter  = 
	| Prompt_Mode_Question  | Prompt_Mode_Solution

exception Fatal_Error of string
exception Syntax_Error of string
exception Input_File_Not_Found of string
