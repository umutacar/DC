(**********************************************************************
 ** Diderot Constants 
 **********************************************************************)

let tmp_dir_name = "/tmp"
let diderot_atomic = "diderot_"
let ext_core = "_core"
let ext_markdown = "md"
let ext_tex = "tex"
let ext_xml = "xml"
let space = " " 

let default_points_per_question = "1.0" 
let zero_points = "0.0" 
let zero_cost = "0.0" 
let cookie_cost_explain = "0.2"
let cookie_cost_hint = "0.4"

exception Fatal_Error of string
exception Input_File_Not_Found of string
