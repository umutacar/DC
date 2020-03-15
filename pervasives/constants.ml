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
let zero_points_per_question = "0.0" 

exception Fatal_Error of string
exception Input_File_Not_Found of string
