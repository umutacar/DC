(** BEGIN: HEADER **)
{
open Printf
open AtomizerParser
open Utils

(* Some Utilities *)
let start = Lexing.lexeme_start
let char_to_str x = String.make 1 x
let pvalopt_to_str x = 
  match x with 
  | None -> "None"
  | Some x -> "Some" ^ x

(**********************************************************************
 ** BEGIN: verbatim machinery 
 **********************************************************************)

let verbatim_pos = ref 0  

let enter_verbatim lexbuf =
  verbatim_pos := start lexbuf

let exit_verbatim lexbuf =
  ()

(**********************************************************************
 ** END: verbatim machinery 
 **********************************************************************)

(**********************************************************************
 ** BEGIN: latex env machinery 
 **********************************************************************)

let latex_env_pos = ref 0  
let latex_env_depth = ref 0  

let do_begin_latex_env () =
  latex_env_depth := !latex_env_depth + 1

let do_end_latex_env () =
  let () = latex_env_depth := !latex_env_depth - 1 in
    (!latex_env_depth = 0)
(**********************************************************************
 ** END: latex env machinery 
 **********************************************************************)

(**********************************************************************
 ** BEGIN: curly bracked depth machinery
 **********************************************************************)
let curly_depth = ref 0  

let inc_curly_depth () =
  curly_depth := !curly_depth + 1

let dec_curly_depth () =
  curly_depth := !curly_depth - 1

let curly_depth () =
  !curly_depth
(**********************************************************************
 ** END: curly bracked depth machinery
 **********************************************************************)

}
(** END: HEADER **)

(** BEGIN: PATTERNS *)	
let p_comma = ','
(* horizontal space *)
let p_hspace = ' ' | '\t'
(* non-space character *)
let p_nschar = [^ ' ' 't' '\n' '\r']
(* newline *)
let p_newline = '\n' | ('\r' '\n')

let p_tab = '\t'	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_percent = '%'
let p_comment_line = p_percent [^ '\n']* '\n'
let p_comment = p_percent [^ '\n']*
let p_skip = p_ws
let p_emptyline = [' ' '\t' '\r']* '\n'
let p_emptyline = [' ' '\t' '\r']* '\n'
let p_nonemptyline = [' ' '\t']* [^ ' ' '\t' 'r' '\n']+ [^ '\r' '\n']*  ['r']? '\n' 

let p_digit = ['0'-'9']
let p_integer = ['0'-'9']+
let p_frac = '.' p_digit*
let p_exp = ['e' 'E'] ['-' '+']? p_digit+
let p_float = p_digit* p_frac? p_exp?

let p_alpha = ['a'-'z' 'A'-'Z']
let p_separator = [':' '.' '-' '_' '/']

(* BEGIN: key value pairs *)
(* key is alphas *)
let p_keyalpha = (p_alpha)+
(* value is everything but white space, comma, or equal *)
let p_valueofkey = [^ ',' '=']+
let p_key_value_pair = (p_keyalpha as key) p_ws '=' p_ws (p_valueofkey as value)
let p_key_value_list = (p_key_value_pair) (p_ws ',' p_ws p_key_value_pair)*
(* END: key value pairs *)

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = p_ws '{' p_ws
let p_c_curly = p_ws '}' p_ws
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_ws											
let p_special_percent = p_backslash p_percent

let p_com_depend = '\\' "depend" p_ws p_o_curly p_ws												 
let p_com_label = '\\' "label" p_ws												 
let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)												 
let p_com_begin = '\\' "begin" p_ws												 
let p_com_end = '\\' "end" p_ws												 
let p_com_choice = '\\' "choice"
let p_com_correct_choice = '\\' "choice*"

let p_com_explain = '\\' "explain"
let p_com_hint = '\\' "help"
let p_com_rubric = '\\' "rubric"
let p_com_refsol = '\\' "sol"

let p_part = '\\' "part"
let p_part_arg = p_part p_ws  (p_o_sq as o_sq) (p_integer) (p_c_sq as c_sq)

(* begin: verbatim 
 * we will treat verbatim as a "box"
 *)
let p_verbatim = "verbatim"
let p_begin_verbatim = p_com_begin p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
let p_end_verbatim = p_com_end p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
(* end: verbatim *)

(* begin: parts
 * we will ignore these.
 *)
let p_parts = "parts"
let p_begin_parts = p_com_begin p_ws p_o_curly p_ws p_parts p_ws p_c_curly
let p_end_parts = p_com_end p_ws p_o_curly p_ws p_parts p_ws p_c_curly
(* end: parts *)

let p_chapter = '\\' "chapter" p_ws
let p_chapter_with_points = '\\' "chapter" p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)
let p_section = '\\' "section" p_ws
let p_section_with_points = '\\' "section" p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_subsection = '\\' "subsection" p_ws 
let p_subsection_with_points = '\\' "subsection" p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_subsubsection = '\\' "subsubsection" p_ws 
let p_subsubsection_with_points = '\\' "subsubsection" p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_paragraph = '\\' "paragraph" p_ws												
let p_paragraph_with_points = '\\' "paragraph" p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_cluster = "cluster"
let p_flex = "flex"
let p_problem_cluster = "mproblem"

let p_xxx = "xxx"
let p_b_xxx = '\\' "begin" p_o_curly p_xxx p_ws p_c_curly
let p_e_xxx = '\\' "end" p_o_curly p_xxx p_ws p_c_curly

let p_diderot_atom = "diderot" ['a'-'z''A'-'Z']*	
let p_algorithm = "algorithm"
let p_assumption = "assumption"
let p_code = "code"
let p_corollary = "corollary"
let p_costspec = "costspec"
let p_datastr = "datastr"
let p_datatype = "datatype"
let p_definition = "definition"
let p_example = "example"
let p_exercise = "exercise"
let p_hint = "hint"
let p_important = "important"
let p_lemma = "lemma"
let p_note = "note"
let p_gram = "gram"
let p_preamble = "preamble"
let p_problem = "problem"
let p_proof = "proof"
let p_proposition = "proposition"
let p_remark = "remark"
let p_reminder = "reminder"
let p_slide = "slide"
let p_solution = "solution"
let p_syntax = "syntax"
let p_task = "task"
let p_theorem = "theorem"

(* Treat teach atoms as tail text, thus they will not be loaded as atoms *)
let p_teachask = "xx__teachask__xx"
let p_teachnote = "xx__teachnote__xx"


(* Ilists *)
let p_chooseone = "xchoice"
let p_chooseany = "anychoice"

let p_ilist_separator = p_com_choice | p_com_correct_choice
let p_ilist_separator_arg = (p_com_choice as kind) p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 
                            | (p_com_correct_choice as kind) p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 

(* A latex environment consists of alphabethical chars plus an optional star *)
let p_latex_env = (p_alpha)+('*')?

let p_atom = ((p_diderot_atom as kind) p_ws as kindws) |
             ((p_algorithm as kind) p_ws as kindws) |
             ((p_assumption as kind) p_ws as kindws) |
             ((p_code as kind) p_ws as kindws) |
             ((p_corollary as kind) p_ws as kindws) |
             ((p_costspec as kind) p_ws as kindws) |
             ((p_datastr as kind) p_ws as kindws) |
             ((p_datatype as kind) p_ws as kindws) |
             ((p_definition as kind) p_ws as kindws) |
             ((p_example as kind) p_ws as kindws) |
             ((p_exercise as kind) p_ws as kindws) |
             ((p_gram as kind) p_ws as kindws) |
             ((p_hint as kind) p_ws as kindws) |
             ((p_important as kind) p_ws as kindws) |
             ((p_lemma as kind) p_ws as kindws) |
             ((p_note as kind) p_ws as kindws) |
             ((p_preamble as kind) p_ws as kindws) |
             ((p_problem as kind) p_ws as kindws) |
             ((p_proof as kind) p_ws as kindws) |
             ((p_proposition as kind) p_ws as kindws) |
             ((p_remark as kind) p_ws as kindws) |
             ((p_reminder as kind) p_ws as kindws) |
             ((p_slide as kind) p_ws as kindws) |
             ((p_solution as kind) p_ws as kindws) |
             ((p_syntax as kind) p_ws as kindws) |
             ((p_task as kind) p_ws as kindws) |
             ((p_teachask as kind) p_ws as kindws) |
             ((p_teachnote as kind) p_ws as kindws) |
             ((p_theorem as kind) p_ws as kindws) 


let p_begin_atom = (p_com_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) 
let p_begin_atom_with_points = (p_com_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_atom = (p_com_end p_ws as e) (p_o_curly as o) p_atom (p_c_curly as c) 

let p_begin_code_atom = (p_com_begin p_ws as b) (p_o_curly as o) ((p_code as kind) p_ws as kindws) (p_c_curly as c) 

let p_end_code_atom = (p_com_end p_ws as e) (p_o_curly as o) ((p_code as kind) p_ws as kindws) (p_c_curly as c) 

let p_ilist_kinds = (p_chooseone | p_chooseany)
let p_ilist = ((p_ilist_kinds as kind) p_ws as kindws) 

let p_begin_ilist = (p_com_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c) 

(* point values now go with atoms.
let p_begin_ilist_arg = (p_com_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c)  (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
*)
 
let p_end_ilist = (p_com_end p_ws as e) (p_o_curly as o) p_ilist (p_c_curly as c) 

let p_begin_latex_env = (p_com_begin p_ws) (p_o_curly) (p_latex_env) (p_c_curly) 
let p_end_latex_env = (p_com_end p_ws) (p_o_curly) (p_latex_env) (p_c_curly) 


let p_group = ((p_cluster as kind) p_ws as kindws) |
              ((p_flex as kind) p_ws as kindws) |
              ((p_problem_cluster as kind) p_ws as kindws) 

let p_begin_group = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) 
let p_begin_group_with_points = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_group = (p_com_end p_ws as e) (p_o_curly as o) p_group (p_c_curly as c) 

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 


(** END PATTERNS *)			


rule token = parse
| p_comment as x
  	{d_printf "!lexer matched comment line %s." x; COMMENT(x)}		

| p_chapter as x
    {
     let _ = d_printf "!lexer matched chapter: %s." x in
     let arg = take_arg lexbuf in
     let h = x ^ arg in
     let _ = d_printf "!lexer matched chapter: %s." h in
       KW_CHAPTER(h, None)
    }		

| p_section as x
    {
     let _ = d_printf "!lexer matched section: %s." x in
     let arg = take_arg lexbuf in
     let h = x ^ arg in
     let _ = d_printf "!lexer matched section: %s." h in
       KW_SECTION(h, None)
    }		


| p_subsection as x
    {
     let _ = d_printf "!lexer matched subsection: %s." x in
     let arg = take_arg lexbuf in
     let h = x ^ arg in
     let _ = d_printf "!lexer matched subsection: %s." h in
       KW_SUBSECTION(h, None)
    }		


| p_subsubsection as x
    {
     let _ = d_printf "!lexer matched subsubsection: %s." x in
     let arg = take_arg lexbuf in
     let h = x ^ arg in
     let _ = d_printf "!lexer matched subsubsection: %s." h in
       KW_SUBSUBSECTION(h, None)
    }		

| p_paragraph as x
    {
     let _ = d_printf "!lexer matched paragraph: %s." x in
     let arg = take_arg lexbuf in
     let h = x ^ arg in
     let _ = d_printf "!lexer matched paragraph: %s." h in
       KW_PARAGRAPH(h, None)
    }		

| p_begin_latex_env as x
      { 
          let _ = d_printf "!lexer: begin latex env: %s\n" x in
          let _ = do_begin_latex_env () in
          let y = latex_env lexbuf in
          let _ = d_printf "!lexer: latex env matched = %s" (x ^ y) in
            ENV(x ^ y)
          
      }   

| p_newline as x
		{d_printf "!lexer found: newline: %s." x;
     NEWLINE(x)
    }

| p_hspace as x
		{d_printf "!lexer found: horizontal space: %s." (char_to_str x);
     HSPACE(char_to_str x)
    }

| p_nschar as x
		{d_printf "!lexer found: char: %s." (char_to_str x);
     NSCHAR(char_to_str x)
    }

| eof
		{EOF}
| _
    {token lexbuf}		
		
and latex_env =
  parse
  | p_begin_verbatim as x
      { 
          let _ = d_printf "!lexer: entering verbatim\n" in
          let _ = enter_verbatim lexbuf in
          let y = verbatim lexbuf in
          let _ = d_printf "!lexer: verbatim matched = %s" (x ^ y) in
          let z = latex_env lexbuf in
            x ^ y ^ z          
      }   
  | p_begin_latex_env as x
        {
            let _ = d_printf "!lexer: begin latex env: %s\n" x in
            let _ = do_begin_latex_env () in
            let y = latex_env lexbuf in
                x ^ y              
        }

  | p_end_latex_env as x
        { 
            let _ = d_printf "!lexer: end latex env: %s\n" x in
            let do_exit = do_end_latex_env () in
                if do_exit then
                    let _ = d_printf "!lexer: exiting latex env\n" in
                        x
                else
                    let y = latex_env lexbuf in
                      x ^ y  
        }      
  | p_comment_line as x   (* skip over comments *)
      	{ 
            let y = latex_env lexbuf in 
                x ^ y
        } 
  | _  as x
        { let y = latex_env lexbuf in
            (char_to_str x) ^ y
        }
and verbatim =
  parse
  | p_end_verbatim as x
        { 
            let _ = d_printf "!lexer: exiting verbatim\n" in
            let _ = exit_verbatim lexbuf in 
                x
        }
  | _  as x
        { let y = verbatim lexbuf in
            (char_to_str x) ^ y
        }
and take_arg = 
  parse 
  | '{' as x
    {
     let _ = inc_curly_depth () in
     let arg = take_arg lexbuf in 
       (char_to_str x) ^ arg
    }
  | '}' as x
    {
     let _ = dec_curly_depth () in
       if curly_depth () = 0 then
         "}"
       else
         let arg = take_arg lexbuf in 
           (char_to_str x) ^ arg
    }

  | _ as x
    {
     let arg = take_arg lexbuf in 
       (char_to_str x) ^ arg
    }



(** BEGIN TRAILER **)
{
}
(** END TRAILER **)


