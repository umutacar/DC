(** BEGIN: HEADER **)
{
open Printf
open Parser
open Utils

(* Some Utilities *)
let start = Lexing.lexeme_start
let char_to_str x = String.make 1 x


(**********************************************************************
 ** BEGIN: lits
 **********************************************************************)


let do_begin_ilist () =
  ()

let do_end_ilist () =
  ()

(**********************************************************************
 ** END: latex env machinery 
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


}
(** END: HEADER **)

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_percent = '%'
let p_comment_line = p_percent [^ '\n']* '\n'
let p_skip = p_ws
let p_digit = ['0'-'9']
let p_alpha = ['a'-'z' 'A'-'Z']
let p_separator = [':' '.' '-' '_' '/']
let p_integer = ['0'-'9']+

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_ws
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_ws											
let p_special_percent = p_backslash p_percent

let p_label = '\\' "label" p_ws												 
let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)												 
let p_begin = '\\' "begin" p_ws												 
let p_end = '\\' "end" p_ws												 
let p_choice = '\\' "choice"
let p_correctchoice = '\\' "correctchoice"
let p_solution = '\\' "solution"

let p_part = '\\' "part"
let p_part_arg = p_part p_ws  (p_o_sq as o_sq) (p_integer) (p_c_sq as c_sq)

(* begin: verbatim 
 * we will treat verbatim as a "box"
 *)
let p_verbatim = "verbatim"
let p_begin_verbatim = p_begin p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
let p_end_verbatim = p_end p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
(* end: verbatim *)

(* begin: parts
 * we will ignore these.
 *)
let p_parts = "parts"
let p_begin_parts = p_begin p_ws p_o_curly p_ws p_parts p_ws p_c_curly
let p_end_parts = p_end p_ws p_o_curly p_ws p_parts p_ws p_c_curly
(* end: parts *)

let p_chapter = '\\' "chapter" p_ws
let p_section = '\\' "section" p_ws
let p_titled_question = '\\' "titledsection" p_ws
let p_subsection = '\\' "subsection" p_ws
let p_subsubsection = '\\' "subsubsection" p_ws
let p_paragraph = '\\' "paragraph" p_ws												
let p_subparagraph = '\\' "subparagraph" p_ws												

let p_b_cluster = '\\' "begin{cluster}" p_ws	
let p_e_cluster = '\\' "end{cluster}" p_ws

let p_b_group = '\\' "begin{flex}" p_ws	
let p_e_group = '\\' "end{flex}" p_ws

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
let p_teachask = "teachask"
let p_teachnote = "teachnote"
let p_theorem = "theorem"

(* Ilists *)
let p_pickone = "pickone"
let p_pickany = "pickany"

let p_ilist_separator = p_choice | p_correctchoice

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

let p_begin_atom = (p_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) 
let p_end_atom = (p_end p_ws as e) (p_o_curly as o) p_atom (p_c_curly as c) 

(* This is special, we use it to detect the end of a solution *)
let p_end_problum = (p_end p_ws as e) (p_o_curly as o) p_problem (p_c_curly as c) 

let p_ilist_kinds = (p_pickone | p_pickany)
let p_ilist = ((p_ilist_kinds as kind) p_ws as kindws) 

let p_begin_ilist = (p_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c) 

let p_begin_ilist_arg = (p_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c)  (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
 
let p_end_ilist = (p_end p_ws as e) (p_o_curly as o) p_ilist (p_c_curly as c) 

let p_begin_latex_env = (p_begin p_ws) (p_o_curly) (p_latex_env) (p_c_curly) 
let p_end_latex_env = (p_end p_ws) (p_o_curly) (p_latex_env) (p_c_curly) 

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 


(** END PATTERNS *)			


rule token = parse
| p_backslash as x
		{d_printf "!lexer matched: \\."; BACKSLASH(char_to_str x)}				
| p_o_curly as x
		{d_printf "!lexer matched: %s.\n" x; O_CURLY(x)}				
| p_c_curly as x
		{d_printf "!lexer matched: %s.\n" x; C_CURLY(x)}				
| p_o_sq as x
		{d_printf "!lexer matched: %s.\n" x; O_SQ_BRACKET(x)}				
| p_c_sq as x
		{d_printf "!lexer matched: %s.\n" x; C_SQ_BRACKET(x)}				
| p_special_percent as x
		{d_printf "!lexer matched: %s.\n" x; PERCENT(x)}				

| p_comment_line as x
  	{d_printf "!lexer matched comment line %s." x; COMMENT_LINE(x)}		

| p_label_and_name as x
  	{d_printf "!lexer matched %s." x; KW_LABEL_AND_NAME(label_pre ^ label_name ^ label_post, label_name)}		
| p_backslash as x
(*
| p_label as x
  	{d_printf "!lexer matched %s." x; KW_LABEL(x)}		
*)
				
| p_chapter as x
  	{d_printf "!lexer matched %s." x; KW_CHAPTER(x)}		
| p_section as x
  	{d_printf "!lexer matched: %s." x; KW_SECTION(x)}		
| p_titled_question as x
  	{d_printf "!lexer matched: %s." x; KW_TITLED_QUESTION(x)}		
| p_subsection as x
  	{d_printf "!lexer matched: %s." x; KW_SUBSECTION(x)}
| p_subsubsection as x
  	{d_printf "!lexer matched: %s." x; KW_SUBSUBSECTION(x)}
| p_paragraph as x
  	{d_printf "!lexer matched: %s." x; KW_PARAGRAPH(x)}				
| p_subparagraph as x
  	{d_printf "!lexer matched: %s." x; KW_SUBPARAGRAPH(x)}		
| p_b_cluster as x
  	{d_printf "!lexer matched: %s." x; KW_BEGIN_CLUSTER(x)}		
| p_e_cluster as x
  	{d_printf "!lexer matched: %s." x; KW_END_CLUSTER(x)}
| p_b_group as x
  	{d_printf "!lexer matched: %s." x; KW_BEGIN_GROUP(x)}		
| p_e_group as x
  	{d_printf "!lexer matched: %s." x; KW_END_GROUP(x)}
| p_begin_atom
  	{let all = b ^ o ^ kindws ^ c in
       d_printf "lexer matched begin atom: %s" kind;
       KW_BEGIN_ATOM(kind, all)
    }		
| p_end_atom
  	{let all = e ^ o ^ kindws ^ c in
       d_printf "lexer matched end atom: %s" kind;
       KW_END_ATOM(kind, all)
    }		
| p_begin_ilist 
      {let kw_b = b ^ o ^ kindws ^ c in
       let _ = d_printf "!lexer: begin ilist: %s\n" kw_b in
       let _ = do_begin_ilist () in
       let (_, l, kw_e) = ilist lexbuf in
       let l_joined = List.map (fun (x, y) -> x ^ y) l in
       let sl = kw_b ^ String.concat "," l_joined ^ kw_e in
       let _ = d_printf "!lexer: ilist matched = %s" sl in
            ILIST(kind, kw_b, None, l, kw_e)          
      }   

| p_begin_ilist_arg 
      {let kw_b = b ^ o ^ kindws ^ c  in
       let kw_b_arg = kw_b ^ o_sq ^ point_val ^ c_sq in
       let _ = d_printf "!lexer: begin ilist: %s\n" kw_b_arg in
       let _ = do_begin_ilist () in
       let (_, l, kw_e) = ilist lexbuf in
       let l_joined = List.map (fun (x, y) -> x ^ y) l in
       let sl = kw_b_arg ^ String.concat "," l_joined ^ kw_e in
       let _ = d_printf "!lexer: ilist matched = %s" sl in
            ILIST(kind, kw_b, Some (o_sq,  point_val, c_sq), l, kw_e)          
      }   
| p_solution as h 
      {
       let _ = d_printf "!lexer: begin solution\n" in
       let body = solution lexbuf in
       let _ = d_printf "!lexer: solution matched = %s" body in
            SOLUTION(h, body)
      }   

| p_part as x           (* treat as comment *)
    {COMMENT_LINE(x)}
(*    {token lexbuf}		*)
| p_part_arg as x            (* treat as comment *)
    {COMMENT_LINE(x)}
(*    {token lexbuf}		*)
| p_begin_parts as x    (* treat as comment *)
    {COMMENT_LINE(x)}
(*    {token lexbuf}		*)
| p_end_parts as x      (* treat as comment *)
    {COMMENT_LINE(x)}
(*    {token lexbuf}		*)


| p_begin_latex_env as x
      { 
          let _ = d_printf "!lexer: begin latex env: %s\n" x in
          let _ = do_begin_latex_env () in
          let y = latex_env lexbuf in
          let _ = d_printf "!lexer: latex env matched = %s" (x ^ y) in
            ENV(x ^ y)
          
      }   

| p_begin_verbatim as x
      { 
          let _ = d_printf "!lexer: entering verbatim\n" in
          let _ = enter_verbatim lexbuf in
          let y = verbatim lexbuf in
          let _ = d_printf "!lexer: verbatim matched = %s" (x ^ y) in
            ENV(x ^ y)
          
      }   
| p_word as x
		{d_printf "!found word: %s." x;
     WORD(x)
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
and ilist = 
  parse
  | p_ilist_separator as x
        {
            let _ = d_printf "!lexer: ilist separator: %s\n" x in
            let (y, zs, e) = ilist lexbuf in
            let l = (x, y) :: zs in
              ("", l, e)                           
        }

  | p_end_ilist as x 
      {
  	   let all = e ^ o ^ kindws ^ c in
       let _ = d_printf "!lexer: end of ilist: %s\n" all in
       let _ = do_end_ilist () in 
           ("", [], all)
      }
  | _  as x
        { let (y, zs, e) = ilist lexbuf in
            ((char_to_str x) ^ y, zs, e)
        }

and solution =
  parse
  | p_end_problem as x
        { 
            let _ = d_printf "!lexer: exiting solution\n" in
                x
        }
  | _  as x
        { let y = solution lexbuf in
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


(** BEGIN TRAILER **)
{
}
(** END TRAILER **)


