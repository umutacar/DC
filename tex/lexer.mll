(** BEGIN: HEADER **)
{
open Printf
open Parser
open Utils

(* Some Utilities *)
let start = Lexing.lexeme_start
let char_to_str x = String.make 1 x
let pvalopt_to_str x = 
  match x with 
  | None -> "None"
  | Some x -> "Some" ^ x

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
let p_comma = ','
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_percent = '%'
let p_comment_line = p_percent [^ '\n']* '\n'
let p_skip = p_ws

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

let p_begin_code_atom = (p_com_begin p_ws as b) (p_o_curly as o) (p_ws p_code p_ws as kindws) (p_c_curly as c) 

let p_begin_code_atom_arg = (p_com_begin p_ws as b) (p_o_curly as o) (p_ws p_code p_ws as kindws) (p_c_curly as c) (p_o_sq as o_sq) (p_key_value_list as keyval) (p_c_sq as c_sq) 

let p_end_code_atom = (p_com_end p_ws as e) (p_o_curly as o) (p_ws p_code p_ws as kindws) (p_c_curly as c) 

(* This is special, we use it to detect the end of a solution *)
let p_end_problem = (p_com_end p_ws as e) (p_o_curly as o) ((p_problem as kind) p_ws as kindws) (p_c_curly as c) 

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

| p_com_depend as h_b 
      {
       let _ = d_printf "!lexer: begin depend:\n" in
       let (l, h_e) = depend lexbuf in
       let sl =  h_b ^ (String.concat "," l) ^ h_e in
       let _ = d_printf "!lexer: depend matched = %s" sl in
            KW_DEPEND(h_b, l, h_e)          
      }   

| p_label_and_name as x
  	{d_printf "!lexer matched %s." x; KW_LABEL_AND_NAME(label_pre ^ label_name ^ label_post, label_name)}		
| p_backslash as x
(*
| p_com_label as x
  	{d_printf "!lexer matched %s." x; KW_LABEL(x)}		
*)
				
| p_chapter as x
  	{d_printf "!lexer matched %s." x; KW_CHAPTER(x, None)}		
| p_chapter_with_points as x
  	{d_printf "!lexer matched %s." x; KW_CHAPTER(x, Some point_val)}		
| p_section as x
  	{d_printf "!lexer matched: %s." x; KW_SECTION(x, None)}		
| p_section_with_points as x
  	{let _ = d_printf "lexer matched section with points: %s" x in
       KW_SECTION(x, Some point_val)
    }		
| p_subsection as x
  	{d_printf "!lexer matched subsection: %s." x; KW_SUBSECTION(x, None)}

| p_subsection_with_points as x
  	{let _ = d_printf "lexer matched subsection with points: %s" x in
       KW_SUBSECTION(x, Some point_val)
    }		
| p_subsubsection as x
  	{d_printf "!lexer matched: %s." x; KW_SUBSUBSECTION(x, None)}

| p_subsubsection_with_points as x
  	{let _ = d_printf "lexer matched subsubsection with points: %s" x in
       KW_SUBSUBSECTION(x, Some point_val)
    }		
| p_paragraph as x
  	{d_printf "!lexer matched: %s." x; KW_PARAGRAPH(x, None)}
| p_paragraph_with_points as x
  	{let _ = d_printf "!lexer matched: %s." x in 
       KW_PARAGRAPH(x, Some point_val)
    }
(*
| p_b_cluster as x
  	{d_printf "!lexer matched: %s." x; KW_BEGIN_CLUSTER(x, None)}		
| p_b_cluster_with_points as x
  	{d_printf "!lexer matched: %s." x; KW_BEGIN_CLUSTER(x, Some pval)}		
| p_e_cluster as x
  	{d_printf "!lexer matched: %s." x; KW_END_CLUSTER(x)}
*)
| p_begin_group
  	{let all = b ^ o ^ kindws ^ c in
       d_printf "lexer matched begin group: %s" kind;
       KW_BEGIN_GROUP(kind, all, None)
    }		
| p_begin_group_with_points as x
  	{let all = b ^ o ^ kindws ^ c ^ o_sq ^ point_val ^ c_sq in
       d_printf "lexer matched begin group %s with points = %s" kind point_val;
       KW_BEGIN_GROUP(kind, all,  Some point_val)
    }		

| p_end_group
  	{let all = e ^ o ^ kindws ^ c in
       d_printf "lexer matched end group: %s" kind;
       KW_END_GROUP(kind, all)
    }		
| p_begin_atom
  	{let all = b ^ o ^ kindws ^ c in
       d_printf "lexer matched begin atom: %s" kind;
       KW_BEGIN_ATOM(kind, all, None)
    }		
| p_begin_atom_with_points
  	{let all = b ^ o ^ kindws ^ c ^ o_sq ^ point_val ^ c_sq in
       d_printf "lexer matched begin atom: %s" kind;
       KW_BEGIN_ATOM(kind, all, Some point_val)
    }		
| p_end_atom
  	{let all = e ^ o ^ kindws ^ c in
       d_printf "lexer matched end atom: %s" kind;
       KW_END_ATOM(kind, all)
    }		

| p_begin_code_atom_arg as h_b
    {
       let _ = printf "!lexer: begin code atom with arg\n" in
       let (label_opt, body, h_e) = code_atom lexbuf in
       let (h_e_a, h_e_b) = h_e in
       let _ = printf "!lexer: code atom matched = %s, h = %s h_e = %s, %s" body h_b h_e_a h_e_b in
         KW_CODE_ATOM((kindws, h_b), Some (o_sq, keyval, c_sq), label_opt, body, h_e)
    }		

| p_begin_code_atom as h_b
    {
       let _ = printf "!lexer: begin code atom\n" in
       let (label_opt, body, h_e) = code_atom lexbuf in
       let (h_e_a, h_e_b) = h_e in
       let _ = printf "!lexer: code atom matched = %s, h = %s h_e = %s, %s" body h_b h_e_a h_e_b in
         KW_CODE_ATOM((kindws, h_b), None, label_opt, body, h_e)
    }		


(*
| p_begin_code_atom_with_arg
  	{let all = b ^ o ^ kindws ^ c in
       d_printf "lexer matched begin atom: %s" kind;
       KW_BEGIN_ATOM(kind, all, None)
    }		
*)
| p_begin_ilist 
      {let kw_b = b ^ o ^ kindws ^ c in
       let _ = d_printf "!lexer: begin ilist: %s\n" kw_b in
       let _ = do_begin_ilist () in
       let (_, l, kw_e) = ilist lexbuf in

       let l_joined = List.map (fun (x, y, z) -> x ^ (pvalopt_to_str y) ^ z) l in
       let sl = kw_b ^ String.concat "," l_joined ^ kw_e in
       let _ = d_printf "!lexer: ilist matched = %s" sl in
            ILIST(kind, kw_b, None, l, kw_e)          
      }   
(* point values now go with atoms
| p_begin_ilist_arg 
      {let kw_b = b ^ o ^ kindws ^ c  in
       let kw_b_arg = kw_b ^ o_sq ^ point_val ^ c_sq in
       let _ = d_printf "!lexer: begin ilist: %s\n" kw_b_arg in
       let _ = do_begin_ilist () in
       let (_, l, kw_e) = ilist lexbuf in
       let l_joined = List.map (fun (x, y, z) -> x ^ (pvalopt_to_str y) ^ z) l in
       let sl = kw_b_arg ^ String.concat "," l_joined ^ kw_e in
       let _ = d_printf "!lexer: ilist matched = %s" sl in
            ILIST(kind, kw_b, Some (o_sq,  point_val, c_sq), l, kw_e)          
      }   
*)
| p_com_hint as h 
      {
       let _ = d_printf "!lexer: begin hint\n" in
       let (body, sol_opt, exp_opt, rubric_opt, h_e) = hint lexbuf in
       let (h_e_a, h_e_b) = h_e in
       let _ = d_printf "!lexer: hint matched = %s, h = %s h_e = %s, %s" body h h_e_a h_e_b in
       let _ = d_printf_opt_str "explain" exp_opt in
       let _ = d_printf_opt_str "solution" sol_opt in
       let _ = d_printf_opt_str "rubric" rubric_opt in
         HINT(h, body, sol_opt, exp_opt, rubric_opt, h_e)
      }   

| p_com_refsol as h 
      {
       let _ = d_printf "!lexer: begin refsol\n" in
       let (body, exp_opt, rubric_opt, h_e) = refsol lexbuf in
       let (h_e_a, h_e_b) = h_e in
       let _ = d_printf "!lexer: refsol matched = %s, h = %s h_e = %s, %s" body h h_e_a h_e_b in
       let _ = d_printf_opt_str "explain" exp_opt in
       let _ = d_printf_opt_str "rubric" rubric_opt in
         REFSOL(h, body, exp_opt, rubric_opt, h_e)
      }   

| p_com_rubric as h 
      {
       let _ = d_printf "!lexer: begin rubric\n" in
       let (body, h_e) = rubric lexbuf in
       let (h_e_a, h_e_b) = h_e in
       let _ = d_printf "!lexer: rubric matched = %s, h = %s h_e = %s, %s" body h h_e_a h_e_b in
         RUBRIC(h, body, h_e)
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
and depend = 
  parse
  | (p_label_name as x) p_ws p_comma p_ws 
        {
            let _ = d_printf "!lexer.depend: label %s\n" x in
            let (t, h_e) = depend lexbuf in
            let l = x :: t in
              (l, h_e)                           
        }
  | (p_label_name as x) (p_ws p_c_curly p_ws as h_e) 
        {
            let _ = d_printf "!lexer.depend: label %s\n" x in
              ([x], h_e)
        }
  | p_c_curly p_ws as x 
      {
         ([], x)
      }
and code_atom =
  parse
  | p_end_code_atom
    { 
  	 let all = e ^ o ^ kindws ^ c in
     let _ = printf "lexer matched end code atom kind: %s" kindws in
     let _ = printf "!lexer: exiting code atom\n" in
       (None, "", (kindws, all))
    }
  | p_label_and_name as x
  	{
     let _ = printf "!lexer.code_atom: matched label %s." x in
     let (label_all, name) = (label_pre ^ label_name ^ label_post, label_name) in
     let (_, body, h_e) = code_atom lexbuf in
       (
        Some (label_all, name),
        body, 
        h_e
       )
    }
  | _  as x
        { match code_atom lexbuf with 
          | (Some l, body, h_e) -> 
            (printf "Syntax Error: a label definition within code atoms not allowed!"; exit (-1))
          | (None, body, h_e) ->
            (None, (char_to_str x) ^ body, h_e)
        }

and ilist = 
  parse
  | p_ilist_separator as x
        {
            let _ = d_printf "!lexer: ilist separator: %s\n" x in
            let (y, zs, e) = ilist lexbuf in
            let l = (x, None, y) :: zs in
              ("", l, e)                           
        }

  | p_ilist_separator_arg as x
        {
            let _ = d_printf "!lexer: ilist separator: %s\n" x in
            let (y, zs, e) = ilist lexbuf in
            let l = (kind, Some point_val, y) :: zs in
              ("", l, e)                           
        }

  | p_end_ilist
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

and rubric = 
  parse 
  | p_end_atom
        { 
  	     let all = e ^ o ^ kindws ^ c in
         let _ = d_printf "lexer matched end problem: %s" kind in
         let _ = d_printf "!lexer: exiting rubric\n" in
           ("", (kind, all))
        }  
  | _  as x
        { let (body, h_e) = rubric lexbuf in
            ((char_to_str x) ^ body, h_e)
        }

and explain = 
  parse 
  | p_com_rubric
      {
       let _ = d_printf "!lexer: begin rubric\n" in
       let (body, h_e) = rubric lexbuf in
       let _ = d_printf "rubric matched = %s" body in
         ("", Some body, h_e)
      }   
  | p_end_atom
        { 
  	     let all = e ^ o ^ kindws ^ c in
         let _ = d_printf "lexer matched end problem: %s" kind in
         let _ = d_printf "!lexer: exiting explain\n" in
           ("", None, (kind, all))
        }  
  | _  as x
        { let (body, rubric_opt, h_e) = explain lexbuf in
            ((char_to_str x) ^ body, rubric_opt, h_e)
        }

and refsol =
  parse
  | p_end_atom
        { 
  	     let all = e ^ o ^ kindws ^ c in
         let _ = d_printf "lexer matched end problem: %s" kind in
         let _ = d_printf "!lexer: exiting refsol\n" in
           ("", None, None, (kind, all))
        }
  | p_com_explain
      {
       let _ = d_printf "!lexer: begin explain\n" in
       let (body, rubric_opt, h_e) = explain lexbuf in
       let _ = d_printf "explain matched = %s" body in
         ("", Some body, rubric_opt, h_e)
      }   
  | p_com_rubric
      {
       let _ = d_printf "!lexer: begin rubric\n" in
       let (body, h_e) = rubric lexbuf in
       let _ = d_printf "rubric matched = %s" body in
         ("", None, Some body, h_e)
      }   
  | _  as x
        { let (body, exp, rubric_opt, h_e) = refsol lexbuf in
            ((char_to_str x) ^ body, exp, rubric_opt, h_e)
        }

and hint =
  parse
  (* just a hint, no solution, no explanation *)
  | p_end_atom
        { 
  	     let all = e ^ o ^ kindws ^ c in
         let _ = d_printf "lexer matched end problem: %s" kind in
         let _ = d_printf "!lexer: exiting refsol\n" in
           ("", None, None, None, (kind, all))
        }

  (* hint + refsol plus optional explanation *) 
  | p_com_refsol
      {
       let _ = d_printf "!lexer: begin refsol\n" in
       let (body, exp_opt, rubric_opt, h_e) = refsol lexbuf in
       let _ = d_printf "refsol matched = %s" body in
         ("", Some body, exp_opt, rubric_opt, h_e)
      }   

  (* hint + explanation, no solution *)
  | p_com_explain
      {
       let _ = d_printf "!lexer: begin explain\n" in
       let (body, rubric_opt, h_e) = explain lexbuf in
       let _ = d_printf "explain matched = %s" body in
         ("", None, Some body, rubric_opt, h_e)
      }   

  (* hint + rubric, no solution, no explanation *)
  | p_com_rubric
      {
       let _ = d_printf "!lexer: begin rubric\n" in
       let (body, h_e) = rubric lexbuf in
       let _ = d_printf "rubric matched = %s" body in
         ("", None, None, Some body, h_e)
      }   

  | _  as x
        { let (body, sol_opt, exp_opt, rubric_opt, h_e) = hint lexbuf in
            ((char_to_str x) ^ body, sol_opt, exp_opt, rubric_opt, h_e)
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


