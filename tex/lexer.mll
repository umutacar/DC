{
open Printf
open Parser

(* Debug prints *)
let debug = false
let d_printf args = 
  if debug then
    fprintf stdout args
  else 
    ifprintf stdout args
}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_percent = '%'
let p_comment_line = p_percent [^ '\n']* '\n'
let p_skip = p_ws

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_ws
let p_o_sq_bracket = '[' p_ws
let p_c_sq_bracket = ']' p_ws											
let p_special_percent = p_backslash p_percent

let p_label = '\\' "label" p_ws												 
let p_begin = '\\' "begin" p_ws												 
let p_end = '\\' "end" p_ws												 


let p_chapter = '\\' "chapter" p_ws
let p_section = '\\' "section" p_ws
let p_subsection = '\\' "subsection" p_ws
let p_subsubsection = '\\' "subsubsection" p_ws
let p_paragraph = '\\' "paragraph" p_ws												
let p_subparagraph = '\\' "subparagraph" p_ws												

let p_b_group = '\\' "begin{flex}" p_ws	
let p_e_group = '\\' "end{flex}" p_ws


let p_xxx = "xxx"
let p_b_xxx = '\\' "begin" p_o_curly p_xxx p_ws p_c_curly
let p_e_xxx = '\\' "end" p_o_curly p_xxx p_ws p_c_curly


let p_diderot_atom = "diderot" ['a'-'z''A'-'Z']*	
let p_algorithm = "algorithm"
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
let p_solution = "solution"
let p_syntax = "syntax"
let p_task = "task"
let p_teachask = "teachask"
let p_teachnote = "teachnote"
let p_theorem = "theorem"

let p_atom = ((p_diderot_atom as kind) p_ws as kindws) |
             ((p_algorithm as kind) p_ws as kindws) |
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
             ((p_solution as kind) p_ws as kindws) |
             ((p_syntax as kind) p_ws as kindws) |
             ((p_task as kind) p_ws as kindws) |
             ((p_teachask as kind) p_ws as kindws) |
             ((p_teachnote as kind) p_ws as kindws) |
             ((p_theorem as kind) p_ws as kindws) 

let p_begin_atom = (p_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) 
let p_end_atom = (p_end p_ws as e) (p_o_curly as o) p_atom (p_c_curly as c) 
let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 


(** END PATTERNS *)			


rule token = parse
| p_backslash as x
		{d_printf "!lexer matched: \\."; BACKSLASH(String.make 1 x)}				
| p_o_curly as x
		{d_printf "!lexer matched: %s.\n" x; O_CURLY(x)}				
| p_c_curly as x
		{d_printf "!lexer matched: %s.\n" x; C_CURLY(x)}				
| p_o_sq_bracket as x
		{d_printf "!lexer matched: %s.\n" x; O_SQ_BRACKET(x)}				
| p_c_sq_bracket as x
		{d_printf "!lexer matched: %s.\n" x; C_SQ_BRACKET(x)}				
| p_special_percent as x
		{d_printf "!lexer matched: %s.\n" x; PERCENT(x)}				


| p_comment_line as x
  	{d_printf "!lexer matched comment line %s." x; COMMENT_LINE(x)}		
| p_label as x
  	{d_printf "!lexer matched %s." x; KW_LABEL(x)}		
				
| p_chapter as x
  	{d_printf "!lexer matched %s." x; KW_CHAPTER(x)}		
| p_section as x
  	{d_printf "!lexer matched: %s." x; KW_SECTION(x)}		
| p_subsection as x
  	{d_printf "!lexer matched: %s." x; KW_SUBSECTION(x)}
| p_subsubsection as x
  	{d_printf "!lexer matched: %s." x; KW_SUBSUBSECTION(x)}
| p_paragraph as x
  	{d_printf "!lexer matched: %s." x; KW_PARAGRAPH(x)}				
| p_subparagraph as x
  	{d_printf "!lexer matched: %s." x; KW_SUBPARAGRAPH(x)}		

(* BEGIN: ATOMS *)
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
(* END ATOMS *)
| p_b_group as x
  	{d_printf "!lexer matched: %s." x; KW_BEGIN_GROUP(x)}		
| p_e_group as x
  	{d_printf "!lexer matched: %s." x; KW_END_GROUP(x)}

| p_word as x
		{d_printf "!found word: %s." x;
     WORD(x)
    }
| eof
		{EOF}
| _
    {token lexbuf}		
		
{
}
