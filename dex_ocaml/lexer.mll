{
open Core
open Printf
open Parser
}

(** BEGIN: PATTERNS *)	
let p_space = ' '
let p_newline = '\n'
let p_tab = '\t'	
let p_ws = [' ' '\t' '\n']*	
let p_percent = '%'
let p_comment_line = p_percent [^ '\n']* '\n'
let p_skip = p_ws

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_ws
let p_o_sq_bracket = '[' p_ws
let p_c_sq_bracket = ']' p_ws											

let p_label = '\\' "label" p_ws												 
let p_definition = "definition" p_ws
let p_example = "example" p_ws

let p_chapter = '\\' "chapter" p_ws
let p_section = '\\' "section" p_ws
let p_subsection = '\\' "subsection" p_ws
let p_subsubsection = '\\' "subsubsection" p_ws
let p_paragraph = '\\' "paragraph" p_ws												
let p_subparagraph = '\\' "subparagraph" p_ws												

let p_xxx = "xxx"
let p_b_xxx = '\\' "begin" p_o_curly p_xxx p_ws p_c_curly
let p_e_xxx = '\\' "end" p_o_curly p_xxx p_ws p_c_curly


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
let p_teachask = "teachask"
let p_theorem = "theorem"

let p_b_group = '\\' "begin{group}" p_ws	
let p_e_group = '\\' "end{group}" p_ws

let p_begin = '\\' "begin" p_ws				
let p_end = '\\' "end" p_ws

let p_b_algorithm = '\\' "begin" p_o_curly p_algorithm p_ws p_c_curly
let p_e_algorithm = '\\' "end" p_o_curly p_algorithm p_ws p_c_curly
let p_b_code = '\\' "begin" p_o_curly p_code p_ws p_c_curly
let p_e_code = '\\' "end" p_o_curly p_code p_ws p_c_curly
let p_b_corollary = '\\' "begin" p_o_curly p_corollary p_ws p_c_curly
let p_e_corollary = '\\' "end" p_o_curly p_corollary p_ws p_c_curly
let p_b_costspec = '\\' "begin" p_o_curly p_costspec p_ws p_c_curly
let p_e_costspec = '\\' "end" p_o_curly p_costspec p_ws p_c_curly
let p_b_datastr = '\\' "begin" p_o_curly p_datastr p_ws p_c_curly
let p_e_datastr = '\\' "end" p_o_curly p_datastr p_ws p_c_curly
let p_b_datatype = '\\' "begin" p_o_curly p_datatype p_ws p_c_curly
let p_e_datatype = '\\' "end" p_o_curly p_datatype p_ws p_c_curly
let p_b_definition = '\\' "begin" p_o_curly p_definition p_ws p_c_curly
let p_e_definition = '\\' "end" p_o_curly p_definition p_ws p_c_curly
let p_b_example = '\\' "begin" p_o_curly p_example p_ws p_c_curly
let p_e_example = '\\' "end" p_o_curly p_example p_ws p_c_curly
let p_b_exercise = '\\' "begin" p_o_curly p_exercise p_ws p_c_curly
let p_e_exercise = '\\' "end" p_o_curly p_exercise p_ws p_c_curly
let p_b_gram = '\\' "begin" p_o_curly p_gram p_ws p_c_curly
let p_e_gram = '\\' "end" p_o_curly p_gram p_ws p_c_curly
let p_b_hint = '\\' "begin" p_o_curly p_hint p_ws p_c_curly
let p_e_hint = '\\' "end" p_o_curly p_hint p_ws p_c_curly
let p_b_important = '\\' "begin" p_o_curly p_important p_ws p_c_curly
let p_e_important = '\\' "end" p_o_curly p_important p_ws p_c_curly
let p_b_lemma = '\\' "begin" p_o_curly p_lemma p_ws p_c_curly
let p_e_lemma = '\\' "end" p_o_curly p_lemma p_ws p_c_curly
let p_b_note = '\\' "begin" p_o_curly p_note p_ws p_c_curly
let p_e_note = '\\' "end" p_o_curly p_note p_ws p_c_curly
let p_b_preamble = '\\' "begin" p_o_curly p_preamble p_ws p_c_curly
let p_e_preamble = '\\' "end" p_o_curly p_preamble p_ws p_c_curly
let p_b_problem = '\\' "begin" p_o_curly p_problem p_ws p_c_curly
let p_e_problem = '\\' "end" p_o_curly p_problem p_ws p_c_curly
let p_b_proof = '\\' "begin" p_o_curly p_proof p_ws p_c_curly
let p_e_proof = '\\' "end" p_o_curly p_proof p_ws p_c_curly
let p_b_proposition = '\\' "begin" p_o_curly p_proposition p_ws p_c_curly
let p_e_proposition = '\\' "end" p_o_curly p_proposition p_ws p_c_curly
let p_b_remark = '\\' "begin" p_o_curly p_remark p_ws p_c_curly
let p_e_remark = '\\' "end" p_o_curly p_remark p_ws p_c_curly
let p_b_solution = '\\' "begin" p_o_curly p_solution p_ws p_c_curly
let p_e_solution = '\\' "end" p_o_curly p_solution p_ws p_c_curly
let p_b_syntax = '\\' "begin" p_o_curly p_syntax p_ws p_c_curly
let p_e_syntax = '\\' "end" p_o_curly p_syntax p_ws p_c_curly
let p_b_teachask = '\\' "begin" p_o_curly p_teachask p_ws p_c_curly
let p_e_teachask = '\\' "end" p_o_curly p_teachask p_ws p_c_curly
let p_b_theorem = '\\' "begin" p_o_curly p_theorem p_ws p_c_curly
let p_e_theorem = '\\' "end" p_o_curly p_theorem p_ws p_c_curly

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 


(** END PATTERNS *)			


rule token = parse
| p_backslash as x
		{printf "!matched: \\."; BACKSLASH(Char.to_string x)}				
| p_o_curly as x
		{printf "!matched: {."; O_CURLY(x)}				
| p_c_curly as x
		{printf "!matched: }.\n"; C_CURLY(x)}				

| p_o_sq_bracket as x
		{printf "!matched: [."; O_SQ_BRACKET(x)}				
| p_c_sq_bracket as x
		{printf "!matched: ].\n"; C_SQ_BRACKET(x)}				

| p_comment_line as x
  	{printf "!matched comment line %s." x; COMMENT_LINE(x)}		

| p_label as x
  	{printf "!matched %s." x; KW_LABEL(x)}		
				
| p_chapter as x
  	{printf "!matched %s." x; KW_CHAPTER(x)}		
| p_section as x
  	{printf "!matched: %s." x; KW_SECTION(x)}		
| p_subsection as x
  	{printf "!matched: %s." x; KW_SUBSECTION(x)}
| p_subsubsection as x
  	{printf "!matched: %s." x; KW_SUBSUBSECTION(x)}
| p_paragraph as x
  	{printf "!matched: %s." x; KW_PARAGRAPH(x)}				
| p_subparagraph as x
  	{printf "!matched: %s." x; KW_SUBPARAGRAPH(x)}		
(*
| (p_begin) (p_o_curly) (p_word ) (p_c_curly)
  	{printf "matched: begin{word} %s" "xword"; KW_BEGIN("x")}		
*)
| p_begin as x
  	{printf "%s" x; KW_BEGIN(x)}		
| p_end as x
  	{printf "%s" x; KW_END(x)}		
(* BEGIN: ATOMS *)
| p_b_algorithm as x
  	{printf "matched begin algorithm: %s" x; KW_BEGIN_ALGORITHM(x)}		
| p_e_algorithm as x
  	{printf "matched end algorithm: %s" x; KW_END_ALGORITHM(x)}		
| p_b_code as x
  	{printf "matched begin code %s" x; KW_BEGIN_CODE(x)}		
| p_e_code as x
  	{printf "matched end code: %s" x; KW_END_CODE(x)}		
| p_b_corollary as x
  	{printf "matched begin corollary %s" x; KW_BEGIN_COROLLARY(x)}		
| p_e_corollary as x
  	{printf "matched end corollary: %s" x; KW_END_COROLLARY(x)}		
| p_b_costspec as x
  	{printf "matched begin COSTSPEC %s" x; KW_BEGIN_COSTSPEC(x)}		
| p_e_costspec as x
  	{printf "matched end COSTSPEC %s" x; KW_END_COSTSPEC(x)}		
| p_b_datastr as x
  	{printf "matched begin DATASTR %s" x; KW_BEGIN_DATASTR(x)}		
| p_e_datastr as x
  	{printf "matched end DATASTR %s" x; KW_END_DATASTR(x)}		
| p_b_datatype as x
  	{printf "matched begin DATATYPE %s" x; KW_BEGIN_DATATYPE(x)}		
| p_e_datatype as x
  	{printf "matched end DATATYPE %s" x; KW_END_DATATYPE(x)}		
| p_b_definition as x
  	{printf "matched begin definition: %s" x; KW_BEGIN_DEFINITION(x)}		
| p_e_definition as x
  	{printf "matched end definition: %s" x; KW_END_DEFINITION(x)}		
| p_b_example as x
  	{printf "matched begin example: %s" x; KW_BEGIN_EXAMPLE(x)}		
| p_e_example as x
  	{printf "matched end example: %s" x; KW_END_EXAMPLE(x)}		
| p_b_exercise as x
  	{printf "matched begin EXERCISE %s" x; KW_BEGIN_EXERCISE(x)}		
| p_e_exercise as x
  	{printf "matched end EXERCISE %s" x; KW_END_EXERCISE(x)}		
| p_b_gram as x
  	{printf "matched begin GRAM %s" x; KW_BEGIN_GRAM(x)}		
| p_e_gram as x
  	{printf "matched end GRAM %s" x; KW_END_GRAM(x)}		
| p_b_hint as x
  	{printf "matched begin HINT %s" x; KW_BEGIN_HINT(x)}		
| p_e_hint as x
  	{printf "matched end HINT %s" x; KW_END_HINT(x)}		
| p_b_important as x
  	{printf "matched begin IMPORTANT %s" x; KW_BEGIN_IMPORTANT(x)}		
| p_e_important as x
  	{printf "matched end IMPORTANT %s" x; KW_END_IMPORTANT(x)}		
| p_b_lemma as x
  	{printf "matched begin LEMMA %s" x; KW_BEGIN_LEMMA(x)}		
| p_e_lemma as x
  	{printf "matched end LEMMA %s" x; KW_END_LEMMA(x)}		
| p_b_note as x
  	{printf "matched begin NOTE %s" x; KW_BEGIN_NOTE(x)}		
| p_e_note as x
  	{printf "matched end NOTE %s" x; KW_END_NOTE(x)}		
| p_b_preamble as x
  	{printf "matched begin PREAMBLE %s" x; KW_BEGIN_PREAMBLE(x)}		
| p_e_preamble as x
  	{printf "matched end PREAMBLE %s" x; KW_END_PREAMBLE(x)}		
| p_b_problem as x
  	{printf "matched begin PROBLEM %s" x; KW_BEGIN_PROBLEM(x)}		
| p_e_problem as x
  	{printf "matched end PROBLEM %s" x; KW_END_PROBLEM(x)}		
| p_b_proof as x
  	{printf "matched begin PROOF %s" x; KW_BEGIN_PROOF(x)}		
| p_e_proof as x
  	{printf "matched end PROOF %s" x; KW_END_PROOF(x)}		
| p_b_proposition as x
  	{printf "matched begin PROPOSITION %s" x; KW_BEGIN_PROPOSITION(x)}		
| p_e_proposition as x
  	{printf "matched end PROPOSITION %s" x; KW_END_PROPOSITION(x)}		
| p_b_remark as x
  	{printf "matched begin REMARK %s" x; KW_BEGIN_REMARK(x)}		
| p_e_remark as x
  	{printf "matched end REMARK %s" x; KW_END_REMARK(x)}		
| p_b_solution as x
  	{printf "matched begin SOLUTION %s" x; KW_BEGIN_SOLUTION(x)}		
| p_e_solution as x
  	{printf "matched end SOLUTION %s" x; KW_END_SOLUTION(x)}		
| p_b_syntax as x
  	{printf "matched begin SYNTAX %s" x; KW_BEGIN_SYNTAX(x)}		
| p_e_syntax as x
  	{printf "matched end SYNTAX %s" x; KW_END_SYNTAX(x)}		
| p_b_teachask as x
  	{printf "matched begin TEACHASK %s" x; KW_BEGIN_TEACHASK(x)}		
| p_e_teachask as x
  	{printf "matched end TEACHASK %s" x; KW_END_TEACHASK(x)}		
| p_b_theorem as x
  	{printf "matched begin THEOREM %s" x; KW_BEGIN_THEOREM(x)}		
| p_e_theorem as x
  	{printf "matched end THEOREM %s" x; KW_END_THEOREM(x)}		
(* END ATOMS *)
| p_b_group as x
  	{printf "!matched: %s." x; KW_BEGIN_GROUP(x)}		
| p_e_group as x
  	{printf "!matched: %s." x; KW_END_GROUP(x)}

| p_word as x
		{printf "!found word: %s." x;
     WORD(x)
    }
| eof
		{EOF}
| _
    {token lexbuf}		
		
{
}
