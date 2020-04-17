(********************************************************************** 
 ** tex/tex_prompt_lexer.mll
 ** Lex and rewrite prompt body
 **********************************************************************)

(** BEGIN: HEADER **)
{
open Printf
open Utils

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args 
*)

(* let d_printf args = printf args *)

let kw_curly_open = "{"
let kw_curly_close = "}"
let kw_sq_open = "["
let kw_sq_close = "]"
let kw_math_open = "\\[ "
let kw_math_close = " \\]"
let kw_lstlisting = "lstlisting"
let kw_verbatim = "verbatim"

(* Keywords  for prompt and cookie rewrites .
 * The lexer rewrites "true false questions" as multiple choice
 * questions.
 *)
let kw_one_choice = "\\onechoice"
let kw_sol_true = "\\choice* True \\choice False"
let kw_sol_false = "\\choice True \\choice* False"


(* Some Utilities *)
let start = Lexing.lexeme_start
}
(** END: HEADER **)

(** BEGIN: PATTERNS *)	
let p_comma = ','
(* horizontal space *)
let p_hspace = ' ' | '\t' | '\r' 
(* non-space character *)
let p_sigchar = [^ ' ' '\t' '\n' '\r']
(* newline *)
let p_newline = '\n' | ('\r' '\n')
let p_esc_curly = "\\{"	 | "\\}"
let p_special_char = p_esc_curly


let p_newline = '\n' | ('\r' '\n')

let p_tab = '\t'	
let p_hs = [' ' '\t']*	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_skip = p_hs
let p_par_break = '\n' p_ws '\n' p_hs

let p_digit = ['0'-'9']
let p_integer = ['0'-'9']+
let p_frac = '.' p_digit*
let p_exp = ['e' 'E'] ['-' '+']? p_digit+
let p_float = p_digit* p_frac? p_exp?

let p_alpha = ['a'-'z' 'A'-'Z']
let p_separator = [':' '.' '-' '_' '/']

(* No white space after backslash *)
let p_backslash = '\\'
let p_double_backslash = '\\' '\\' 
let p_quad_backslash = '\\' '\\' '\\' '\\'
let p_o_curly = '{' p_ws
(* don't take newline with close *)
let p_c_curly = '}' p_hs
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											

let p_point_val = (p_o_sq as o_sq) (p_integer as point_val) p_ws '.' '0'? p_ws (p_c_sq as c_sq)

let p_com_begin = '\\' "begin" p_ws												 
let p_com_end = '\\' "end" p_ws												 
let p_com_fold = '\\' "fold" p_ws												 
let p_com_infer = '\\' "infer" p_ws												 
let p_com_lstinline = '\\' ("lstinline" as kind) p_ws
let p_com_verb = '\\' ("verb" as kind) p_ws
let p_com_skip = p_com_lstinline | p_com_verb
let p_com_caption = "\\caption"
let p_com_ask_true_false = "\\asktf"
let p_com_sol_true = "\\solt"
let p_com_sol_false = "\\solf"
let p_com_fillin = "\\fin"
    
(* Diderot commands *)
let p_com_attach = "\\" ("attach" as kind) p_ws
let p_com_download = "\\" ("download" as kind) p_ws
let p_com_video = "\\" ("video" as kind) p_ws
let p_com_diderot = p_com_attach | p_com_download | p_com_video

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							


(* Latex environment: alphabethical chars plus an optional star *)
let p_env = (p_alpha)+('*')?
let p_env_lstlisting = "lstlisting"
let p_env_verbatim = "verbatim"
let p_env_run_star = "run" p_alpha*


let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env) p_ws (p_c_curly) 
let p_begin_env_with_points = (p_com_begin p_ws) (p_o_curly) (p_env) (p_c_curly) p_ws (p_point_val as points)
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env as kind) (p_c_curly) 

let p_begin_env_lstlisting = (p_com_begin p_ws) (p_o_curly) (p_env_lstlisting as kind) p_ws (p_c_curly) 
let p_end_env_lstlisting = (p_com_end p_ws) (p_o_curly) (p_env_lstlisting) (p_c_curly)
let p_begin_env_run_star = (p_com_begin p_ws) (p_o_curly) (p_env_run_star as kind) p_ws (p_c_curly) 
let p_end_env_run_star = (p_com_end p_ws) (p_o_curly) (p_env_run_star) (p_c_curly)

let p_begin_env_verbatim = p_com_begin p_ws p_o_curly p_ws (p_env_verbatim as kind) p_ws p_c_curly
let p_end_env_verbatim = p_com_end p_ws p_o_curly p_ws p_env_verbatim p_ws p_c_curly

let p_begin_env_skip = p_begin_env_lstlisting | p_begin_env_run_star | p_begin_env_verbatim

(* end: environments *)



(** END PATTERNS *)			

(* Takes is_empty, the emptiness status of the current line *)
rule initial = 
parse

(* Rewrite \fin{argument} --> \_\_\_\_...\_ *)
| (p_com_fillin p_ws p_o_curly as  x)
		{
     let _ = d_printf "!prompt_lexer found: fillin" in
     let (arg, _) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
     let underscores = Utils.replace_with_underscores arg in
     let rest = initial lexbuf in
     underscores ^ rest
    }

| eof
		{""}

| _ as x
    {let rest = initial lexbuf in
     (str_of_char x) ^ rest
		}		

and skip_inline = 		
  (* Skip inline command, e.g. \lstinline<delimiter> ... <delimeter> *)
  parse
  (* This should work but it will be a common error. 
     And LaTeX seems allow it, probably because it boxes things.
   *)
  | '{' as x      
    { let x = str_of_char x in
(*  		let _ = printf "skip_inline kind = %s delimiter %s\n" kind x in *)
      let (rest, c) = skip_arg 1 "{" "}" lexbuf in
      let all =  x ^ rest ^ c in
(*			let _ = d_printf "skip_inline all = %s\n"  all in *)
        all
    } 

  | _ as x
    { let x = str_of_char x in
(*  		let _ = printf "skip_inline kind = %s delimiter %s\n" kind x in *)
      let (rest, c) = skip_arg 1 x x lexbuf in
      let all =  x ^ rest ^ c in
(*			let _ = d_printf "skip_inline all = %s\n"  all in *)
        all
    } 

and skip_env stop_kind =
  (* Assumes non-nested environments *)
  parse
  | p_end_env as x
      { (* let _ = d_printf "!lexer: exiting environment\n" in *)
          if kind = stop_kind then
						("", x)
          else 
            let (y, h_e) = skip_env stop_kind lexbuf in
						(x ^ y, h_e)
      }
  | _  as x
      { let (y, h_e) = skip_env stop_kind lexbuf in
        ((str_of_char x) ^ y, h_e)
      }

and take_arg_force =  
  (* Take argument of the form { ... }, skip whitespace at the start. *)
  parse
  | p_ws as x   
    {
       take_arg_force lexbuf 
    }
  | p_o_curly as x 
    {
     let (arg, c_c) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
       arg
    }   

and take_arg depth delimiter_open delimiter_close = 
  parse
  | p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let h = x ^ arg ^ c_sq in
     let i = skip_inline lexbuf in
     let (rest, h_e) = take_arg depth delimiter_open delimiter_close lexbuf in
		 (h ^ i ^ rest, h_e)
    }

  | _ as x
    {
     let x = str_of_char x in
(*     let _ = d_printf "take_arg x =  %s arg depth = %d\n" x (arg_depth ()) in  *)
     (* Tricky: check close first so that you can handle 
        a single delimeter used for both open and close,
        as in lstinline.
      *)
		 if x = delimiter_close then
			 let depth = depth - 1 in
       if depth = 0 then
(*				 let _ = d_printf "exit\n" in *)
         ("", x)
       else
         let (arg, c_c) = take_arg depth delimiter_open delimiter_close lexbuf in 
         (x ^ arg, c_c)					 
		 else if x = delimiter_open then
			 let depth = depth + 1 in
			 let (arg, c_c) = take_arg depth delimiter_open delimiter_close lexbuf in 
			 (x ^ arg, c_c)
		 else
			 let (rest, c_c) = take_arg depth delimiter_open delimiter_close lexbuf in
       (x ^ rest, c_c)
    }

and skip_arg depth delimiter_open delimiter_close = 
	  (* this is like take_arg but does not skip over comments *)
  parse
  | _ as x
    {
     let x = str_of_char x in
(*     let _ = printf "skip_arg x =  %s arg depth = %d\n" x depth  in *)
     (* Tricky: check close first so that you can handle 
        a single delimeter used for both open and close,
        as in lstinline.
      *)
		 if x = delimiter_close then
			 let depth = depth - 1 in
       if depth = 0 then
(*				 let _ = printf "exit\n" in *)
         ("", x)
       else
         let (arg, c_c) = skip_arg depth delimiter_open delimiter_close lexbuf in 
         (x ^ arg, c_c)					 
		 else if x = delimiter_open then
			 let depth = depth + 1 in
			 let (arg, c_c) = skip_arg depth delimiter_open delimiter_close lexbuf in 
			 (x ^ arg, c_c)
		 else
			 let (rest, c_c) = skip_arg depth delimiter_open delimiter_close lexbuf in
       (x ^ rest, c_c)
    }




(** BEGIN TRAILER **)
{
let lexer: Lexing.lexbuf -> string = 
		initial
}
(** END TRAILER **)

