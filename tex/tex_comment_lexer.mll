(********************************************************************** 
 ** tex/tex_comment_lexer.mll
 **********************************************************************)

(** BEGIN: HEADER **)
{
open Printf
open Utils
open Tex_parser

(* Turn off prints *)
let d_printf args = 
    ifprintf stdout args
(*
let d_printf args = printf args
*)

(* Some Utilities *)
let start = Lexing.lexeme_start

let kw_sq_open = "["
let kw_sq_close = "]"


}
(** END: HEADER **)

(* horizontal space *)
let p_hspace = ' ' | '\t' | '\r' 
(* newline *)
let p_newline = '\n' | ('\r' '\n')
(* all white space *)
let p_ws = [' ' '\t' '\n' '\r']*	
let p_hs = [' ' '\t' '\r']*	

(* non-space character *)
let p_sigchar = [^ ' ' '\t' '%' '\n' '\r']
let p_alpha = ['a'-'z' 'A'-'Z']
let p_percent = "%"	
let p_esc_percent = "\\%"	


let p_run_star = "run" p_alpha*

(* begin end *)
let p_com_begin = '\\' "begin" p_ws												 
let p_com_end = '\\' "end" p_ws												 

(* braces *)
(* don't take newline with close *)
let p_o_curly = '{' p_ws
let p_c_curly = '}' p_hs
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											

(* Skips *)
(* href: the first argument, the url, could have % *)
let p_com_href = '\\' ("href" as kind) 
let p_com_lstinline = '\\' ("lstinline" as kind) 
let p_com_verb = '\\' ("verb" as kind)
let p_com_skip = p_com_href | p_com_lstinline | p_com_verb

let p_com_include = '\\' "include" p_ws '{' p_ws ([^ '}'] as filename) p_ws '}'
let p_com_input = '\\' "input" p_ws '{' p_ws ([^ '}'] as filename) p_ws '}'

let p_begin_env_comment = p_com_begin p_ws p_o_curly p_ws ("comment" as kind) p_ws p_c_curly
let p_end_env_comment = p_com_end p_ws p_o_curly p_ws ("comment") p_ws p_c_curly
let p_begin_env_lstlisting = (p_com_begin p_ws) (p_o_curly) ("lstlisting" as kind) p_ws (p_c_curly) 
let p_end_env_lstlisting = (p_com_end p_ws) (p_o_curly) ("lstlisting") (p_c_curly)
let p_begin_env_runlang = p_com_begin p_ws p_o_curly p_ws (p_run_star as kind) p_ws p_c_curly
let p_end_env_runlang = p_com_end p_ws p_o_curly p_ws (p_run_star as kind) p_ws p_c_curly
let p_begin_env_verbatim = p_com_begin p_ws p_o_curly p_ws ("verbatim" as kind) p_ws p_c_curly
let p_end_env_verbatim = p_com_end p_ws p_o_curly p_ws "verbatim" p_ws p_c_curly

let p_begin_env_skip = p_begin_env_lstlisting | p_begin_env_verbatim | p_begin_env_comment | p_begin_env_runlang

let p_env = (p_alpha)+('*')? p_ws
let p_end_env = p_com_end p_o_curly (p_env as kind) p_c_curly 

(** END PATTERNS *)			
(* Takes is_empty, the emptiness status of the current line *)
rule initial is_empty = 
parse

| (p_begin_env_skip as x)
    {
	   let _ = d_printf "! comment_lexer: skip env %s\n" x in
     let this_kind = kind in
     let env = skip_env kind lexbuf in
     let env = x ^ env in
	   let _ = d_printf "! comment_lexer: skipped env %s\n" env in
		 let rest = initial false lexbuf in
		 if this_kind = "comment" then
   		 rest
		 else
   		 env ^ rest
}

| p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 false kw_sq_open kw_sq_close lexbuf in
     let h = x ^ arg ^ c_sq in
     let body = skip_inline lexbuf in
		 let rest = initial false lexbuf in
     let all = h ^ body ^ rest in
     let _ = d_printf "skip_sq: %s\n" all in
     all
    }

  
| (p_com_skip p_ws p_o_curly) as x 
		{
     (* This is somewhat abusive. 
      * We are allowing all skip's to use {} arg's.
      *)
     let arg = skip_arg '}' lexbuf in
		 let rest = initial false lexbuf in
     let all = x ^ arg ^ rest in
      all
    }

| p_com_skip as x 
		{
(*     let _ = d_printf "!lexer found: p_com_skip %s." (str_of_char x) in  *)
     let inline = skip_inline lexbuf in
     let inline = x ^ inline in
	   let _ = d_printf "! comment_lexer: skipped inline %s\n" inline in
		 let rest = initial false lexbuf in
     inline ^ rest
    }

| p_hspace as x 
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
     let rest = initial is_empty lexbuf in
		 (str_of_char x) ^ rest
    }		

| p_esc_percent as x 
    {
     let _ = d_printf "!lexer matched espace percent %s." x in
     let rest = initial false lexbuf in
		 x ^ rest
    }		

| p_percent as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let body = take_comment lexbuf in
     let comment = (str_of_char x) ^ body in
     let _ = d_printf "comment taken:\n%s" body in
     (* If the line is empty, then we will nuke it.
        Otherwise, we will start a fresh line.
        In both cases, take the rest starting from the beginning of a line.
      *)
     let rest = initial true lexbuf in
		 if is_empty then
		   rest
		 else
       "\n" ^ rest
    }

| p_newline as x
		{
      let rest = initial true lexbuf in
			x ^ rest
    }
| p_hspace as x
		{
      let rest = initial is_empty lexbuf in
			(str_of_char x) ^ rest
    }

| eof
		{""}

| _ as x
   (* Non-space char *)
    {
     let x = (str_of_char x) in
     let _ = d_printf "!%s" x in
	   let rest = initial false lexbuf in 
     x ^ rest
    }		

and skip_inline = 		
  (* Skip inline command, e.g. \lstinline<delimiter> ... <delimeter> *)
  parse
  (* This should not work but it will be a common error. 
     And LaTeX seems allow it, probably because it boxes {...}.
   *)
  | '{' as x      
    { let x = str_of_char x in
(*  		let _ = d_printf "skip_inline kind = %s delimiter %s\n" x in *)
      let rest = skip_arg '}' lexbuf in
      x ^ rest
    } 

  | _ as x
    { let rest = skip_arg x lexbuf in
      (str_of_char x) ^ rest
    } 

and skip_arg delimiter_close = 
	  (* this is like take_arg but does not skip over comments *)
  parse
  | _ as x
    {
(*     let _ = d_printf "skip_arg x =  %c\n" x  in *)
     (* Tricky: check close first so that you can handle 
        a single delimeter used for both open and close,
        as in lstinline.
      *)
		 if x = delimiter_close then
       str_of_char x
     else
       let rest = skip_arg delimiter_close lexbuf in 
       (str_of_char x) ^ rest
    }

and skip_env stop_kind =
  (* Assumes non-nested environments *)
  parse
  | p_end_env as x
      { (* let _ = d_printf "!lexer: end skip environment %s\n" x in *)
          if kind = stop_kind then
						x
          else 
            let y = skip_env stop_kind lexbuf in
						x ^ y
      }
  | _  as x
      {(* let _ = d_printf "%c" x in *)
       let y = skip_env stop_kind lexbuf in
        (str_of_char x) ^ y
      }

and take_comment = 		
  parse
  | p_newline as x
    { (* let _ = d_printf "take_comment: newline %s" x in *)
        x
    } 
  | _ as x 
    {
     (* let _ = d_printf "take_comment: %s" (str_of_char x) in *)
     let comment = take_comment lexbuf in 
       (str_of_char x) ^ comment
    }

and take_arg depth is_empty delimiter_open delimiter_close = 
parse
| p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 false kw_sq_open kw_sq_close lexbuf in
     let h = x ^ arg ^ c_sq in
     let i = skip_inline lexbuf in
     let (rest, h_e) = take_arg depth false delimiter_open delimiter_close lexbuf inb
		 (h ^ i ^ rest, h_e)
    }
| p_esc_percent as x 
    {
     let _ = d_printf "!lexer matched espace percent %s." x in
     let (rest, h_e) = take_arg 1 false kw_sq_open kw_sq_close lexbuf in
		 (x ^ rest, h_e)
    }		

| p_percent as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let body = take_comment lexbuf in
     let comment = (str_of_char x) ^ body in
     let _ = d_printf "comment taken:\n%s" body in
     (* If the line is empty, then we will nuke it.
        Otherwise, we will start a fresh line.
        In both cases, take the rest starting from the beginning of a line.
      *)
     let (rest, h_e) = take_arg 1 false kw_sq_open kw_sq_close lexbuf in
		 if is_empty then
		   (rest, h_e)
		 else
       ("\n" ^ rest, h_e)
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
         let (arg, c_c) = take_arg depth false delimiter_open delimiter_close lexbuf in 
         (x ^ arg, c_c)					 
		 else if x = delimiter_open then
			 let depth = depth + 1 in
			 let (arg, c_c) = take_arg depth false delimiter_open delimiter_close lexbuf in 
			 (x ^ arg, c_c)
		 else
			 let (rest, c_c) = take_arg depth false delimiter_open delimiter_close lexbuf in
       (x ^ rest, c_c)
    }



(** BEGIN TRAILER **)
{
let lexer: Lexing.lexbuf -> string = 
		initial true
}
(** END TRAILER **)

