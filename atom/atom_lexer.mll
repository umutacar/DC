(** BEGIN: HEADER **)
{
open Printf
open Utils
open Atom_parser

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args
*)

(* Some Utilities *)
let start = Lexing.lexeme_start

(**********************************************************************
 ** BEGIN: latex env machinery 
 **********************************************************************)

let env_depth = ref 0  

let do_begin_env () =
  env_depth := !env_depth + 1

let do_end_env () =
  let _ = env_depth := !env_depth - 1 in
    !env_depth = 0

(**********************************************************************
 ** END: latex env machinery 
 **********************************************************************)

(**********************************************************************
 ** BEGIN: Argument depth machinery
 **********************************************************************)
let arg_depth = ref 0  

let inc_arg_depth () =
  arg_depth := !arg_depth + 1

let dec_arg_depth () =
  arg_depth := !arg_depth - 1

let arg_depth () =
  !arg_depth
(**********************************************************************
 ** END: argument depth machinery
 **********************************************************************)

}
(** END: HEADER **)

(** BEGIN: PATTERNS *)	
let p_comma = ','
(* horizontal space *)
let p_hspace = ' ' | '\t' | '\r' 
(* non-space character *)
let p_sigchar = [^ ' ' '\t' '%' '\n' '\r']
(* newline *)
let p_newline = '\n' | ('\r' '\n')
let p_percent = '%'	
let p_percent_esc = "\\%"	

let p_newline = '\n' | ('\r' '\n')
let p_comment_line = p_percent [^ '\n']* '\n'
let p_comment = p_percent [^ '\n']*
let p_headerskip = ('\n' | p_comment_line | p_hspace)*

let p_tab = '\t'	
let p_hs = [' ' '\t']*	
let p_ws = [' ' '\t' '\n' '\r']*	
let p_skip = p_hs
let p_par_break = '\n' p_ws '\n' p_hs

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

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = '{' p_ws
(* don't take newline with close *)
let p_c_curly = '}' p_hs

let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											
let p_special_percent = p_backslash p_percent

let p_com_begin = '\\' "begin" p_ws												 
let p_com_end = '\\' "end" p_ws												 

let p_com_ask = '\\' "ask"
let p_com_choice = '\\' "choice"
let p_com_choice_correct = '\\' "choice*"
let p_com_explain = '\\' "explain"
let p_com_fold = '\\' "fold"
let p_com_hint = '\\' "help"
let p_com_notes = '\\' "notes"
let p_com_rubric = '\\' "rubric"
let p_com_refsol = '\\' "sol"

let p_point_val = (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							

(* begin: verbatim 
 * we will treat verbatim as a "box"
 *)
let p_verbatim = "verbatim"
let p_begin_verbatim = p_com_begin p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
let p_end_verbatim = p_com_end p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
(* end: verbatim *)

let p_item = 
	(p_com_ask as kind) |
	(p_com_choice as kind) |
	(p_com_choice_correct as kind) |
	(p_com_explain as kind) |
  (p_com_hint as kind) |
  (p_com_notes as kind) |
  (p_com_refsol as kind) |
  (p_com_rubric as kind) 

let p_item_arg = 
  p_item p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 

(* Latex environment: alphabethical chars plus an optional star *)
let p_env = (p_alpha)+('*')?

let p_begin_list = "mambo"
let p_end_list = "mambo"

let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env) p_ws (p_c_curly) 
let p_begin_env_with_points = (p_com_begin p_ws) (p_o_curly) (p_env) (p_c_curly) p_ws (p_point_val as points)
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env) (p_c_curly) 


(** END PATTERNS *)			


rule initial = parse
| (p_begin_env_with_points as x) (p_o_sq as a)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ = inc_arg_depth () in
     let (title, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ title ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
     let _ = do_begin_env () in		
     let (lopt, body, items, h_e) = take_env lexbuf in
   	 let all = h_b ^ body ^ h_e in
       ATOM (Some point_val, Some title, lopt, body, items, all)
}

| p_begin_env_with_points as x
      { 
(*          let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
          let _ = do_begin_env () in		
          let (lopt, body, items, h_e) = take_env lexbuf in
					let all = x ^ body ^ h_e in
(*          let _ = d_printf "!lexer: latex env matched = %s.\n" (x ^ y) in *)
            ATOM(Some point_val, None, lopt, body, items, all)
          
      }   

| (p_begin_env as x) (p_o_sq as a)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ = inc_arg_depth () in
     let (title, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ title ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
     let _ = do_begin_env () in		
     let (lopt, body, items, h_e) = take_env lexbuf in
   	 let all = h_b ^ body ^ h_e in
            ATOM(None, Some title, lopt, body, items, all)
}

| p_begin_env as x
      { 
(*          let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
          let _ = do_begin_env () in		
          let (lopt, body, items, h_e) = take_env lexbuf in
   				let all = x ^ body ^ h_e in
(*          let _ = d_printf "!lexer: latex env matched = %s.\n" (x ^ y) in *)
            ATOM(None, None, lopt, body, items, all)
          
      }   

| eof
		{EOF}
| _
    {initial lexbuf}		

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


and take_env =
  parse
  | p_begin_verbatim as x
      { 
(*          let _ = d_printf "!lexer: entering verbatim\n" in *)
          let y = verbatim lexbuf in
          let _ = d_printf "!lexer: verbatim matched = %s" (x ^ y) in
          let (lopt, z, items, h_e) = take_env lexbuf in
            (lopt, x ^ y ^ z, items, h_e)          
      }   
	| p_begin_list as x
			{
		   (* This should be at the top level, not nested within other env's.         
        * It should also be at the tail of an environment.
        * TODO: check for these and return an error if not satisfied.
        *) 

        let _ = d_printf "* lexer: begin choices." in
        let (prefix, items) = take_list lexbuf in
        (* Prefix is what comes before the first choice, it should be whitespace
         * so it is dropped. 
         *)
        let _ = d_printf "* lexer: end list, leftover prefix = %s." prefix in 
        let (lopt__, y, items__, h_e) = take_env lexbuf in
	        (* No labels.
           * Drop items from body, by returning empty. 
           * The recursive call must not return items or anything else really.
           *)
 	        (None, y, items, h_e)
      }
  | p_begin_env as x
        {
(*            let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
            let _ = do_begin_env () in
            let (lopt, y, items, h_e) = take_env lexbuf in
                (lopt, x ^ y, items, h_e)              
        }

  | p_end_env as x
        { 
(*            let _ = d_printf "!lexer: end latex env: %s\n" x in *)
            let do_exit = do_end_env () in
                if do_exit then
(*                    let _ = d_printf "!lexer: exiting latex env\n" in *)
                        ( None, "", [], x)
                else
                    let (lopt, y, items, h_e) = take_env lexbuf in
                      (lopt, x ^ y, items, h_e)  
        }      
  | p_label_and_name as x
  		{ 
(*		    let _ = d_printf "!lexer matched label %s." x in *)
				let all = label_pre ^ label_name ^ label_post in
        let (lopt, y, items, h_e) = take_env lexbuf in
          (* Important: Drop inner label lopt *)
          (Some label_name, all ^ y, items, h_e)  
			}		
  | _  as x
        { let (lopt, y, items, h_e) = take_env lexbuf in
            (lopt, (str_of_char x) ^ y, items, h_e)
        }
and verbatim =
  parse
  | p_end_verbatim as x
        { 
            let _ = d_printf "!lexer: exiting verbatim\n" in
                x
        }
  | _  as x
        { let y = verbatim lexbuf in
            (str_of_char x) ^ y
        }
and take_arg = 
  parse 
  | p_o_curly as x
    {
     let _ = inc_arg_depth () in
     let (arg, c_c) = take_arg lexbuf in 
       (x ^ arg, c_c)
    }
  | (p_c_curly p_ws) as x
    {
     let _ = dec_arg_depth () in
       if arg_depth () = 0 then
           ("", x)
       else
         let (arg, c_c) = take_arg lexbuf in 
           (x ^ arg, c_c)
    }
  | _ as x
    {
     let (arg, c_c) = take_arg lexbuf in 
       ((str_of_char x) ^ arg, c_c)
    }
and take_opt_arg = 
  parse 
  | p_o_sq as x
    {
     let _ = inc_arg_depth () in
     let (arg, c_sq) = take_opt_arg lexbuf in 
       (x ^ arg, c_sq)
    }
  | (p_c_sq p_hs) as x
    {
     let _ = dec_arg_depth () in
       if arg_depth () = 0 then
           ("", x)
       else
         let (arg, c_sq) = take_opt_arg lexbuf in 
           (x ^ arg, c_sq)
    }
  | _ as x
    {
     let (arg, c_sq) = take_opt_arg lexbuf in 
       ((str_of_char x) ^ arg, c_sq)
    }

and take_list =
	 parse
	 | p_item_arg as x 
	 { let (body, items) = take_list lexbuf in
     let _ = d_printf "* lexer: item kind %s points = %s body = %s\n" kind point_val body in
	   let items = (kind, Some point_val, body)::items in
	     ("", items)	 	 
	 }
	 | p_item as x 
	 { let (body, items) = take_list lexbuf in
     let _ = d_printf "* lexer: item kind %s body = %s\n" kind body in
	   let items = (kind, None, body)::items in
	     ("", items)	 	 
	 }
   (* TODO: can we have environment inside metas?
    * In principle yes, so we need to nest these things.
    *)
	 | p_end_list as x 
	 { let _ = d_printf "* lexer: end list %s\n" x in
	     ("", [])	   
	 }

	 | _ as x 
	 { let (body, choices) = take_list lexbuf in
	   let body =  (str_of_char x) ^ body in
	     (body, choices)
	 }

(** BEGIN TRAILER **)
{
(* This is the default lexer *)
let lexer: Lexing.lexbuf -> token = 
		initial

}
(** END TRAILER **)


