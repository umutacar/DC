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
(*
let d_printf args = printf args 
*)
let kw_comment = "comment"
let kw_lstlisting = "lstlisting"
let kw_verbatim = "verbatim"



(* Some Utilities *)
let start = Lexing.lexeme_start

(**********************************************************************
 ** BEGIN: latex env machinery 
 **********************************************************************)

let env_depth = ref 0  

let do_reset_env () =
   env_depth := 0

let do_begin_env () =
   env_depth := !env_depth + 1

let do_end_env () =
	 let _ = env_depth := !env_depth - 1 in
	 let _ = d_printf "env depth = %d" !env_depth in
   !env_depth = 0

(**********************************************************************
 ** END: latex env machinery 
 **********************************************************************)

(**********************************************************************
 ** BEGIN: current atom
 **********************************************************************)

let current_atom = ref None  

let set_current_atom a =
  current_atom := Some a

let get_current_atom () =
  !current_atom

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

let mk_atom_str (h_b, body, capopt, items, h_e) = 
	 let items = str_of_items items in
   let cap = str_of_str_opt capopt in
	 let all_but_items = h_b ^ body ^ cap ^ h_e in
	 let all = h_b ^ body ^ cap ^ items ^ h_e in
     (all_but_items, all)	 	 

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
let p_com_part = '\\' "part"
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
  (p_com_part as kind) |
  (p_com_refsol as kind) |
  (p_com_rubric as kind) 

let p_item_arg = 
  p_item p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 

(* Latex environment: alphabethical chars plus an optional star *)
(* Latex environment: alphabethical chars plus an optional star *)
let p_env = (p_alpha)+('*')?
let p_env_comment = "comment"
let p_env_lstlisting = "lstlisting"
let p_env_verbatim = "verbatim"

let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env as kind) p_ws (p_c_curly) 
let p_begin_env_with_points = (p_com_begin p_ws) (p_o_curly) (p_env as kind) (p_c_curly) p_ws (p_point_val as points)
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env as kind) (p_c_curly) 

let p_begin_env_lstlisting = (p_com_begin p_ws) (p_o_curly) (p_env_lstlisting as kind) p_ws (p_c_curly) 
let p_end_env_lstlisting = (p_com_end p_ws) (p_o_curly) (p_env_lstlisting) (p_c_curly)
let p_begin_env_verbatim = p_com_begin p_ws p_o_curly p_ws (p_env_verbatim as kind) p_ws p_c_curly
let p_end_env_verbatim = p_com_end p_ws p_o_curly p_ws p_env_verbatim p_ws p_c_curly
let p_begin_env_skip = p_begin_env_lstlisting | p_begin_env_verbatim
(* end: environments *)

let p_caption = "\\caption"
let p_short_answer = "\\shortanswer"
let p_free_response = "\\freeresponse"
let p_one_choice  = "\\onechoice"
let p_any_choice = "\\anychoice"

let p_begin_list = 
	(p_short_answer as kind) | 
	(p_free_response as kind) | 
	(p_one_choice as kind) | 
	(p_any_choice as kind) 

let p_end_list = "mambo"

(** END PATTERNS *)			

rule initial = parse

| (p_begin_env_lstlisting as x) (p_o_sq as a)
    {
     let _ = d_printf "!!atom lexer matched begin lstlisting %s." x in 
     let _ = inc_arg_depth () in
     let (title, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ title ^ c_sq in
     let (body, h_e) = skip_env kw_lstlisting lexbuf in
   	 let all = h_b ^ body ^ h_e in
     let _ = d_printf "!atom lexer matched lstlisting\n %s." all in 
     ATOM(kind, None, Some title, None, body, None, [], all)
}

| (p_begin_env_skip as x)
    {
     let _ = d_printf "!atom lexer matched begin skip kind =  %s." kind in 
     let h_b = x in
     let (body, h_e) = skip_env kind lexbuf in
   	 let all = h_b ^ body ^ h_e in
     let _ = d_printf "!atom lexer matched skip environment\n %s." all in 
     ATOM(kind, None, None, None, body, None, [], all)
}
| (p_begin_env_with_points as x) (p_o_sq as a)
    {
     let _ = d_printf "!atom lexer: matched begin group %s." kind in 
	   let _ = set_current_atom kind in
     let _ = inc_arg_depth () in
     let (title, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ title ^ c_sq in
(*     let _ = d_printf "!atom lexer: matched group all: %s." h in  *)
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
       ATOM (kind, Some point_val, Some title, lopt, body, capopt, items, all)
}

| p_begin_env_with_points as h_b
    { 
     let _ = d_printf "!atom lexer: begin latex env: %s\n" h_b in 
	   let _ = set_current_atom kind in
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
(*          let _ = d_printf "!atom lexer: latex env matched = %s.\n" (x ^ y) in *)
     ATOM(kind, Some point_val, None, lopt, body, capopt, items, all)       
}   

| (p_begin_env as x) (p_o_sq as a)
    {
     let _ = d_printf "!atom lexer: matched begin group %s." kind in 
	   let _ = set_current_atom kind in
     let _ = inc_arg_depth () in
     let (title, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ title ^ c_sq in
(*     let _ = d_printf "!atom lexer: matched group all: %s." h in  *)
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
     ATOM(kind, None, Some title, lopt, body, capopt, items, all)
}

| p_begin_env as h_b
    { 
	   let _ = set_current_atom kind in
     let _ = d_printf "!atom lexer: begin latex env: %s\n" h_b in 
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
(*          let _ = d_printf "!atom lexer: latex env matched = %s.\n" (x ^ y) in *)
     ATOM(kind, None, None, lopt, body,  capopt, items, all)
       
    }   

| p_sigchar as x
		{
(*     d_printf "!%s" (char_to_str x); *)
     SIGCHAR(str_of_char x)
    }

| eof
		{EOF}
| _
    {initial lexbuf}		

and take_env =
  parse
  | p_begin_env_skip as x
      { 
(*          let _ = d_printf "!atom lexer: entering verbatim\n" in *)
       let (v_body, v_e) = skip_env kind lexbuf in
       let v = x ^ v_body ^ v_e in
       let _ = printf "!atom lexer: skip env matched = %s" v in
       let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
       (lopt, v ^ rest, capopt, items, h_e)          
      }   
	| p_begin_list as x
			{
		   (* This should be at the top level, not nested within other env's.         
        * It should also be at the tail of an environment.
        * TODO: check for these and return an error if not satisfied.
        *) 
        let kind_of_list = kind in
        let _ = d_printf "* lexer: begin choices kind = %s.\n" kind_of_list in
        let (body, items, h_e) = take_list lexbuf in
				let items = (kind_of_list, None, body)::items in 
          (* Drop items from body *)
 	        (None, "", None, items, h_e)
      }
  | p_begin_env as x
        {
            let _ = d_printf "!atom lexer: begin latex env: %s\n" x in 
            let _ = do_begin_env () in
            let (lopt, y, capopt, items, h_e) = take_env lexbuf in
                (lopt, x ^ y, capopt, items, h_e)              
        }
  | p_end_env as x
        { 
            let _ = d_printf "!atom lexer: end latex env: %s\n" x in 
            let do_exit = do_end_env () in
                if do_exit then
                  let _ = d_printf "!atom lexer: exiting latex env\n" in
                  ( None, "", None, [], x)
                else
                  let _ = d_printf "!atom lexer: not exiting env\n" in
                  let (lopt, y, capopt, items, h_e) = take_env lexbuf in
                  (lopt, x ^ y, capopt, items, h_e)  
        }      
  | p_label_and_name as x
  		{ 
(*		    let _ = d_printf "!atom lexer: matched label %s." x in *)
				let all = label_pre ^ label_name ^ label_post in
        let (lopt, y, capopt, items, h_e) = take_env lexbuf in
          (* Important: Drop inner label lopt *)
          (Some label_name, all ^ y, capopt, items, h_e)  
			}		
	| (p_caption p_ws p_o_curly) as x
		{
     let _ = inc_arg_depth () in
     let (arg, c_c) = take_arg lexbuf in
     let capopt = Some arg in
     let all = x ^ arg ^ c_c in
     let _ = d_printf "!atom lexer matched caption %s." all  in
		 let (lopt, y, capopt_, items, h_e) = take_env lexbuf in
     (* Drop capopt_, it would be another caption. *)
      (lopt,  all ^ y, capopt, items, h_e)
    }

  | _  as x
    { let (lopt, y, capopt, items, h_e) = take_env lexbuf in
      (lopt, (str_of_char x) ^ y, capopt, items, h_e)
    }

and skip_env stop_kind =
  (* Assumes non-nested environments *)
  parse
  | p_end_env as x
      {if kind = stop_kind then
				let _ = d_printf "!atom lexer: exiting environment stop = %s tk = %s\n" stop_kind x in
				("", x)
      else 
				let _ = d_printf "!atom lexer: skip_env, stop = %s: %s \n" stop_kind x in
        let (y, h_e) = skip_env stop_kind lexbuf in
				(x ^ y, h_e)
      }
  | _  as x
      { let _ = d_printf "!atom lexer: skip_env, stop = %s: %c \n" stop_kind x in
		    let (y, h_e) = skip_env stop_kind lexbuf in
        ((str_of_char x) ^ y, h_e)
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
	 { let (body, items, h_e) = take_list lexbuf in
     let _ = d_printf "* lexer: item kind %s points = %s body = %s\n" kind point_val body in
	   let items = (kind, Some point_val, body)::items in
	     ("", items, h_e)	 	 
	 }
	 | p_item as x 
	 { let (body, items, h_e) = take_list lexbuf in
     let _ = d_printf "* lexer: item kind %s body = %s\n" kind body in
	   let items = (kind, None, body)::items in
	     ("", items, h_e)	 	 
	 }

   | p_end_env as x
   { 
(*            let _ = d_printf "!atom lexer: end latex env: %s\n" x in *)
		 match get_current_atom () with
		 | None -> (printf "Fatal Error occured in atom_lexer.  No atom."; exit 1)
		 | Some atom ->
				 if kind = atom then 
					 ("", [], x)
				 else
					 let (body, items, h_e) = take_list lexbuf in
					 (x ^ body, items, h_e)  
  }      

	 | _ as x 
	 { let (body, items, h_e) = take_list lexbuf in
	   let body =  (str_of_char x) ^ body in
	     (body, items, h_e)
	 }

(** BEGIN TRAILER **)
{
(* This is the default lexer *)
let lexer: Lexing.lexbuf -> token = 
		initial

}
(** END TRAILER **)


(*
| (p_begin_env_lstlisting as x)
    {
     let _ = printf "!atom lexer matched begin lstlisting %s." x in 
     let h_b = x in
     let (body, h_e) = skip_env kw_lstlisting lexbuf in
   	 let all = h_b ^ body ^ h_e in
     let _ = printf "!atom lexer matched begin lstlisting\n %s." all in 
     ATOM(kind, None, None, None, body, None, [], all)
}

| (p_begin_env_verbatim as x)
    {
     let _ = printf "!atom lexer matched begin verbatim %s." x in 
     let (body, h_e) = skip_env kw_verbatim lexbuf in
   	 let all = x ^ body ^ h_e in
     let _ = printf "!atom lexer matched verbatim \n %s." all in 
		 (* Drop comments *)
     ATOM(kind, None, None, None, body, None, [], all)
}

  | p_begin_env_verbatim as x
      { 
(*          let _ = d_printf "!atom lexer: entering verbatim\n" in *)
       let (v_body, v_e) = skip_env kw_verbatim lexbuf in
       let v = x ^ v_body ^ v_e in
       let _ = d_printf "!atom lexer: verbatim matched = %s" v in
       let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
       (lopt, v ^ rest, capopt, items, h_e)          
      }   
	| (p_begin_env_lstlisting as x)
    {
     let _ = printf "!atom lexer matched begin lstlisting %s." x in 
     let (body, h_e) = skip_env kw_lstlisting lexbuf in
   	 let lst = x ^ body ^ h_e in
     let _ = printf "!atom lexer matched begin lstlisting\n %s." lst in 
     let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
     (lopt, lst ^ rest, capopt, items, h_e)
    }


*)
