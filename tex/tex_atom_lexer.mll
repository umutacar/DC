(********************************************************************** 
 ** atom/atom_lexel.mll
 **********************************************************************)

(** BEGIN: HEADER **)
{
open Printf
open Utils
open Tex_atom_parser
module Tex = Tex_syntax
module Prompt_lexer = Tex_prompt_lexer

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args


let d_printf args = printf args 
*)
let kw_curly_open = "{"
let kw_curly_close = "}"
let kw_sq_open = "["
let kw_sq_close = "]"
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
(*	 let _ = d_printf "env depth = %d" !env_depth in *)
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

let normalize_env (kind, title, kw_args) =
	let _ = d_printf "normalize_env_title: kind = %s, title = %s\n" kind title in

  (* Drop final star if env name has one. *)
  let kind = 
		if kind = "table*" || kind = "figure*" then
			Utils.drop_final_char kind
		else
			kind
	in
	let _ = d_printf "kind = %s title = %s\n" kind title in
  if kind = "table" || kind = "figure" then
    if Tex.title_is_significant title then
      (kind, Some title, ("title", title)::kw_args)
		else
   		let _ = d_printf "normalize_env_title: title is not significant.\n" in
			(kind, None, kw_args)
	else
		if title = "" then
			(kind, None, kw_args)
		else
			(kind, Some title, ("title", title)::kw_args)
		
(**********************************************************************
 ** END: latex env machinery 
 **********************************************************************)

let rewrite_prompt_body (body: string) =
  let lexbuf = Lexing.from_string body in
  (* Rewrie body *)
  let body_new = Prompt_lexer.lexer Constants.Prompt_Mode_Question lexbuf in
	let _ = d_printf "\nrewrite_prompt: body_old: %s\n" body in
	let _ = d_printf "rewrite_prompt: body_new: %s\n" body_new in
  body_new

let rewrite_prompt (kind: string) 
                   (tag: string option) 
                   (point_val_opt: string option) 
                   (lopt: string option) 
                   (body: string) = 
  let _ = d_printf "rewrite prompt kind = %s label = %s\n" kind (str_of_str_opt lopt) in
  if Tex.is_prompt_refsol_fillin kind then          
    (* Question version *) 
    (* Use the supplied labels for the question version. *)
		let lexbuf = Lexing.from_string body in
		let body_ask = Prompt_lexer.lexer Constants.Prompt_Mode_Question lexbuf in 
    let prompt_ask = (Tex.kw_refsol_fillin_ask, tag, point_val_opt, lopt, body_ask) in

    (* Solution versionsion *) 
    (* For this version, do not use the supplied labels *)
		let lexbuf = Lexing.from_string body in
		let body_sol = Prompt_lexer.lexer Constants.Prompt_Mode_Solution lexbuf in
    let prompt_sol = (Tex.kw_refsol_fillin_sol, None, None, None, body_sol) in

    (* Include both *)
      [prompt_ask; prompt_sol]
  else
    let prompt = (kind, tag, point_val_opt, lopt, body) in
		[prompt]

let mk_atom_str (h_b, body, capopt, items, h_e) = 
	 let items = str_of_items items in
   let cap = str_of_str_opt capopt in
	 let all_but_items = h_b ^ body ^ cap ^ h_e in
	 let all = h_b ^ body ^ cap ^ items ^ h_e in
     (all_but_items, all)	 	 

let mk_heading (heading, title, kw_args) =
	let title = 
		match title with 
		| None -> ""
		| Some t -> "[" ^ t ^ "]" 
	in
  match kw_args with
  | [ ] ->  heading ^ title 
  | (l: (string * string) list) -> 
    heading ^ title ^ "[" ^ (str_of_str2_list l) ^ "]"

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
let p_ws_hard = [' ' '\t' '\n' '\r']+	
let p_skip = p_hs
let p_par_break = '\n' p_ws '\n' p_hs

let p_emptyline = [' ' '\t' '\r']* '\n'
let p_emptyline = [' ' '\t' '\r']* '\n'
let p_nonemptyline = [' ' '\t']* [^ ' ' '\t' 'r' '\n']+ [^ '\r' '\n']*  ['r']? '\n' 

let p_digit = ['0'-'9']
let p_integer = ['0'-'9']+
let p_frac = '.' p_digit*
let p_float = p_digit* p_frac?
let p_weight = ['+' '-']? ('0'? '.' p_digit*) | ('1' '.' '0'?)

let p_alpha = ['a'-'z' 'A'-'Z']
(* Parantheses are only allowable for Diderot LaTeX outputs*)
let p_separator = [':' '.' '-' '_' '/' '(' ')']  
let p_keyword = p_alpha+

(* No white space after backslash *)
let p_backslash = '\\'
let p_o_curly = '{' p_ws
(* don't take newline with close *)
let p_c_curly = '}' p_hs

let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											
let p_o_sq_and_kw = '[' p_ws (p_keyword as keyword) p_ws '=' 

let p_special_percent = p_backslash p_percent

let p_com_begin = '\\' "begin" p_ws												 
let p_com_end = '\\' "end" p_ws												 
let p_com_lstinline = '\\' ("lstinline" as kind) p_ws
let p_com_skip = p_com_lstinline

let p_com_algo = '\\' "algo"
let p_com_auto_grade_one = '\\' "agone"
let p_com_auto_grade_any = '\\' "agany"
let p_com_manu_grade_one = '\\' "gone"
let p_com_manu_grade_any = '\\' "gany"


let p_com_algo_c = '\\' "algoc"
let p_com_algo_g = '\\' "algog"
let p_com_choice = '\\' "choice"
let p_com_continue = '\\' "continue"
let p_com_choice_correct = '\\' "choice*"
let p_com_explain = '\\' "explain"
let p_com_fold = '\\' "fold"
let p_com_hint = '\\' "hint"
let p_com_notes = '\\' "notes"
let p_com_part = '\\' "part"
let p_com_rubric = '\\' "rubric"
let p_com_refsol = '\\' "sol"
let p_com_refsol_fillin = '\\' "solfin"
let p_com_refsol_fillin_ask = '\\' "solfinask"
let p_com_refsol_fillin_sol = '\\' "solfinsol"
let p_com_refsol_false = '\\' "solf"
let p_com_refsol_true = '\\' "solt"
let p_com_score = '\\' "sc"

let p_com_ask = "\\ask"
let p_com_ask_true_false = "\\asktf"
let p_com_short_answer = "\\asks"
let p_com_one_choice  = "\\onechoice"
let p_com_any_choice = "\\anychoice"

let p_tag = (p_alpha | p_digit)+
let p_tag_included = '(' p_hs (p_tag as tag) p_hs ')'
let p_point_val = (p_o_sq as o_sq) (p_integer as point_val) p_ws '.' '0'? p_ws (p_c_sq as c_sq)
(* item point values can be floating point *)
let p_item_point_val = (p_o_sq as o_sq)  p_tag_included? (p_float as point_val)? p_ws (p_c_sq as c_sq)
let p_item_weight_val = (p_o_sq as o_sq) p_tag_included? (p_weight as weight)? p_ws (p_c_sq as c_sq)

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							


(* begin: verbatim 
 * we will treat verbatim as a "box"
 *)
let p_verbatim = "verbatim"
let p_begin_verbatim = p_com_begin p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
let p_end_verbatim = p_com_end p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
(* end: verbatim *)

let p_primary_item = 
	(p_com_algo as kind) | 
	(p_com_ask as kind) | 
	(p_com_short_answer as kind) | 
	(p_com_one_choice as kind) | 
	(p_com_any_choice as kind) |
	(p_com_ask_true_false as kind) 

let p_item = 
  (p_primary_item as kind) | 
	(p_com_choice as kind) |
	(p_com_choice_correct as kind) |
	(p_com_continue as kind) |
  (p_com_part as kind) |
  (p_com_refsol as kind) |
  (p_com_refsol_fillin as kind) |
  (p_com_refsol_fillin_ask as kind) |
  (p_com_refsol_fillin_sol as kind) |
  (p_com_refsol_false as kind) |
  (p_com_refsol_true as kind) 

let p_item_weighted = 
	(p_com_explain as kind) |
  (p_com_hint as kind) |
	(p_com_score as kind)| 
  (p_com_notes as kind) |
  (p_com_rubric as kind) |
  (p_com_algo_c as kind) |
  (p_com_algo_g as kind) |
  (p_com_auto_grade_one as kind) |
  (p_com_auto_grade_any as kind) |
  (p_com_manu_grade_one as kind) |
  (p_com_manu_grade_any as kind) 

let p_item_and_points =  p_item p_ws  p_item_point_val
let p_item_and_weight =  p_item_weighted p_ws  p_item_weight_val
let p_primary_item_points =  p_primary_item p_ws  p_item_point_val

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

(** END PATTERNS *)			

rule initial = parse

| (p_begin_env_lstlisting as x) (p_o_sq as a)
    {
     let _ = d_printf "!!atom lexer matched begin lstlisting %s." x in 
     let (title, kw_args) = take_atom_args 1 lexbuf in
     let h_b = mk_heading (x, Some title, kw_args) in
     let kw_args = ["title", title] @ kw_args in 
     let (body, h_e) = skip_env kw_lstlisting lexbuf in
   	 let all = h_b ^ body ^ h_e in
     let _ = d_printf "!atom lexer matched lstlisting\n %s." all in 
     ATOM(kind, None, kw_args, None, body, None, [], all)
}

| (p_begin_env_skip as x)
    {
     let _ = d_printf "!atom lexer matched begin skip kind =  %s." kind in 
     let h_b = x in
     let (body, h_e) = skip_env kind lexbuf in
   	 let all = h_b ^ body ^ h_e in
     let _ = d_printf "!atom lexer matched skip environment\n %s." all in 
     ATOM(kind, None, [], None, body, None, [], all)
}

| (p_begin_env_with_points as x) (p_o_sq as a)
    {
     let _ = d_printf "!atom lexer: matched begin env %s." kind in 
	   let _ = set_current_atom kind in
     let (title, kw_args) = take_atom_args 1 lexbuf in
     let (kind, title_opt, kw_args) = normalize_env (kind, title, kw_args) in
     let h_b = mk_heading (x, title_opt, kw_args) in
(*     let _ = d_printf "!atom lexer: matched group all: %s." h in  *)
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
       ATOM (kind, Some point_val, kw_args, lopt, body, capopt, items, all)
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
     ATOM(kind, Some point_val, [], lopt, body, capopt, items, all)       
}   

| (p_begin_env as x) (p_o_sq as a)
    {
     let _ = d_printf "!atom lexer: matched begin env %s.\n" kind in 
	   let _ = set_current_atom kind in
     let (title, kw_args) = take_atom_args 1 lexbuf in
     let (kind, title_opt, kw_args) = normalize_env (kind, title, kw_args) in
     let h_b = mk_heading (x, title_opt, kw_args) in
(*     let _ = d_printf "!atom lexer: matched group all: %s." h in  *)
     let _ = do_reset_env () in		
     let _ = do_begin_env () in		
     let (lopt, body, capopt, items, h_e) = take_env lexbuf in
   	 let (all_but_items, all) = mk_atom_str (h_b, body, capopt, items, h_e) in
     ATOM(kind, None, kw_args, lopt, body, capopt, items, all)
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
     ATOM(kind, None, [], lopt, body,  capopt, items, all)
       
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
       let _ = d_printf "!atom lexer: skip env matched = %s" v in
       let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
       (lopt, v ^ rest, capopt, items, h_e)          
      }   

  | p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let body = skip_inline kind lexbuf in
     let s = x ^ arg ^ c_sq ^ body in
     let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
       (lopt, s ^ rest, capopt, items, h_e)
    }

  | p_com_skip as x 
		{
     let body = skip_inline kind lexbuf in
     let s = x ^ body in
     let (lopt, rest, capopt, items, h_e) = take_env lexbuf in
       (lopt, s ^ rest, capopt, items, h_e)
    }

	| (p_primary_item_points p_ws_hard) as x
			{
		   (* This case is not allowed occur within nested env's.         
        * It should also be at the tail of an environment.
        * TODO: check for these and return an error if not satisfied.
        *) 
        let _ = d_printf "* atom lexer: begin primary items kind = %s tag = %s.\n" kind (str_of_str_opt tag) in
        let (body, lopt, items, h_e) = take_list lexbuf in
				let items = (kind, tag, point_val, lopt, body)::items in 
          (* Drop items from body *)
 	        (None, "", None, items, h_e)
      }

	| (p_primary_item p_ws_hard) as x
			{
		   (* This case is not allowed occur within nested env's.         
        * It should also be at the tail of an environment.
        * TODO: check for these and return an error if not satisfied.
        *) 
        let _ = d_printf "* atom lexer: begin primary items kind = %s.\n" kind in
        let (body, lopt, items, h_e) = take_list lexbuf in
				let items = (kind, None, None, lopt, body)::items in 
          (* Drop items from body *)
 	        (None, "", None, items, h_e)
      }

  | (p_item_and_points p_ws_hard) as x 
			{
		   (* This case is not allowed occur at the top level.
        * Raise Error.
        *) 
        let _ = d_printf "* atom lexer: begin items kind = %s.\n" kind in
        let (body, lopt, items, h_e) = take_list lexbuf in
 				let err = sprintf "Syntax Error: Encountered a \"%s\" solution prompt without a preceeding question prompt.\nContext:\n%s" kind (x ^ body) in
				let _ = printf "%s\n" err in
				raise (Constants.Syntax_Error err)							
      }

	| (p_item p_ws_hard) as x
			{
		   (* This case is not allowed occur at the top level.
        * Raise Error.
        *) 
        let _ = d_printf "* atom lexer: begin items kind = %s.\n" kind in
        let (body, lopt, items, h_e) = take_list lexbuf in
 				let err = sprintf "Syntax Error: Encountered a \"%s\" solution prompt without a preceeding question prompt.\nContext:\n%s" kind (x ^ body) in
				let _ = printf "%s\n" err in
				raise (Constants.Syntax_Error err)							
      }

	| (p_item_and_weight p_ws_hard) as x
			{
		   (* This case is not allowed occur at the top level.
        * Raise Error.
        *) 
        let _ = d_printf "* atom lexer: begin items kind = %s.\n" kind in
        let (body, lopt, items, h_e) = take_list lexbuf in
 				let err = sprintf "Syntax Error: Encountered a \"%s\" cookie without a preceeding prompt.\nContext:\n%s" kind (x ^ body) in
				let _ = printf "%s\n" err in
				raise (Constants.Syntax_Error err)							
      }

	| (p_item_weighted p_ws_hard) as x
			{
		   (* This case is not allowed occur at the top level.
        * Raise Error.
        *) 
        let _ = d_printf "* atom lexer: begin items kind = %s.\n" kind in
        let (body, lopt, items, h_e) = take_list lexbuf in
 				let err = sprintf "Syntax Error: Encountered a \"%s\" cookie without a preceeding prompt.\nContext:\n%s" kind (x ^ body) in
				let _ = printf "%s\n" err in
				raise (Constants.Syntax_Error err)							
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
     let (body, c_c) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
     let capopt = Some body in
     let all = x ^ body ^ c_c in
     let _ = d_printf "!atom lexer matched caption %s." all  in
		 let (lopt, y, capopt_, items, h_e) = take_env lexbuf in
     (* Drop capopt_, it would be another caption. *)
     (* Do not include caption in the body. *)
      (lopt, y, capopt, items, h_e)
    }

  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning a LaTeX environment.  An environment was not ended perhaps?" in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
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
  | eof 
      {
    	 let err = sprintf "Syntax Error: File ended while searching for: \\end{%s}." stop_kind in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _  as x
      { let _ = d_printf "!atom lexer: skip_env, stop = %s: %c \n" stop_kind x in
		    let (y, h_e) = skip_env stop_kind lexbuf in
        ((str_of_char x) ^ y, h_e)
      }

and skip_inline kind = 		
  (* Skip inline command, e.g. \lstinline<delimiter> ... <delimeter> 
   * Note: kind unused.
   *)
  parse
  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning a skip-command such as a \\verb or \\lstinline." in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _ as x
    { let x = str_of_char x in
(*  		let _ = d_printf "skip_inline kind = %s delimiter %s\n" kind x in *)
      let (rest, c) = skip_arg 1 x x lexbuf in
      let all =  x ^ rest ^ c in
(*			let _ = d_printf "skip_inline all = %s\n"  all in *)
        all
    } 

and take_arg depth delimiter_open delimiter_close = 
  parse
  | p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let h = x ^ arg ^ c_sq in
     let i = skip_inline kind lexbuf in
     let (rest, h_e) = take_arg depth delimiter_open delimiter_close lexbuf in
		 (h ^ i ^ rest, h_e)
    }

  | eof 
      {
    	 let err = sprintf "Syntax Error: File ended while searching for: %s." delimiter_close in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
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

and take_atom_args depth = 
  parse 
  | ((p_c_sq p_ws as x) (p_o_sq p_ws (p_keyword as kw) p_ws '=' p_ws) as all)
    {
     let depth = depth - 1 in
       if depth = 0 then
         let (a, l) = take_kw_args 1 lexbuf in
           ("", (kw, a)::l)
       else
         let (arg, l) = take_atom_args depth lexbuf in 
           (all ^ arg, l)
     }   
  | p_o_sq as x
    {
     let (arg, l) = take_atom_args (depth + 1) lexbuf in 
       (x ^ arg, l)
    }

  | (p_c_sq p_hs) as x
    {
     let depth = depth - 1 in
       if depth = 0 then
           ("", [])
       else
         let (arg, l) = take_atom_args depth lexbuf in 
           (x ^ arg, l)
    }
  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning the arguments for an atom." in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _ as x
    {
     let (arg, l) = take_atom_args depth lexbuf in 
       ((str_of_char x) ^ arg, l)
    }

and take_kw_args depth = 
  parse 
  | (p_c_sq p_ws as x)
    {
(*     let _ = d_printf "atom_lexer: take_kw_args: %s\n" x in *)
     let depth = depth - 1 in
       if depth = 0 then
           ("", [])
       else
         let (arg, l) = take_kw_args depth lexbuf in 
           (x ^ arg, l)
    }
  | ';' p_ws (p_keyword as kw) p_ws '=' p_ws 
 	  {
(*     let _ = d_printf "atom_lexer: take_kw_args: keyword = %s\n" kw in *)
      let (arg, l) = take_kw_args depth lexbuf in 
        ("", (kw, arg)::l)
    }
  | p_o_sq as x
    {
(*     let _ = d_printf "atom_lexer: take_kw_args: %s\n" x in *)
     let depth = depth + 1 in
     let (arg, l) = take_kw_args depth lexbuf in 
       (x ^ arg, l)
    }
  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning for keyword arguments of an atom." in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _ as x
    {
(*     let _ = d_printf "atom_lexer: take_kw_args: %s\n" (str_of_char x) in *)
     let (arg, l) = take_kw_args depth lexbuf in 
       ((str_of_char x) ^ arg, l)
    }

and take_list =
	 parse
	 | (p_item_and_points as x) p_ws_hard 
	 { let (body, lopt, items, h_e) = take_list lexbuf in
     (* If this is a fillin prompt, then rewrite body *) 
     let prompt_items =  rewrite_prompt kind tag point_val lopt  body in
     let _ = d_printf "* atom_lexer: item kind %s tag = %s points = %s body = %s\n" kind (str_of_str_opt tag) (str_of_str_opt point_val) body in
	   let items = prompt_items @ items in     
	   ("", None, items, h_e)	 	 
	 }
	 | (p_item as x) p_ws_hard
	 { let (body, lopt, items, h_e) = take_list lexbuf in
     let prompt_items =  rewrite_prompt kind None None lopt body in
     let _ = d_printf "* atom lexer: item kind %s body = %s\n" kind body in
	   let items = prompt_items @ items in     
	     ("", None, items, h_e)	 	 
	 }
	 | (p_item_and_weight as x) p_ws_hard 
	 { let (body, lopt, items, h_e) = take_list lexbuf in
     let cookie = (kind, tag, weight, lopt, body) in
     let _ = d_printf "* atom_lexer: item kind %s tag = %s weight = %s body = %s\n" kind (str_of_str_opt tag) (str_of_str_opt weight) body in
	   let items =  cookie::items in     
	   ("", None, items, h_e)	 	 
	 }
	 | (p_item_weighted as x) p_ws_hard
	 { let (body, lopt, items, h_e) = take_list lexbuf in
     let cookie = (kind, None, None, lopt, body) in
     let _ = d_printf "* atom_lexer: item kind %s body = %s\n" kind body in
	   let items = cookie::items in     
	     ("", None, items, h_e)	 	 
	 }
 	 | (p_item as x)     (* Catch ill formed item *)
   {
  	let err = sprintf "Syntax Error: Encountered an ill-formed item, perhaps the points or the tag are not specified correctly?\n    Context: %s" x in 
       raise (Constants.Syntax_Error err)
	 }
   | p_label_and_name as x
   { 
	   let _ = d_printf "!atom lexer: take_list: matched label %s.\n" x in 
	   let all = label_pre ^ label_name ^ label_post in
     let (body, lopt, items, h_e) = take_list lexbuf in
     let _ = 
       match lopt with 
			 | None -> ()
			 | Some l -> 
			 	 let err = sprintf "Syntax Error: This prompt/cookie defines multiple labels: %s and %s" label_name l in 
  		   raise (Constants.Syntax_Error err)
		 in
	   let body =  all ^ body in
     (* Important: Drop inner label lopt *)
     (body, Some label_name, items, h_e)  
   }		

   | p_end_env as x
   { 
(*            let _ = d_printf "!atom lexer: end latex env: %s\n" x in *)
		 match get_current_atom () with
		 | None -> (printf "Fatal Error occured in atom_lexer.  No atom."; exit 1)
		 | Some atom ->
				 if kind = atom then 
					 ("", None, [], x)
				 else
					 let (body, lopt, items, h_e) = take_list lexbuf in
					 (x ^ body, lopt, items, h_e)  
   }      
   | eof  
      {
  		 match get_current_atom () with
	  	 | None -> 
			 	 let err = "Syntax Error: File ended unexpectedly while scanning the tail list of an (unknown) atom" in
         let _ = printf "%s\n" err in
  		   raise (Constants.Syntax_Error err)
		   | Some atom ->
			 	 let err = sprintf  "Syntax Error: File ended unexpectedly while scanning the tail list of an atom: %s." atom in
         let _ = printf "%s\n" err in
  		   raise (Constants.Syntax_Error err)
      } 

	 | _ as x 
	 { let (body, lopt, items, h_e) = take_list lexbuf in
	   let body =  (str_of_char x) ^ body in
	     (body, lopt, items, h_e)
	 }

and skip_arg depth delimiter_open delimiter_close = 
	  (* this is like take_arg but does not skip over comments *)
  parse
  | eof 
      {
    	 let err = sprintf "Syntax Error: File ended while searching for: %s." delimiter_close in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _ as x
    {
     let x = str_of_char x in
(*     let _ = d_printf "skip_arg x =  %s arg depth = %d\n" x (arg_depth ()) in  *)
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
(* This is the default lexer *)
let lexer: Lexing.lexbuf -> token = 
		initial

}
(** END TRAILER **)

