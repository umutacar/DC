(** BEGIN: HEADER **)
{
open Printf
open Utils
open Tex_parser

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args
*)

let kw_comment = "comment"
let kw_lstlisting = "lstlisting"
let kw_verbatim = "verbatim"

(*let d_printf args = printf args*)
type t_lexer_state = 
	| Busy
	| Idle
 
(* State is either busy or idle.
 * Idle means that we are not in the middle of a paragraph.
 * Busy means htat we are in the middle of handling a paragraph.
 *)
let state_to_string st = 
	match st with 
	|  Busy -> "Busy"
	|  Idle -> "Idle"

let state = ref Idle
let set_state s = 
  state := s
let get_state = fun () -> !state 

(* Are we making progress horizontally or vertically? 
 * Vertically means that we are in the middle of a run of empty lines
 * possibly with horizontal spaces.
 *)
type t_space_state = 
	| Horizontal
	| Vertical

let space_state = ref Horizontal
let set_space_state t = 
  space_state := t
let get_space_state () = 
	!space_state
let space_state_to_string () =
	match !space_state with  
	| Horizontal -> "Horizontal"
	| Vertical -> "Vertical"



(* Indicates the depth at which the lexer is operating at
   0 = surface level
   1 = inside of an env
 *)
let lexer_depth = ref 0
let get_lexer_depth () =
	!lexer_depth
let inc_lexer_depth () = 
   lexer_depth := !lexer_depth + 1
let dec_lexer_depth () = 
  (assert (!lexer_depth > 0);
   lexer_depth := !lexer_depth - 1
		 )

		
(* Indicates that the current line is empty *)
let lexer_line_status = ref true
let set_line_empty () = 
	if get_lexer_depth () = 0 then
		lexer_line_status := true

let set_line_nonempty () = 
	if get_lexer_depth () = 0 then
		lexer_line_status := false
let line_is_empty () = 
	if get_lexer_depth () = 0 then
		!lexer_line_status
	else
		(* Always return true, empty lines are harmless *)
		true

type t_cache = token list
let cache = ref [ ]
let cache_is_empty () =
  match !cache with 
	| [ ] -> true
	| _ -> false
let cache_insert t =
  cache := !cache @ [ t ]
let cache_remove () =
  match !cache with 
	| [ ] -> None
	| h::t -> 
			(cache := t;
			 Some h)

(* Some Utilities *)
let start = Lexing.lexeme_start

let token_to_str tk =
	match tk with 
	| NEWLINE x -> "* lexer: token = newline."
	| HSPACE x ->  "* lexer: token = hspace."
	| PAR_CHUNK (x, lopt) -> "* lexer: token = par chunk " ^ x
	| CHUNK (x, lopt) ->  "* lexer: token = chunk " ^ x 
	| KW_BEGIN_GROUP (x, _, _) -> "* lexer: token = begin group " ^ x
	| KW_END_GROUP (x, _) -> "* lexer: token = end group " ^ x
	| KW_FOLD (x) -> "* lexer: token = fold: " ^ x
	| KW_HEADING (x, _, _) -> "* lexer: token = heading: " ^ x
  | EOF -> "* lexer: token = EOF.";
  | _ ->  "Fatal Error: token match not found!!!"

(**********************************************************************
 ** BEGIN: latex env machinery 
 **********************************************************************)

let env_pos = ref 0  
let env_depth = ref 0  

let do_begin_env () =
  inc_lexer_depth ()

let do_end_env () =
  let _ = dec_lexer_depth () in
    get_lexer_depth () = 0

(**********************************************************************
 ** END: latex env machinery 
 **********************************************************************)

(**********************************************************************
 ** BEGIN: curly bracked depth machinery
 **********************************************************************)
let arg_depth = ref 0  

let inc_arg_depth () =
  arg_depth := !arg_depth + 1

let dec_arg_depth () =
  arg_depth := !arg_depth - 1

let arg_depth () =
  !arg_depth
(**********************************************************************
 ** END: curly bracked depth machinery
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

let p_com_lstinline = '\\' "lstinline"

let p_point_val = (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							


let p_kw_chapter = ("chapter" as kind) ['*']? 
let p_chapter = p_kw_chapter
let p_chapter_with_points = p_kw_chapter p_ws (p_point_val as points)

let p_kw_section = ("section" as kind) ['*']? 
let p_section = p_kw_section p_ws
let p_section_with_points = p_kw_section p_ws (p_point_val as points)

let p_kw_subsection = ("subsection" as kind) ['*']? 
let p_subsection = p_kw_subsection p_ws 
let p_subsection_with_points = p_kw_subsection p_ws (p_point_val as points)

let p_kw_subsubsection = ("subsubsection" as kind) ['*']? 
let p_subsubsection = p_kw_subsubsection p_ws 
let p_subsubsection_with_points = p_kw_subsubsection p_ws (p_point_val as points)

let p_paragraph = ("paragraph" as kind) p_ws												
let p_paragraph_with_points = ("paragraph" as kind) p_ws (p_point_val as points)

let p_cluster = "cluster"
let p_flex = "flex"
let p_problem_cluster = "mproblem"

let p_questions = "ques"

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


let p_env_lstlisting = "lstlisting"
let p_env_comment = "comment"
let p_env_verbatim = "verbatim"

let p_end_env_generic = "\\end" p_ws "{" p_ws (p_env as kind) p_ws "}"

let p_begin_env_comment = p_com_begin p_ws p_o_curly p_ws p_comment p_ws p_c_curly
let p_end_env_comment = p_com_end p_ws p_o_curly p_ws p_comment p_ws p_c_curly

let p_begin_env_verbatim = p_com_begin p_ws p_o_curly p_ws p_env_verbatim p_ws p_c_curly
let p_end_env_verbatim = p_com_end p_ws p_o_curly p_ws p_env_verbatim p_ws p_c_curly
(* end: verbatim *)


(* Segments *)
let p_segment = 
	p_chapter |
  p_section |
  p_subsection |
  p_subsubsection |
  p_paragraph  

let p_segment_with_points =
	p_chapter_with_points | 
  p_section_with_points | 
  p_subsection_with_points | 
  p_subsubsection_with_points | 
  p_paragraph_with_points

(* Headings *)
let p_heading = '\\' p_segment
let p_heading_with_points = '\\' p_segment_with_points

let p_group = ((p_cluster as kind) p_ws as kindws) |
              ((p_flex as kind) p_ws as kindws) |
              ((p_problem_cluster as kind) p_ws as kindws) 

(* Groups *)
let p_begin_group = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) 
let p_begin_group_with_points = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_group = (p_com_end p_ws as e) (p_o_curly as o) p_group (p_c_curly as c) 



let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env) p_ws (p_c_curly) 
let p_begin_env_with_points = (p_com_begin p_ws) (p_o_curly) (p_env) (p_c_curly) p_ws (p_point_val as points)
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env) (p_c_curly) 

let p_begin_env_lstlisting = (p_com_begin p_ws) (p_o_curly) (p_env_lstlisting) p_ws (p_c_curly) 
let p_end_env_lstlisting = (p_com_end p_ws) (p_o_curly) (p_env_lstlisting) (p_c_curly) 

let p_list = 
	(p_questions as kind) p_ws as kindws 

let p_begin_list = (p_com_begin p_ws as b) (p_o_curly as o) p_list (p_c_curly as c) 
let p_end_list = (p_com_end p_ws as e) (p_o_curly as o) p_list (p_c_curly as c) 


(** END PATTERNS *)			

rule initial = parse
| (p_heading as x) (p_o_curly as o_c)
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
     let _ = inc_arg_depth () in
     let (arg, c_c) = take_arg lexbuf in
     let h = x ^ o_c ^ arg ^ c_c in
(*     let _ = d_printf "!lexer matched segment all: %s." h in *)
     let _ =  set_line_nonempty () in
       KW_HEADING(kind, arg, None)
    }		

| (p_heading_with_points as x) (p_o_curly as o_c)
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
     let _ = inc_arg_depth () in
     let (arg, c_c) = take_arg lexbuf in
(*     let h = x ^ o_c ^ arg ^ c_c in *)
(*     let _ = d_printf "!lexer matched segment all: %s." h in *)
     let _ =  set_line_nonempty () in
       KW_HEADING(kind, arg, Some point_val)
    }		

| (p_begin_group as x) (p_o_sq as a)
    {
     let _ = d_printf "!lexer matched begin group %s." kind in 
     let _ = inc_arg_depth () in
     let (arg, c_sq) = take_opt_arg lexbuf in
     let h = x ^ a ^ arg ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
     let _ =  set_line_nonempty () in
       KW_BEGIN_GROUP(kind, arg, None)
    }		

| p_begin_group as x
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ =  set_line_nonempty () in
       KW_BEGIN_GROUP(kind, x, None)
    }		

| p_com_fold as x
    {
(*     let _ = d_printf "!lexer matched fold %s." x in *)
     let _ =  set_line_nonempty () in
       KW_FOLD(x)
    }		

| p_end_group as x
    {
     let _ = d_printf "!lexer matched end group %s." kind in
     let _ =  set_line_nonempty () in
       KW_END_GROUP(kind, x)
    }		

| (p_begin_env_comment as x)
    {
     let _ = printf "!lexer matched begin comment %s." x in 
     let (rest, h_e) = skip_env kw_comment lexbuf in
   	 let all = x ^ rest ^ h_e in
     let _ = printf "!lexer matched comment \n %s." all in 
		 (* Drop comments *)
     initial lexbuf
}

| (p_begin_env_lstlisting as x)
    {
     let _ = printf "!lexer matched begin lstlisting %s." x in 
     let (rest, h_e) = take_env_lstlisting lexbuf in
   	 let all = x ^ rest ^ h_e in
     let _ = printf "!lexer matched begin lstlisting\n %s." all in 
     let _ =  set_line_nonempty () in
     CHUNK(all, None)
}

| (p_begin_env as x)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ = do_begin_env () in		
     let (rest, h_e) = take_env lexbuf in
   	 let all = x ^ rest ^ h_e in
     let _ =  set_line_nonempty () in
            CHUNK(all, None)
}

| p_label_and_name as x
 		{ 
(*	    let _ = d_printf "!lexer matched %s." x in *)
      let _ =  set_line_nonempty () in
			let all = label_pre ^ label_name ^ label_post in
(*			KW_LABEL_AND_NAME(label_pre ^ label_name ^ label_post, label_name) *)
			CHUNK (all, Some label_name)
		}		

| p_com_lstinline as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let _ =  set_line_nonempty () in
     let body = take_lstinline lexbuf in
     let all = x ^ body in
		   CHUNK(all, None)
    }

| p_sigchar as x
		{
(*     d_printf "!%s" (str_of_char x); *)
     let _ =  set_line_nonempty () in
     CHUNK(str_of_char x, None)
    }

| p_percent_esc as x 
		{
(*     d_printf "!lexer found: espaced percent char: %s." x; *)
     let _ =  set_line_nonempty () in
     CHUNK(x, None)
    }

| p_percent as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let is_line_empty = line_is_empty () in
     let rest = take_comment lexbuf in
     let _ = d_printf "line empty = %b\n" is_line_empty in
     (* A comment ends with a newline so set the line empty *)
     let _ =  set_line_empty () in
     let comment = (str_of_char x) ^ rest in
(*     let _ = d_printf "!lexer found: comment: %s." result in *)
		 if is_line_empty then
      (* Drop linens consisting of comments only *)
       initial lexbuf 

		 else
      (* Drop comments but finish the line, which is not empty *)
			 NEWLINE "\n"
    }

| p_newline as x
		{
(*	   d_printf "!lexer found: newline: %s." x; *)
     let _ =  set_line_empty () in
       NEWLINE(x)
    }

| p_hspace as x
		{
(*     d_printf "!lexer found: horizontal space: %s." (str_of_char x); *)
     HSPACE(str_of_char x)
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
and take_lstinline = 		
  parse
  | _ as x
    { let _ = printf "take_lstinline: delimiter %c\n" x in 
      let rest = take_lstinline_tail x lexbuf in
      let all = (str_of_char x) ^ rest in
			let _ = printf "take_lstinline: all = %s\n"  all in
        all
    } 
and take_lstinline_tail delimiter = 		
  parse
  | _ as x
    {
(*     let _ = d_printf "take_lstinline_tail: %s" (str_of_char x) in *) 
		 if x = delimiter then
			 str_of_char x
		 else
			 let rest = take_lstinline_tail delimiter lexbuf in 
       (str_of_char x) ^ rest
    }
and take_env =
  parse
  | p_begin_env_verbatim as x
      { 
(*          let _ = d_printf "!lexer: entering verbatim\n" in *)
          let y = verbatim lexbuf in
          let _ = d_printf "!lexer: verbatim matched = %s" (x ^ y) in
          let (rest, h_e) = take_env lexbuf in
            (x ^ y ^ rest, h_e)          
      }   
  | p_com_lstinline as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let body = take_lstinline lexbuf in
     let (rest, h_e) = take_env lexbuf in
     (x ^ body ^ rest, h_e)          
    }

	| (p_begin_env_lstlisting as x)
    {
     let _ = printf "!lexer matched begin lstlisting %s." x in 
     let (body, h_e) = take_env_lstlisting lexbuf in
   	 let lst = x ^ body ^ h_e in
     let _ = printf "!lexer matched begin lstlisting\n %s." lst in 
     let (rest, h_e) = take_env lexbuf in
     (lst ^ rest, h_e)
    }
  | p_begin_env as x
        {
(*            let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
            let _ = do_begin_env () in
            let (rest, h_e) = take_env lexbuf in
                (x ^ rest, h_e)              
        }

  | p_end_env as x
        { 
(*            let _ = d_printf "!lexer: end latex env: %s\n" x in *)
            let do_exit = do_end_env () in
                if do_exit then
(*                    let _ = d_printf "!lexer: exiting latex env\n" in *)
                        ( "", x)
                else
                    let (rest, h_e) = take_env lexbuf in
                      (x ^ rest, h_e)  
        }      

  (* Important because otherwise lexer will think that it is comment *)
  | p_percent_esc as x 
		{
(*     let _ = d_printf "!lexer found: espaced percent char: %s." x in *)
     let (rest, h_e) = take_env lexbuf in
          (x ^ rest, h_e)
    }

  | p_percent as x   (* comments *)
   	{ 
     let y = take_comment lexbuf in
     let (rest, h_e) = take_env lexbuf in 
     (* Drop comment, including newline at the end of the comment.   *)
     (rest, h_e)
     } 

  | _  as x
        { let (rest, h_e) = take_env lexbuf in
            ((str_of_char x) ^ rest, h_e)
        }
and verbatim =
  parse
  | p_end_env_verbatim as x
        { 
            let _ = d_printf "!lexer: exiting verbatim\n" in
                x
        }
  | _  as x
        { let y = verbatim lexbuf in
            (str_of_char x) ^ y
        }

and skip_env stop_kind =
  parse
  | p_end_env_generic as x
      { let _ = d_printf "!lexer: exiting environment\n" in
          if kind = stop_kind then
						("", x)
          else 
            let (y, h_e) = skip_env stop_kind lexbuf in
						(x ^ y, h_e)
      }
  | _  as x
      { let (y, h_e) = take_env_lstlisting lexbuf in
        ((str_of_char x) ^ y, h_e)
      }

and take_env_lstlisting =
  parse
  | p_end_env_lstlisting as x
      { let _ = d_printf "!lexer: exiting lstlisting\n" in
        ("", x)
      }
  | _  as x
      { let (y, h_e) = take_env_lstlisting lexbuf in
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


(** BEGIN TRAILER **)
{
(* This is the default lexer *)
let lexer: Lexing.lexbuf -> token = 
		initial

(* This is the spiced up lexer that modifies the token stream in
 * several ways.
 *
 * First, it maintains  state and implements a state transition 
 * system to detect the beginning of paragraphs.
 *
 * It also emits an additional newline token to "force end" a paragraph
 * when a heading or EOF is encountered but a paragraph is not 
 * completed.  When an extra token is emitted, the current token
 * is cached and emitted at the next request.
 *
 * The sate machine consists of two piece of state.  
 * One tracks whether we are "Busy" in the middle of a paragraph
 * or "Idle".
 * The "space state" tracks whether we have are in the middle of a 
 * "Vertical" space run, or "Horizontal" space run.
 * Here, "Vertical" means that we have seen at least two newlines
 * and a bunch of horizontal spaces, no non-space characters.
 * Horizontal means that we are processing significant non-space 
 * characters along with horizontal white space.
 * 
 * The state machine transitions from Busy to Idle when we see 
 * a newline and we are in "Vertical" state.
 *)

let lexer: Lexing.lexbuf -> token =
  let prev_token = ref None in
    fun lexbuf ->
(*  		let _ = d_printf "!lexer: state = %s\n" (state_to_string !state) in *)
      let old_state = get_state () in

			let next_token = ref None in
			let set_next_token t = next_token := t in

			let return_token tk = 
				let tk_to_return = 
					match !next_token with 
					| None -> tk
					| Some ntk -> ntk
				in
				(prev_token := Some tk_to_return;
				 d_printf "returning token: %s\n" (token_to_str tk_to_return); 
				 tk_to_return)
			in
			let is_token_from_cache = ref false in
      let handle_keyword tk = 
				match !state with 
   			| Idle -> 
						let _ = set_space_state Horizontal in
						set_state Idle
				| Busy -> 
						(* Force end of paragraph *)
						let _ = cache_insert (NEWLINE "\n") in
						let _ = cache_insert tk in
						let _ = set_next_token (Some (NEWLINE "\n")) in
            (* Because next token is newline, we will return a newline *)
						let _ = set_space_state Vertical in 
						set_state Busy

			in
      (* Take token from cache, 
       * if cache is empty,  then take from the lexer 
       *)
  		let tk = 
				match cache_remove () with 
				| None -> (is_token_from_cache := false; lexer lexbuf)
				| Some t -> (is_token_from_cache := true; t) in
			let _ = d_printf "!lexer: handling token: %s\n" (token_to_str tk) in 
			let _ =
				match tk with 
				| NEWLINE x -> 
(*						let _ = d_printf "\n **token = newline! \n" in *)
(*						let _ = d_printf " **space_state = %s! \n" (space_state_to_string ()) in *)
						begin
            match get_space_state () with 
						| Horizontal ->
								let _ = set_space_state Vertical in
								  ()
						| Vertical ->
								let _ = set_state Idle in
                ()
						end
				| HSPACE x ->  
(*						let _ = d_printf "** token = hspace %s \n" x in *)
						(* Does not change space state! *)
						()
				| CHUNK x -> 
						let (c, l) =  x in
(*						let _ = d_printf "** token = sigchar %s \n" c in *)
(*						let _ = d_printf "%s" x in *)
						let _ = set_space_state Horizontal in
						begin
						match !state with 
   					| Idle -> 						
								let _ = set_state Busy in
								next_token := Some (PAR_CHUNK x)
						| Busy -> 
								let _ = set_state Busy in
								()
						end

				| KW_BEGIN_GROUP x ->
(*  					let _ = d_printf "** token = begin group \n"  in *)
            handle_keyword tk

				| KW_END_GROUP x ->
(*  					let _ = d_printf "** token = end group \n"  in *)
            handle_keyword tk

				| KW_FOLD x ->
(*  					let _ = d_printf "** token = fold \n"  in *)
            handle_keyword tk

				| KW_HEADING x ->
(*  					let _ = d_printf "** token = heading \n"  in *)
            handle_keyword tk

        | EOF -> 
(*						let _ = d_printf "token = EOF \n" in *)
						handle_keyword tk

        | _ -> printf "Fatal Error: token match not found!!!\n"
			in  
      let _ = if old_state = Idle && (get_state () = Busy) then
        d_printf "!!START PARAGRAPH!!\n"
      in 
        return_token tk

}
(** END TRAILER **)

