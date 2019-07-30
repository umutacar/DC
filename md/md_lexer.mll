(********************************************************************** 
 ** md/md_lexer.mll
 **********************************************************************)

(** BEGIN: HEADER **)
{
open Printf
open Utils

open Md_parser
open Md_syntax
(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args
*)
(*
let d_printf args = printf args
*)

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
	| KW_HEADING (x, _, _) -> "* lexer: token = heading: " ^ x
  | EOF -> "* lexer: token = EOF.";
  | _ ->  "Fatal Error: token match not found!!!"

let token_to_dbg_str tk =
	match tk with 
	| NEWLINE x -> x
	| HSPACE x ->  x
	| PAR_CHUNK (x, lopt) -> x
	| CHUNK (x, lopt) ->  x
	| KW_HEADING (kind, title, _) -> sprintf "\\%s{%s}" kind title 
  | EOF -> "EOF\n"
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
let p_o_curly = '{' p_ws
(* don't take newline with close *)
let p_c_curly = '}' p_hs
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											


let p_chapter = ("#" as kind) 

let p_section = ("##" as kind)
let p_subsection = ("###" as kind)
let p_subsubsection = ("###" as kind)
let p_paragraph = ("####" as kind) p_ws												

(* Latex environment: alphabethical chars plus an optional star *)
let p_codeblock = "```"['`']*

let p_begin_env = (p_codeblock as kind) | '<' p_ws (p_alpha+ as kind) p_ws '>'
let p_end_env = (p_codeblock as kind) | "</" p_ws (p_alpha+ as kind) p_ws '>'
(* end: environments *)


(* Segments *)
let p_segment = 
	p_chapter |
  p_section |
  p_subsection |
  p_subsubsection |
  p_paragraph  

(* Headings *)
let p_heading = p_segment

(** END PATTERNS *)			

rule initial = parse

| (p_heading as x) p_hs
    {
     let title = take_line lexbuf in
(*     let h = x ^ o_c ^ arg ^ c_c in *)
     let _ = d_printf "!lexer matched segment title =  %s" title in 
     let kind = il_kind_of_segment kind in
       KW_HEADING(kind, title, None)
    }		

| (p_begin_env as x)
    {
     let _ = d_printf "!lexer matched begin env %s." kind in 
     let (body, y) = skip_env kind lexbuf in
     let env = x ^ body ^ y in
     let _ = d_printf "!lexer matched env %s." env in 

       CHUNK(env, None)
}

| p_sigchar as x
		{
(*     d_printf "!%s" (str_of_char x); *)
     let _ =  set_line_nonempty () in
     CHUNK(str_of_char x, None)
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

and take_line  = 		
  parse
  | p_newline as x 
    { ""
    } 
  | _ as x
    { let x = str_of_char x in
(*  		let _ = d_printf "skip_inline kind = %s delimiter %s\n" kind x in *)
      let rest = take_line lexbuf in
      let all =  x ^ rest  in
(*			let _ = d_printf "skip_inline all = %s\n"  all in *)
        all
    } 


and skip_inline kind = 		
  (* Skip inline command, e.g. \lstinline<delimiter> ... <delimeter> *)
  parse
  | _ as x
    { let x = str_of_char x in
(*  		let _ = d_printf "skip_inline kind = %s delimiter %s\n" kind x in *)
      let _ = inc_arg_depth () in
      let (rest, c) = skip_arg x x lexbuf in
      let all =  x ^ rest ^ c in
(*			let _ = d_printf "skip_inline all = %s\n"  all in *)
        all
    } 

and skip_env stop_kind =
  (* Assumes non-nested environments *)
  parse
  | p_end_env as x
      { 
       let _ = d_printf "!lexer: end environment kind= %s stop kind = %s\n" kind stop_kind in 
          if kind = stop_kind then
            let _ = d_printf "!lexer: exiting environment stop kind = %s\n" stop_kind in 
						("", x)
          else 
            let _ = d_printf "!lexer: not exiting environment kind = %s\n" kind  in 
            let (y, h_e) = skip_env stop_kind lexbuf in
						(x ^ y, h_e)
      }
  | _  as x
      { let (y, h_e) = skip_env stop_kind lexbuf in
        ((str_of_char x) ^ y, h_e)
      }

and skip_arg delimiter_open delimiter_close = 
	  (* this is like take_arg but does not skip over comments *)
  parse
  | _ as x
    {
     let x = str_of_char x in
(*     let _ = d_printf "skip_arg x =  %s arg depth = %d\n" x (arg_depth ()) in  *)
     (* Tricky: check close first so that you can handle 
        a single delimeter used for both open and close,
        as in lstinline.
      *)
		 if x = delimiter_close then
			 let _ = dec_arg_depth () in
       if arg_depth () = 0 then
(*				 let _ = d_printf "exit\n" in *)
         ("", x)
       else
         let (arg, c_c) = skip_arg delimiter_open delimiter_close lexbuf in 
         (x ^ arg, c_c)					 
		 else if x = delimiter_open then
			 let _ = inc_arg_depth () in
			 let (arg, c_c) = skip_arg delimiter_open delimiter_close lexbuf in 
			 (x ^ arg, c_c)
		 else
			 let (rest, c_c) = skip_arg delimiter_open delimiter_close lexbuf in
       (x ^ rest, c_c)
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
				let _ = d_printf "%s" (token_to_dbg_str tk_to_return) in
				(prev_token := Some tk_to_return;
(*				 d_printf "returning token: %s\n" (token_to_str tk_to_return);  *)
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
(*			let _ = d_printf "!lexer: handling token: %s\n" (token_to_str tk) in *)
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
(*        d_printf "!!START PARAGRAPH!!\n" *)
        ()
      in 
        return_token tk

}
(** END TRAILER **)

