(********************************************************************** 
 ** md/md_lexer.mll
 **********************************************************************)

(* BEGIN: HEADER *)
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

(* Lexer State *)
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
let set_state s =  state := s
let get_state = fun () -> !state 

(* Are we making progress horizontally or vertically? 
 * Vertically means that we are in the middle of a run of empty lines
 * possibly with horizontal spaces.
 *)
type t_space_state = 
	| Horizontal
	| Vertical

let space_state = ref Horizontal
let set_space_state t =  space_state := t
let get_space_state () = !space_state
let space_state_to_string () =
	match !space_state with  
	| Horizontal -> "Horizontal"
	| Vertical -> "Vertical"


(* Token Cache *)
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


}
(** END: HEADER **)

(** BEGIN: PATTERNS *)	

(* significant, non-space character *)
let p_sigchar = [^ ' ' '\t' '\n' '\r']

(* white space *)
let p_hspace = ' ' | '\t' | '\r' 
let p_newline = '\n' | ('\r' '\n')
let p_tab = '\t'	
let p_hs = [' ' '\t']*	
let p_ws = [' ' '\t' '\n' '\r']*	


let p_comma = ','
let p_backslash = '\\'
let p_special = '\\' | '`'
let p_escape = p_backslash p_special

let p_digit = ['0'-'9']
let p_integer = ['0'-'9']+
let p_frac = '.' p_digit*
let p_exp = ['e' 'E'] ['-' '+']? p_digit+
let p_float = p_digit* p_frac? p_exp?

let p_alpha = ['a'-'z' 'A'-'Z']
let p_separator = [':' '.' '-' '_' '/']


(* Delimiters are used for span/inline environment that are not nested.
 * They can be multiple characters.  In that case, extend take_arg
 * to match them first.
 * TODO: code block should be treated like this.
 *)
let p_backtick = "`" 
let p_double_backtick = "``" 


(* Inline skip *)
let p_skip = 
	(p_backtick as delimeter) |	(p_double_backtick as delimeter)


(* BEGIN: environments *)
let p_codeblock = ("```"['`']* as kind)
let p_html_begin = '<' (p_alpha+ as kind) '>'
let p_html_end = "</" (p_alpha+ as kind) '>'

let p_begin_env = p_codeblock | p_html_begin
let p_end_env = p_codeblock | p_html_end



(* END: environments *)

(* Segments *)
let p_chapter = ("#" as kind) 
let p_section = ("##" as kind)
let p_subsection = ("###" as kind)
let p_subsubsection = ("###" as kind)
let p_paragraph = ("####" as kind) p_ws												

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
| p_escape as x 
  {
   CHUNK(x, None)
	}

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

| (p_skip as delimiter) 
		{
(*     let _ = d_printf "!lexer found: skip %s." (str_of_char x) in *)
     let (body, _) = take_arg 1 delimeter delimeter lexbuf in
     let all = delimeter ^ body ^ delimeter in
		   CHUNK(all, None)
    }

| p_sigchar as x
		{
(*     d_printf "!%s" (str_of_char x); *)
     CHUNK(str_of_char x, None)
    }

| p_newline as x
		{
(*	   d_printf "!lexer found: newline: %s." x; *)
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


and take_arg depth delimiter_open delimiter_close = 
 (* Take argument delimited by delimiter open and close.
  * Allow nesting. (TODO: probably unnecessary?) 
  *)
  parse
  | p_double_backtick as x
    {
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
(*   			 d_printf "returning token: %s\n" (token_to_str tk_to_return); *)
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

