(** BEGIN: HEADER **)
{
open Printf
open Atom_parser
open Utils


type t_lexer_state = 
	| Busy
	| Idle
 

let state_to_string st = 
	match st with 
	|  Busy -> "Busy"
	|  Idle -> "Idle"

let state = ref Idle
let set_state s = 
  state := s
let get_state = fun () -> !state 

type t_space_trace = 
	| No_space
	| Hor_space
	| Ver_space

let trace = ref No_space
let set_trace t = 
  trace := t
let get_trace () = 
	!trace
let trace_to_string () =
	match !trace with  
	| No_space -> "No space"
	| Hor_space -> "Hor space"
	| Ver_space -> "Ver space"


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

type t_cache = Atom_parser.token list
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
let char_to_str x = String.make 1 x
let pvalopt_to_str x = 
  match x with 
  | None -> "None"
  | Some x -> "Some" ^ x

let token_to_str tk =
	match tk with 
	| NEWLINE x -> "token = newline."
	| HSPACE x ->  "token = hspace."
	| ENV (topt, lopt, x, all) ->  "token = env = " ^ all
	| PAR_ENV (topt, lopt, x, all) ->  "token = env = " ^ all
	| PAR_SIGCHAR x -> "token = par sigchar: " ^ x
	| PAR_LABEL_AND_NAME (x, y) -> "token = par label %s " ^ x
	| SIGCHAR x ->  "token = sigchar: " ^ x 
	| KW_LABEL_AND_NAME _ -> "token = label" 
	| KW_BEGIN_GROUP (x, _, _) -> "token = begin group " ^ x
	| KW_END_GROUP (x, _) -> "token = end group " ^ x
	| KW_FOLD (x) -> "token = fold: " ^ x
	| KW_HEADING (x, _, _) -> "token = heading: " ^ x
  | EOF -> "token = EOF.";
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
let p_com_choice = '\\' "choice"
let p_com_correct_choice = '\\' "choice*"

let p_com_fold = '\\' "fold"
let p_com_explain = '\\' "explain"
let p_com_hint = '\\' "help"
let p_com_rubric = '\\' "rubric"
let p_com_refsol = '\\' "sol"

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							

(* begin: verbatim 
 * we will treat verbatim as a "box"
 *)
let p_verbatim = "verbatim"
let p_begin_verbatim = p_com_begin p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
let p_end_verbatim = p_com_end p_ws p_o_curly p_ws p_verbatim p_ws p_c_curly
(* end: verbatim *)

let p_chapter = ("chapter" as kind)
let p_chapter_with_points = ("chapter" as kind) p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)
let p_section = ("section" as kind) p_ws
let p_section_with_points = ("section" as kind) p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_subsection = ("subsection" as kind) p_ws 
let p_subsection_with_points = ("subsection" as kind) p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_subsubsection = ("subsubsection" as kind) p_ws 
let p_subsubsection_with_points = ("subsubsection" as kind) p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_paragraph = ("paragraph" as kind) p_ws												
let p_paragraph_with_points = ("paragraph" as kind) p_ws (p_o_sq as o_sq) (p_integer as point_val) p_ws (p_c_sq as c_sq)

let p_cluster = "cluster"
let p_flex = "flex"
let p_problem_cluster = "mproblem"


let p_diderot_atom = "diderot" ['a'-'z''A'-'Z']*	
let p_algorithm = "algorithm"
let p_assumption = "assumption"
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
let p_reminder = "reminder"
let p_slide = "slide"
let p_solution = "solution"
let p_syntax = "syntax"
let p_task = "task"
let p_theorem = "theorem"

(* Treat teach atoms as tail text, thus they will not be loaded as atoms *)
let p_teachask = "xx__teachask__xx"
let p_teachnote = "xx__teachnote__xx"


(* Ilists *)
let p_chooseone = "xchoice"
let p_chooseany = "anychoice"

let p_ilist_separator = p_com_choice | p_com_correct_choice
let p_ilist_separator_arg = (p_com_choice as kind) p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 
                            | (p_com_correct_choice as kind) p_ws  (p_o_sq as o_sq) (p_float as point_val) (p_c_sq as c_sq) 

(* A latex environment consists of alphabethical chars plus an optional star *)
let p_env = (p_alpha)+('*')?

let p_segment = p_chapter |
                p_section |
                p_subsection |
                p_subsubsection |
                p_paragraph 
let p_heading = '\\' p_segment

let p_atom = ((p_diderot_atom as kind) p_ws as kindws) |
             ((p_algorithm as kind) p_ws as kindws) |
             ((p_assumption as kind) p_ws as kindws) |
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
             ((p_reminder as kind) p_ws as kindws) |
             ((p_slide as kind) p_ws as kindws) |
             ((p_solution as kind) p_ws as kindws) |
             ((p_syntax as kind) p_ws as kindws) |
             ((p_task as kind) p_ws as kindws) |
             ((p_teachask as kind) p_ws as kindws) |
             ((p_teachnote as kind) p_ws as kindws) |
             ((p_theorem as kind) p_ws as kindws) 


let p_begin_atom = (p_com_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) 
let p_begin_atom_with_points = (p_com_begin p_ws as b) (p_o_curly as o) p_atom (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_atom = (p_com_end p_ws as e) (p_o_curly as o) p_atom (p_c_curly as c) 

let p_begin_code_atom = (p_com_begin p_ws as b) (p_o_curly as o) ((p_code as kind) p_ws as kindws) (p_c_curly as c) 

let p_end_code_atom = (p_com_end p_ws as e) (p_o_curly as o) ((p_code as kind) p_ws as kindws) (p_c_curly as c) 

let p_ilist_kinds = (p_chooseone | p_chooseany)
let p_ilist = ((p_ilist_kinds as kind) p_ws as kindws) 

let p_begin_ilist = (p_com_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c) 

(* point values now go with atoms.
let p_begin_ilist_arg = (p_com_begin p_ws as b) (p_o_curly as o) p_ilist (p_c_curly as c)  (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
*)
 
let p_end_ilist = (p_com_end p_ws as e) (p_o_curly as o) p_ilist (p_c_curly as c) 

let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env) (p_c_curly) 
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env) (p_c_curly) 


let p_group = ((p_cluster as kind) p_ws as kindws) |
              ((p_flex as kind) p_ws as kindws) |
              ((p_problem_cluster as kind) p_ws as kindws) 

let p_begin_group = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) 
let p_begin_group_with_points = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_group = (p_com_end p_ws as e) (p_o_curly as o) p_group (p_c_curly as c) 

let p_word = [^ '%' '\\' '{' '}' '[' ']']+ 


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

| p_begin_group as x
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ =  set_line_nonempty () in
       KW_BEGIN_GROUP(kind, x, None)
    }		

| (p_begin_group as x) (p_o_sq as a)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ = inc_arg_depth () in
     let (arg, c_sq) = take_opt_arg lexbuf in
     let h = x ^ a ^ arg ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
     let _ =  set_line_nonempty () in
       KW_BEGIN_GROUP(kind, arg, None)
    }		

| p_com_fold as x
    {
(*     let _ = d_printf "!lexer matched fold %s." x in *)
     let _ =  set_line_nonempty () in
       KW_FOLD(x)
    }		

| p_end_group as x
    {
(*     let _ = d_printf "!lexer matched end group %s." kind in *)
     let _ =  set_line_nonempty () in
       KW_END_GROUP(kind, x)
    }		


| p_begin_env as x
      { 
(*          let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
          let _ = do_begin_env () in		
          let (lopt, body, h_e) = take_env lexbuf in
(*          let _ = d_printf "!lexer: latex env matched = %s.\n" (x ^ y) in *)
					let _ =  set_line_nonempty () in
            ENV(None, lopt, body, x ^ body ^ h_e)
          
      }   

| (p_begin_env as x) (p_o_sq as a)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let _ = inc_arg_depth () in
     let (arg, c_sq) = take_opt_arg lexbuf in
     let h_b = x ^ a ^ arg ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
     let _ = do_begin_env () in		
      let (lopt, body, h_e) = take_env lexbuf in
     let _ =  set_line_nonempty () in
            ENV(Some arg, lopt, body, h_b ^ body ^ h_e)
}

| p_label_and_name as x
 		{ 
(*	    let _ = d_printf "!lexer matched %s." x in *)
      let _ =  set_line_nonempty () in
			KW_LABEL_AND_NAME(label_pre ^ label_name ^ label_post, label_name)
		}		

| p_sigchar as x
		{
(*     d_printf "!%s" (char_to_str x); *)
     let _ =  set_line_nonempty () in
     SIGCHAR(char_to_str x)
    }

| p_percent_esc as x 
		{
(*     d_printf "!lexer found: espaced percent char: %s." x; *)
     let _ =  set_line_nonempty () in
     SIGCHAR(x)
    }

| p_percent as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (char_to_str x) in *)
     let comment = take_comment lexbuf in
     let result = (char_to_str x) ^ comment in
(*     let _ = d_printf "!lexer found: comment: %s." result in *)
		 if line_is_empty () then
      (* Drop comments *)
       initial lexbuf
		 else
			 NEWLINE comment
    }

| p_newline as x
		{
(*	   d_printf "!lexer found: newline: %s." x; *)
     let _ =  set_line_empty () in
       NEWLINE(x)
    }

| p_hspace as x
		{
(*     d_printf "!lexer found: horizontal space: %s." (char_to_str x); *)
     HSPACE(char_to_str x)
    }

| eof
		{EOF}
| _
    {initial lexbuf}		
and take_comment = 		
  parse
  | p_newline as x
    { (* let _ = d_printf "take_comment: newline %s" x in *)
     let _ =  set_line_empty () in
        x
    } 
  | _ as x 
    {
     (* let _ = d_printf "take_comment: %s" (char_to_str x) in *)
     let comment = take_comment lexbuf in 
       (char_to_str x) ^ comment
    }
and take_env =
  parse
  | p_begin_verbatim as x
      { 
(*          let _ = d_printf "!lexer: entering verbatim\n" in *)
          let y = verbatim lexbuf in
          let _ = d_printf "!lexer: verbatim matched = %s" (x ^ y) in
          let (lopt, z, h_e) = take_env lexbuf in
            (lopt, x ^ y ^ z, h_e)          
      }   
  | p_begin_env as x
        {
(*            let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
            let _ = do_begin_env () in
            let (lopt, y, h_e) = take_env lexbuf in
                (lopt, x ^ y, h_e)              
        }

  | p_end_env as x
        { 
(*            let _ = d_printf "!lexer: end latex env: %s\n" x in *)
            let do_exit = do_end_env () in
                if do_exit then
(*                    let _ = d_printf "!lexer: exiting latex env\n" in *)
                        ( None, "", x)
                else
                    let (lopt, y, h_e) = take_env lexbuf in
                      (lopt, x ^ y, h_e)  
        }      
  | p_label_and_name as x
  		{ 
(*		    let _ = d_printf "!lexer matched label %s." x in *)
				let all = label_pre ^ label_name ^ label_post in
        let (lopt, y, h_e) = take_env lexbuf in
          (* Important: Drop inner label lopt *)
          (Some label_name, all ^ y, h_e)  
			}		
  (* Important because otherwise lexer will think that it is comment *)
  | p_percent_esc as x 
		{
(*     let _ = d_printf "!lexer found: espaced percent char: %s." x in *)
     let (lopt, y, h_e) = take_env lexbuf in
          (lopt, x ^ y, h_e)
    }
  | p_percent as x   (* skip over comments *)
   	{ 
     let y = take_comment lexbuf in
     let (lopt, z, h_e) = take_env lexbuf in 
          (lopt, (char_to_str x) ^ y ^ z, h_e)
     } 
  | _  as x
        { let (lopt, y, h_e) = take_env lexbuf in
            (lopt, (char_to_str x) ^ y, h_e)
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
            (char_to_str x) ^ y
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
       ((char_to_str x) ^ arg, c_c)
    }
and take_opt_arg = 
  parse 
  | p_o_sq as x
    {
     let _ = inc_arg_depth () in
     let (arg, c_sq) = take_opt_arg lexbuf in 
       (x ^ arg, c_sq)
    }
  | (p_c_sq p_ws) as x
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
       ((char_to_str x) ^ arg, c_sq)
    }


(** BEGIN TRAILER **)
{
(* This is the default lexer *)
let lexer: Lexing.lexbuf -> Atom_parser.token = 
		initial

(* This is the spiced up lexer that modifies the token stream in
 * several ways.
 * First, it maintains a state and implements a state transition 
 * system that allows it to detect the beginning of paragraps.
 *
 * It also emits an additional newline token to force end a paragraph
 * when a heading or EOF is encountered but a paragraph is not 
 * completed.  When an extra token is emitted, the current token
 * is cached and emitted at the next request.
 *)



let lexer: Lexing.lexbuf -> Atom_parser.token =
  let prev_token = ref None in

	let rec take_spaces lexbuf =
		let t = lexer lexbuf in
		match t with
		| HSPACE _ -> take_spaces lexbuf 
		| NEWLINE _ -> 
				let (ntk, m) = take_spaces lexbuf in
				(ntk, m+1)
		| _ -> (t, 0) 
	in
    fun lexbuf ->
(*  		let _ = d_printf "!lexer: state = %s\n" (state_to_string !state) in *)
      let old_state = get_state () in

			let next_token = ref None in
			let set_next_token t = next_token := t in

			let start_par tk = (set_state Busy; next_token := Some tk) in

			let return_tk tk = 
				let tk_ret = 
					match !next_token with 
					| None -> tk
					| Some ntk -> ntk
				in
				(prev_token := Some tk_ret;
(*				 d_printf "returning token: %s\n" (token_to_str tk_ret); *)
				 tk_ret)
			in
			let is_token_from_cache = ref false in
  		let tk = 
				match cache_remove () with 
				| None -> (is_token_from_cache := false; lexer lexbuf)
				| Some t -> (is_token_from_cache := true; t) in
(*			let _ = d_printf "!lexer: handling token: %s\n" (token_to_str tk) in *)
			let _ =
				match tk with 
				| NEWLINE x -> 
(*						let _ = d_printf "\n **token = newline! \n" in *)
(*						let _ = d_printf " **trace = %s! \n" (trace_to_string ()) in *)
						begin
            match get_trace () with 
						| No_space ->
								let _ = set_trace Ver_space in
								  ()
						| Hor_space ->
								let _ = set_trace Ver_space in
								  ()
						| Ver_space ->
								let _ = set_trace Ver_space in
								let _ = set_state Idle in
								if !is_token_from_cache then
									()
(*									d_printf "token is from cache.\n"  *)
								else 
									let (ntk, n) = take_spaces lexbuf in
(*									let _ = d_printf "took %s spaces and next token = %s \n" (string_of_int n) (token_to_str ntk) in *)
									cache_insert ntk
									
						end
				| HSPACE x -> 
(*						let _ = d_printf "** token = hspace %s \n" x in *)
						begin
						match get_trace () with 
						| Ver_space -> set_trace Ver_space
						| _ -> set_trace Hor_space 
						end
				| SIGCHAR x -> 
(*						let _ = d_printf "** token = sigchar %s \n" x in *)
(*						let _ = d_printf "%s" x in *)
						let _ = set_trace No_space in
						begin
						match !state with 
   					| Idle -> start_par (PAR_SIGCHAR x)
						| Busy -> set_state Busy
						end

				| ENV x -> 
(*						let _ = d_printf "** token = env %s \n" (fst x) in  *)
						let _ = set_trace No_space in
						begin
						match !state with 
   					| Idle -> start_par (PAR_ENV x)
						| Busy -> set_state Busy
						end

				| KW_LABEL_AND_NAME x -> 
(*  					let _ = d_printf "** token = label \n" in *)
						let _ = set_trace No_space in
						begin
						match !state with 
   					| Idle -> start_par (PAR_LABEL_AND_NAME x)
						| Busy -> set_state Busy
						end

				| KW_BEGIN_GROUP x ->
(*  					let _ = d_printf "** token = begin group \n"  in *)
            begin
						match !state with 
   					| Idle -> 
								let _ = set_trace No_space in
								set_state Idle
						| Busy -> 
								let _ = cache_insert (NEWLINE "\n") in
								let _ = cache_insert tk in
								let _ = set_next_token (Some (NEWLINE "\n")) in
								let _ = set_trace Ver_space in (* Because we will return a newline *)
								set_state Busy
						end

				| KW_END_GROUP x ->
(*  					let _ = d_printf "** token = end group \n"  in *)
            begin
						match !state with 
   					| Idle -> 
								let _ = set_trace No_space in
								set_state Idle
						| Busy -> 
								let _ = cache_insert (NEWLINE "\n") in
								let _ = cache_insert tk in
								let _ = set_next_token (Some (NEWLINE "\n")) in
								let _ = set_trace Ver_space in (* Because we will return a newline *)
								set_state Busy
						end

				| KW_FOLD x ->
(*  					let _ = d_printf "** token = fold \n"  in *)
            begin
						match !state with 
   					| Idle -> 
								let _ = set_trace No_space in
								set_state Idle
						| Busy -> 
								let _ = cache_insert (NEWLINE "\n") in
								let _ = cache_insert tk in
								let _ = set_next_token (Some (NEWLINE "\n")) in
								let _ = set_trace Ver_space in (* Because we will return a newline *)
								set_state Busy
						end

				| KW_HEADING x ->
(*  					let _ = d_printf "** token = heading \n"  in *)
            begin
						match !state with 
   					| Idle -> 
								let _ = set_trace No_space in
								set_state Idle
						| Busy -> 
								let _ = cache_insert (NEWLINE "\n") in
								let _ = cache_insert tk in
								let _ = set_next_token (Some (NEWLINE "\n")) in
								let _ = set_trace Ver_space in (* Because we will return a newline *)
								set_state Busy
						end
        | EOF -> 
(*						let _ = d_printf "token = EOF \n" in *)
            (* TODO THIS IS NOT ENOUGH 
               WE NEED TWO NEWLINES. *)
						begin
						match !state with 
   					| Idle -> set_state Idle
						| Busy -> 
								let _ = cache_insert (NEWLINE "\n") in
								let _ = cache_insert tk in
								let _ = set_next_token (Some (NEWLINE "\n")) in
								let _ = set_trace Ver_space in (* Because we will return a newline *)
								  set_state Busy
						end

        | _ -> printf "Fatal Error: token match not found!!!\n"
			in  
      let _ = if old_state = Idle && (get_state () = Busy) then
(*        d_printf "!!START PARAGRAPH!!\n" *)
				()
      in 
        return_tk tk

}
(** END TRAILER **)


