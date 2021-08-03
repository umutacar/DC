(********************************************************************** 
 ** tex/tex_lexer.mll
 **********************************************************************)

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

(** BEGIN: State Machine **)
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

(** END State Machine **)


(** BEGIN: Token Cache **)
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

(** END: Token Cache **)

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
	| KW_HEADING (x, _, _, _) -> "* lexer: token = heading: " ^ x
  | EOF -> "* lexer: token = EOF.";
  | _ ->  "Fatal Error: token match not found!!!"

let token_to_dbg_str tk =
	match tk with 
	| NEWLINE x -> x
	| HSPACE x ->  x
	| PAR_CHUNK (x, lopt) -> x
	| CHUNK (x, lopt) ->  x
	| KW_BEGIN_GROUP (kind, arg, _) -> sprintf "\\begin{%s}" kind 
	| KW_END_GROUP (kind, _) -> sprintf "\\end{%s}" kind 
	| KW_FOLD (x) -> x
	| KW_HEADING (kind, title, _, _) -> sprintf "\\%s{%s}" kind title 
  | EOF -> "EOF\n"
  | _ ->  "Fatal Error: token match not found!!!"

let mk_infer_arg (opener, body, width, closer) = 
  let rec mk_columns w = 
    let column = "l@{\\qquad}" in
    if w = 0 then column
    else column ^ (mk_columns (w - 1))
  in 
  let columns = mk_columns width in
  let result = 
    opener ^
		"\\begin{array}" ^ "{" ^ columns ^ "}" ^ "\n" ^ 
    body ^ 
    "\\end{array}" ^ "\n" ^ 
    closer 
  in
  let _ = d_printf "mk_infer_arg: result = %s \n" result in
  result

let mk_infer (h, opt, a, b) = 
  match opt with 
  | None -> h ^ a ^ b 
  | Some opt -> 
    let box = "\\mbox" ^ "{" ^ opt ^ "}" in
    h ^ a ^ b ^ "\\quad" ^ box 


let fmt_attach = 
	Printf.sprintf "\\begin{verbatim}\n%%%%%%%% diderot_html\n<a href = '%s' data-diderot='__diderot_attachment__'> %s </a>\n\\end{verbatim}"

let fmt_download = 
	Printf.sprintf "\\begin{verbatim}\n%%%%%%%% diderot_html\n<a href = '%s' download = '%s' data-diderot='__diderot_download__'> %s </a>\n\\end{verbatim}"

let fmt_video = 
  Printf.sprintf "\\begin{verbatim}\n%%%%%%%% diderot_html\n<div class='video-container' style='margin-bottom:15px'><iframe class='ql-video' frameborder='0' allowfullscreen='true' src='%s'>%s</iframe></div>\n\\end{verbatim}"


(* DEPRACATED.  See below!
 * This is a minimal html escaping function that only espaces
 * & --> &amp;
 * < --> &lt;
 * > --> &gt;
 * I am not sure that it is enough.
 *)
let escape_html url = 
  let r_amp = Str.regexp "&" in
  let html_amp = "&amp;" in
  let r_angle_open = Str.regexp "<" in 
  let html_angle_open = "&lt;" in
  let r_angle_close = Str.regexp ">" in 
  let html_angle_close = "&gt;" in
  let url = Str.global_replace r_amp html_amp url in
  let url = Str.global_replace r_angle_open html_angle_open url in
  let url = Str.global_replace r_angle_close html_angle_close url in
    url

let escape_html url = 
  let encode = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~prefer_name:true () in
  let r = encode url in
  let _ = d_printf "escape_html: input url = %s \n encoded url = %s\n" url r in
    r 

(* Command rewriter *)
let diderot_com_create (kind, arg, text) = 
	let _ = d_printf "diderot_com_create: kind = %s text = %s arg = %s\n" kind text arg in
  (* HTML encode arg *)
  let arg = escape_html arg in
  if kind = "attach" then
    let body = fmt_attach arg text in
    body
  else if kind = "download" then
    let body = fmt_download arg arg text in
    body
  else if kind = "video" then
    (* encode url into proper ascii*)
    let body = fmt_video arg text in
    body
  else
		(printf "Fatal Error. Lexer: Diderot Command could not be %s. Exiting! \n" kind ;
     exit(1))

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
let p_ws_hard = [' ' '\t' '\n' '\r']+	
let p_skip = p_hs
let p_par_break = '\n' p_ws '\n' p_hs

let p_digit = ['0'-'9']
let p_integer = ['0'-'9']+
let p_frac = '.' p_digit*
let p_exp = ['e' 'E'] ['-' '+']? p_digit+
let p_float = p_digit* p_frac? p_exp?

let p_alpha = ['a'-'z' 'A'-'Z']
let p_alpha_num = ['a'-'z' 'A'-'Z' '0'-'9']
let p_separator = [':' '.' '-' '_' '/']

let p_alpha_nums = p_alpha_num*

(* No white space after backslash *)
let p_backslash = '\\'
let p_double_backslash = '\\' '\\' 
let p_quad_backslash = '\\' '\\' '\\' '\\'
let p_o_curly = '{' p_ws
(* don't take newline with close *)
let p_c_curly = '}' p_hs
let p_o_sq = '[' p_ws
let p_c_sq = ']' p_hs											

let p_strategy = "ur" | "all"

(* Examples: "50." "50.0" "50.0.ur" "50.ur" ".ur" *)
(*let p_point_val =  *)
let p_seg_attr = 
  (p_o_sq as o_sq)
  (p_integer as point_val)? p_ws '.' 
  ('0' | "0." (p_strategy as strategy) | (p_strategy as strategy))? 
  p_ws (p_c_sq as c_sq)

(* Examples: "50." "50.0" "50.0.ur" "50.ur" ".ur" *)
(*let p_point_val =  *)
let p_err_seg_attr = 
  (p_o_sq as o_sq)
  (p_alpha_nums as point_val) p_ws '.' 
  (p_alpha_nums as strategy)
  p_ws (p_c_sq as c_sq)

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
    
(* Diderot commands *)
let p_com_attach = "\\" ("attach" as kind) p_ws
let p_com_download = "\\" ("download" as kind) p_ws
let p_com_video = "\\" ("video" as kind) p_ws
let p_com_diderot = p_com_attach | p_com_download | p_com_video

let p_label_name = (p_alpha | p_digit | p_separator)*
let p_label_and_name = (('\\' "label" p_ws  p_o_curly) as label_pre) (p_label_name as label_name) ((p_ws p_c_curly) as label_post)							

let p_kw_chapter = ("chapter" as kind) ['*']? 
let p_chapter = p_kw_chapter
let p_chapter_with_attr = p_kw_chapter p_ws p_seg_attr
let p_err_chapter_with_attr = p_kw_chapter p_ws p_err_seg_attr

let p_kw_section = ("section" as kind) ['*']? 
let p_section = p_kw_section p_ws
let p_section_with_attr = p_kw_section p_ws p_seg_attr
let p_err_section_with_attr = p_kw_section p_ws p_err_seg_attr

let p_kw_subsection = ("subsection" as kind) ['*']? 
let p_subsection = p_kw_subsection p_ws 
let p_subsection_with_attr = p_kw_subsection p_ws p_seg_attr
let p_err_subsection_with_attr = p_kw_subsection p_ws p_err_seg_attr

let p_kw_subsubsection = ("subsubsection" as kind) ['*']? 
let p_subsubsection = p_kw_subsubsection p_ws 
let p_subsubsection_with_attr = p_kw_subsubsection p_ws p_seg_attr
let p_err_subsubsection_with_attr = p_kw_subsubsection p_ws p_err_seg_attr

let p_paragraph = ("paragraph" as kind) p_ws												
let p_paragraph_with_attr = ("paragraph" as kind) p_ws p_seg_attr
let p_err_paragraph_with_attr = ("paragraph" as kind) p_ws p_err_seg_attr

let p_cluster = "cluster"
let p_flex = "flex"
let p_cflex = "cflex"
let p_problem_cluster = "mproblem"

(* Latex environment: alphabethical chars plus an optional star *)
let p_env = (p_alpha)+('*')?
let p_env_lstlisting = "lstlisting"
let p_env_verbatim = "verbatim"
let p_env_run_star = "run" p_alpha*

(* These environments will be rewritten so that they are inside math environment * \[ .. \] so that they can be passed to mathjax.
 *)
let p_env_align = "align"
let p_env_align_star = "align*"
let p_env_alignat = "alignat"
let p_env_alignat_star = "alignat*"
let p_env_equation = "equation"
let p_env_equation_star = "equation*"
let p_env_math = 
	p_env_align | p_env_align_star |
 	p_env_alignat | p_env_alignat_star |
 	p_env_equation | p_env_equation_star 


let p_begin_env = (p_com_begin p_ws) (p_o_curly) (p_env) p_ws (p_c_curly) 
let p_end_env = (p_com_end p_ws) (p_o_curly) (p_env as kind) (p_c_curly) 

let p_begin_env_math = (p_com_begin p_ws) (p_o_curly) (p_env_math) p_ws (p_c_curly) 
let p_end_env_math = (p_com_end p_ws) (p_o_curly) (p_env_math) p_ws (p_c_curly) 


let p_begin_env_lstlisting = (p_com_begin p_ws) (p_o_curly) (p_env_lstlisting as kind) p_ws (p_c_curly) 
let p_end_env_lstlisting = (p_com_end p_ws) (p_o_curly) (p_env_lstlisting) (p_c_curly)
let p_begin_env_run_star = (p_com_begin p_ws) (p_o_curly) (p_env_run_star as kind) p_ws (p_c_curly) 
let p_end_env_run_star = (p_com_end p_ws) (p_o_curly) (p_env_run_star) (p_c_curly)

let p_begin_env_verbatim = p_com_begin p_ws p_o_curly p_ws (p_env_verbatim as kind) p_ws p_c_curly
let p_end_env_verbatim = p_com_end p_ws p_o_curly p_ws p_env_verbatim p_ws p_c_curly

let p_begin_env_skip = p_begin_env_lstlisting | p_begin_env_run_star | p_begin_env_verbatim

(* end: environments *)


(* Segments *)
let p_segment = 
	p_chapter |
  p_section |
  p_subsection |
  p_subsubsection |
  p_paragraph  

let p_segment_with_attr =
	p_chapter_with_attr | 
  p_section_with_attr | 
  p_subsection_with_attr | 
  p_subsubsection_with_attr | 
  p_paragraph_with_attr

let p_err_segment_with_attr =
	p_err_chapter_with_attr | 
  p_err_section_with_attr | 
  p_err_subsection_with_attr | 
  p_err_subsubsection_with_attr | 
  p_err_paragraph_with_attr

(* Headings *)
let p_heading = '\\' p_segment
let p_heading_with_attr = '\\' p_segment_with_attr
let p_err_heading_with_attr = '\\' p_err_segment_with_attr

let p_group = ((p_cluster as kind) p_ws as kindws) |
              ((p_flex as kind) p_ws as kindws) |
              ((p_cflex as kind) p_ws as kindws) |
              ((p_problem_cluster as kind) p_ws as kindws) 

(* Groups *)
let p_begin_group = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) 
let p_begin_group_with_attr = (p_com_begin p_ws as b) (p_o_curly as o) p_group (p_c_curly as c) (p_o_sq as o_sq) (p_integer as point_val) (p_c_sq as c_sq)
let p_end_group = (p_com_end p_ws as e) (p_o_curly as o) p_group (p_c_curly as c) 


(** END PATTERNS *)			
(* Takes is_empty, the emptiness status of the current line *)
rule initial = 
parse
| (p_heading as x) (p_o_curly as o_c)
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
     let (arg, c_c) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
     let h = x ^ o_c ^ arg ^ c_c in
(*     let _ = d_printf "!lexer matched segment all: %s." h in *)
       KW_HEADING(kind, arg, None, None)
    }		

| (p_heading_with_attr as x) (p_o_curly as o_c)
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
     let (arg, c_c) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
(*     let h = x ^ o_c ^ arg ^ c_c in *)
(*     let _ = d_printf "!lexer matched segment all: %s." h in *)
(* 
        let _ = d_printf "!lexer matched segment all: %s, points = %s, strategy=%s.\n" 
                         kind 
                         (Utils.str_of_str_opt point_val) 
                         (Utils.str_of_str_opt strategy) 
        in 
*)
				KW_HEADING(kind, arg, point_val, strategy)
    }		

| (p_err_heading_with_attr as x)
    {
(*     let _ = d_printf "!lexer matched segment: %s." kind in *)
        let _ = printf "!lexer matched segment all: %s, points = %s, strategy=%s.\n" 
                         kind 
                         point_val
                         strategy 
        in
     	  let err = sprintf "Syntax Error: heading %s has an ill-formed descriptor %s" kind x in
        let _ = printf "%s\n" err in
		    raise (Constants.Syntax_Error err)
    }		

| (p_begin_group as x) (p_o_sq as a)
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let h = x ^ a ^ arg ^ c_sq in
(*     let _ = d_printf "!lexer matched group all: %s." h in  *)
       KW_BEGIN_GROUP(kind, arg, None)
    }		

| p_begin_group as x
    {
(*     let _ = d_printf "!lexer matched begin group %s." kind in *)
       KW_BEGIN_GROUP(kind, x, None)
    }		

| p_com_fold as x
    {
(*     let _ = d_printf "!lexer matched fold %s." x in *)
       KW_FOLD(x)
    }		

| p_end_group as x
    {
(*     let _ = d_printf "!lexer matched end group %s." kind in *)
       KW_END_GROUP(kind, x)
    }		

| (p_begin_env_skip as x)
    {
     let _ = d_printf "!lexer matched begin skip env kind = %s." kind in 
     let keep_kind = kind in
     let (rest, h_e) = skip_env kind lexbuf in
   	 let all = x ^ rest ^ h_e in
		 let _ = d_printf "!lexer matched skip env: %s.\n" all in  
		 CHUNK(all, None)
}

| (p_begin_env_math as x)
    {
     let rest = take_env 1 lexbuf in
   	 let all = x ^ rest in
     let all = kw_math_open ^ all ^ kw_math_close in
     let _ = d_printf "!lexer matched env %s.\n" all in 
     CHUNK(all, None)
}

| (p_begin_env as x)
    {
     let rest = take_env 1 lexbuf in
   	 let all = x ^ rest in
     let _ = d_printf "!lexer matched env %s.\n" all in 
     CHUNK(all, None)
}

| p_label_and_name as x
 		{ 
(*	    let _ = d_printf "!lexer matched %s." x in *)
			let all = label_pre ^ label_name ^ label_post in
(*			KW_LABEL_AND_NAME(label_pre ^ label_name ^ label_post, label_name) *)
			CHUNK (all, Some label_name)
		}		

| p_com_skip p_ws p_o_sq as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let h = x ^ arg ^ c_sq in
     let body = skip_inline lexbuf in
     let all = h ^ body in
		   CHUNK(all, None)
    }

| p_com_skip as x 
		{
(*     let _ = printf "!lexer found: p_com_skip %s." (str_of_char x) in  *)
     let body = skip_inline lexbuf in
     let all = x ^ body in
		   CHUNK(all, None)
    }

| (p_com_diderot as x)
		{
     let arg = take_arg_force lexbuf in
     let text = take_arg_force lexbuf in
		 let _ = d_printf "diderot_com: %s %s %s" kind arg text in
     let command = diderot_com_create (kind, arg, text) in
		 CHUNK(command, None)
    }

| p_special_char as x 
		{
(*     d_printf "!lexer found: espaced char: %s." x; *)
     CHUNK(x, None)
    }

(* No need to treat [ .. ] as a chunk. *)
(* | p_o_sq as x *)
(*   { *)
(*      let (arg, c_c) = take_arg 1 kw_sq_open kw_sq_close lexbuf in *)
(*      let all = x ^ arg ^ c_c in *)
(*      let _ = d_printf "!lexer matched square-bracket chunk:\n%s.\n" all in  *)
(*        CHUNK(all, None) *)
(*   } *)

| p_o_curly as x
  {
     let (arg, c_c) = take_arg 1 kw_curly_open kw_curly_close lexbuf in
     let all = x ^ arg ^ c_c in
     let _ = d_printf "!lexer matched curly-bracket chunk:\n%s.\n" all in 
       CHUNK(all, None)

  }
| p_sigchar as x
		{
     d_printf "!%s" (str_of_char x); 
     CHUNK(str_of_char x, None)
    }

| p_newline as x
		{
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

and take_env depth =  (* not a skip environment, because we have to ignore nested skip environments/commands *)
  parse
  | p_begin_env_skip as x
      { 
(*          let _ = d_printf "!lexer: entering verbatim\n" in *)
       let (v_body, v_e) = skip_env kind lexbuf in
       let v = x ^ v_body ^ v_e in
(*       let _ = d_printf "!lexer: env skip matched = %s" v in *)
       let rest = take_env depth lexbuf in
       (v ^ rest)
      }   

  | p_com_lstinline  p_ws p_o_sq  as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let (arg, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let body = skip_inline lexbuf in
     let lst = x ^ arg ^ c_sq ^ body in
     let rest = take_env depth lexbuf in
		 lst ^ rest
    }

  | p_com_lstinline as x 
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let body = skip_inline lexbuf in
     let rest = take_env depth lexbuf in
     x ^ body ^ rest
    }

  | (p_com_diderot as x)
		{
     let arg = take_arg_force lexbuf in
     let text = take_arg_force lexbuf in
		 let _ = d_printf "diderot_com: %s %s %s" kind arg text in
     let rest = take_env depth lexbuf in
     let command = diderot_com_create (kind, arg, text) in
		 command ^ rest
    }

  | (p_com_infer as h) (p_o_sq as x) 
		{
     let _ = d_printf "!lexer found: infer with opt %s." x in 
     let (opt, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let a = take_arg_infer lexbuf in
     let b = take_arg_infer lexbuf in
     let i = mk_infer (h, Some opt, a, b) in 
     let rest = take_env depth lexbuf in
     i ^ rest
    }

  | p_com_infer as x 
		{
     let _ = d_printf "!lexer found: infer %s." x in 
     let a = take_arg_infer lexbuf in
     let b = take_arg_infer lexbuf in
     let rest = take_env depth lexbuf in
     x ^ a ^ b ^ rest
    }

  (* Rewrite \asktf --> \onechoice *)
  | (p_com_ask_true_false as h) 
		{
     let _ = d_printf "!lexer found: true-false-prompt." in 
     let rest = take_env depth lexbuf in
     kw_one_choice ^ rest
    }

  (* Rewrite \solt --> \choice* True \choice False *)
  | (p_com_sol_true as h) (p_ws_hard as ws)
		{
     let _ = d_printf "!lexer found: solution prompt, solution=true." in 
     let rest = take_env depth lexbuf in
     kw_sol_true ^ ws ^ rest
    }

  (* Rewrite \solf --> \choice True \choice* False *)
  | (p_com_sol_false as h) (p_ws_hard as ws)
		{
     let _ = d_printf "!lexer found: solution prompt, solution=true." in 
     let rest = take_env depth lexbuf in
     kw_sol_false ^ ws ^ rest
    }

  | p_begin_env_math as x
    {
(*            let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
     let rest = take_env (depth+1) lexbuf in            
     (* Wrap with math.  This is ok, because these are never nested. *)
     kw_math_open ^ x ^ rest
    }

  | p_begin_env as x
    {
(*            let _ = d_printf "!lexer: begin latex env: %s\n" x in *)
     let rest = take_env (depth+1) lexbuf in
     x ^ rest
    }

  | p_end_env_math as x
    { 
(*            let _ = d_printf "!lexer: end latex env: %s\n" x in *)
     let depth = depth - 1 in
     if depth = 0 then
(*                    let _ = d_printf "!lexer: exiting latex env\n" in *)
       x ^ kw_math_close
     else
       let rest = take_env depth lexbuf in
       x ^ kw_math_close ^ rest
    }      

  | p_end_env as x
    { 
(*            let _ = d_printf "!lexer: end latex env: %s\n" x in *)
     let depth = depth - 1 in
       if depth = 0 then
(*                   let _ = d_printf "!lexer: exiting latex env\n" in *)
         x
       else
         let rest = take_env depth lexbuf in
         x ^ rest
    }      

	| (p_com_caption p_ws p_o_sq) as x
		{
     let (title, c_c) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let body = take_arg_force lexbuf in
     (* Drop short title, used in some latex packages for titling the figure *)
     let caption = "\\caption" ^ kw_curly_open ^ body ^ kw_curly_close in
     let _ = d_printf "!tex_lexer matched caption: title = %s \n %s." title caption  in
     
		 let rest = take_env depth lexbuf in
     (* Drop capopt_, it would be another caption. *)
     caption ^ rest
    }
  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning a LaTeX environment.  An environment was not ended perhaps?" in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 
  | _  as x
    {let rest = take_env depth lexbuf in
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
  | eof  
      {
    	 let err = "Syntax Error: File ended unexpectedly while scanning a skip-command such as a \\verb or \\lstinline." in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
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
  | eof 
      {
    	 let err = sprintf "Syntax Error: File ended while searching for: \\end{%s}." stop_kind in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
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

and take_arg_infer = 
  parse
  | p_ws as x   
    {
       take_arg_infer lexbuf 
    }
  | p_o_curly as x
    { let (a, w, widths, y) = take_arg_array lexbuf  in
      let Some max_width = reduce  (fun x y -> if x > y then x else y) (w::widths) in
      let i = mk_infer_arg (x, a, max_width, y) in
      i
    }
  | eof 
      {
    	 let err = "Syntax Error: File ended while searching for: }." in
       let _ = printf "%s\n" err in
		   raise (Constants.Syntax_Error err)
      } 


and take_arg_array =  
  (* Take argument of the form { arg_1 &[\\] arg_2 &[\\] arg_3 \\\\ ...  }, where
   * arg_i may contain many instances of \infer{arg_11 & arg ... & ... \\ }{ .. & ... & }.
   * Rewrite each nested as \infer{\begin{array}{lll...l} ... \end{array}
   *)
  parse
  | p_quad_backslash as x (* This is quad backslash \\\\, espaced *)
    {
      let (rest, width, widths, c_c) = take_arg_array lexbuf in
      (* Rewrite as newline for the array environment being created *)
      ("\\\\" ^ rest, 0, width::widths, c_c)
    }

  | p_double_backslash as x  (* This is double backslash \\, espaced *)
    {
      let (rest, width, widths, c_c) = take_arg_array lexbuf in
      ("&" ^ rest, width+1, widths, c_c)
    }

  | '&' as x
    {
      let (rest, width, widths, c_c) = take_arg_array lexbuf in
      ("&" ^ rest, width+1, widths, c_c)
    }

  | (p_com_infer as h) (p_o_sq as x) 
		{
     let _ = d_printf "!lexer found: infer with opt %s." x in 
     let (opt, c_sq) = take_arg 1 kw_sq_open kw_sq_close lexbuf in
     let a = take_arg_infer lexbuf in
     let b = take_arg_infer lexbuf in
     let i = mk_infer (h, Some opt, a, b) in 
     let (rest, width, widths, c_c) = take_arg_array lexbuf in
     (i ^ rest, width, widths, c_c)          
    }

	| p_com_infer as x
		{
(*     let _ = d_printf "!lexer found: percent char: %s." (str_of_char x) in *)
     let a = take_arg_infer lexbuf  in
     let b = take_arg_infer lexbuf  in
     let i = mk_infer (x, None, a, b) in 
     let (rest, width, widths, c_c) = take_arg_array lexbuf in
     (i ^ rest, width, widths, c_c)          
    }

  | p_c_curly as x
    {("", 0, [], x) }

  | p_o_curly as x 
    { let (arg, c_c_a) = take_arg 1 "{" "}" lexbuf in 
      let (rest, width, widths, cc_i) = take_arg_array lexbuf in
 		  (x ^ arg ^ c_c_a ^ rest, width, widths, cc_i)
    }

  | _ as x 
  	{
     let x = str_of_char x in
     let (rest, width, widths, c_c) = take_arg_array lexbuf in
 		 (x ^ rest, width, widths, c_c)		 
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

