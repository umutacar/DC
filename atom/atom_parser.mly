(********************************************************************** 
 ** atom/atom_parser.mly
 **********************************************************************)
%{
open Core
open Printf
open Utils

module Tex = Tex_syntax

(* Turn off prints *)
(*
let d_printf args = 
    ifprintf stdout args
*)
let parse_error s = printf "Parse Error: %s"

let mk_point_val_f_opt (s: string option) = 
  match s with
  | None -> (None, "None")
  | Some x -> (Some (float_of_string x), "Some " ^ x)


let str_of_items items = 
	str_of_str2_list items

%}	

%token EOF

%token <string> SIGCHAR
%token <string *          (* kind *)
        string option *   (* points *)
	      (string * string) list *   (* keyword arguments *)
        string option *   (* label *)
        string *          (* body *)
        string option *   (* caption: body option *)
        ((string * string option * string) list) *   (* items kind, point opt, body *)
	      string> ATOM       (* all *)

%start top

%type <(string *          (* kind *)
        string option *   (* points *)
	      (string * string) list *   (*  keyword arguments *)
        string option *   (* label *)
        string *          (* body *)
        string option *   (* caption *)
        ((string * string option * string) list) *  (* items kind, point opt, body *)
        string            (* all *)
	      , string) result>  top  (* all *)

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/
atom:
| a = ATOM
  { let (kind, popt, kwargs, lopt, body, capopt, items, all) = a in
    let _ = d_printf "* atom_parser, atom body = %s\n" body in 
		(kind, popt, kwargs, lopt, body, capopt, items, all) 
  }

any:
| a = ATOM
  { let (kind, popt, kwargs, lopt, body, capopt, items, all) = a in
      all
  }
| c = SIGCHAR
  { c } 

many: 
| a = any
  { a } 
| m = many
  a = any
  { m ^ a}
   
top:
| a = atom
  EOF 
  {let (kind, popt, kwargs, lopt, body, capopt, items, all) = a in
	 Ok a
	}
| a = atom
  m = many
  { let (kind, popt, kwargs, lopt, body, capopt, items, all) = a in
			Error (all ^ m) 
  }

| c = SIGCHAR
  { Error c }


| c = SIGCHAR
  m = many
  { Error (c ^ m) }
