%{
open Core
open Printf
open Utils

module Ast = Ast
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
	      string option *   (* title *)
        string option *   (* label *)
        string *          (* body *)
        ((string * string option * string) list) *   (* items kind, point opt, body *)
        string *           (* all but the items *)
	      string> ATOM       (* all *)

%start top

%type <(string *          (* kind *)
        string option *   (* points *)
	      string option *   (* title *)
        string option *   (* label *)
        string *          (* body *)
        ((string * string option * string) list) *   (* items kind, point opt, body *)
	      string) option>  top  (* all *)

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/
atom:
| a = ATOM
  { let (kind, popt, topt, lopt, body, items, all_but_items, all) = a in
    let _ = d_printf "* atom: %s\n" all in
		a
  }

top:
| a = atom
  EOF
  { let (kind, popt, topt, lopt, body, items, all_but_items, all) = a in
	  let a = (kind, popt, topt, lopt, body, items, all_but_items) in
	  Some a
	}
| a = atom
  b = atom
  { None }
| n = atom
  c = SIGCHAR
  { None }
| c = SIGCHAR
  { None }
