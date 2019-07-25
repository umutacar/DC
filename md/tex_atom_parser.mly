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


%token <string option *   (* points *)
	      string option *   (* title *)
        string option *   (* label *)
        string *          (* body *)
        ((string * string option * string) list) *   (* items kind, point opt, body *)
	      string> ATOM       (* all *)


%token <string> NONATOM

%start top

%type <string option> top

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/
atom:
| a = ATOM
  { let (popt, topt, lopt, body, items, all_but_items, all) = a in
    let label = match lopt with | None -> "" | Some l -> l in
    let _ = d_printf "* atom: %s\n" all in
  	  all
  }

nonatom:
| n = NONATOM
  {
	  n
  }

top:
| a = atom
  EOF
  { Some a }
| n = nonatom
  EOF
  { Some n }
