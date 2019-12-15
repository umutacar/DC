(********************************************************************** 
 ** atom/atom_parser.mly
 ** This parser does not parse the whole input but just 
 ** checks that the input is a (single standing) atom.
 ** If not, it basically aborts and returns None.
 ** 
 ** This is implemented as follows. 
 ** The lexer returns ATOM tokens for the environments it matches.
 ** The parser requires there to be one atom followed by EOF.
 ** If such a things is found, then the atom is returned.
 ** Otherwise, there are multiple atoms, and if there are other characters
 ** following the atom, the parser returns "None".
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
        ((string * string option * string) list)  (* items kind, point opt, body *)
	      ) option>  top  (* all *)

/*  BEGIN RULES */
%%

/**********************************************************************
 ** BEGIN: Lines, Textparagraphs, and environments
 **********************************************************************/
atom:
| a = ATOM
  { let (kind, popt, kwargs, lopt, body, capopt, items, all) = a in
(*    let _ = d_printf "* atom_parser, atom body = %s\n" body in *)
		(kind, popt, kwargs, lopt, body, capopt, items) 
  }

top:
| a = atom
  EOF
  {
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
