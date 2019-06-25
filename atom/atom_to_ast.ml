(* texel.ml 
 * Elaborator tool for LaTex.
 *) 
open Core
open Lexing
open Utils

module Lexer = Atom_lexer
module Parser = Atom_parser

let verbose = ref false

(* Stub. *)
let handle_parser_error () = 
  printf ("Parse Error\n.")

let atom_to_ast input = 
	let _ = d_printf "atom_to_ast input = %s" input in
      let lexbuf = Lexing.from_string input in
	    Parser.top Lexer.lexer lexbuf

