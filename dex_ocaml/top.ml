open Core
open Lexer
open Lexing
open MenhirLib

(* let filename = Sys.argv.(1) *)

let main () =
  let result = Lexer.main () in
    result
let _ = main ()
