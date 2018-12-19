open Core
open Lexer
open Lexing
open MenhirLib

let filename = Sys.argv.(1)

let main () =
  let lines = In_channel.read_lines filename in
  try
    let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.blocks in
    print_endline (Pp.pp_blocks (parser (Lexer.lex lines)))
  with
  | Parser.Error ->
      Printf.eprintf "Syntax error."

  let _ = main ()
