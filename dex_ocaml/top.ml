open Core
open Lexer
open Lexing
open MenhirLib

let filename = Sys.argv.(1)

let main () =
  let lines = In_channel.read_lines filename in
    ()  
let _ = main ()
