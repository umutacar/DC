open Core
open Lexer
open Lexing

let filename = Sys.argv.(1)

let main () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    print_endline (Pp.pp_blocks (Parser.blocks Lexer.read lexbuf))
  with
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf);
      Printf.eprintf "Unexpected: %s\n" (Lexing.lexeme lexbuf);
  In_channel.close inx

  let _ = main ()
