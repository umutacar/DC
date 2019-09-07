# Here is simple lexer for lexing a string and encoding html characters.  It deals with errors.

{
    (* ocamllex scan.mll &&  ocamlc -o scan scan.ml *)

    module L = Lexing 
    module B = Buffer

let get      = L.lexeme
let sprintf  = Printf.sprintf

let position lexbuf =
    let p = lexbuf.L.lex_curr_p in
        sprintf "%s:%d:%d" 
        p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt

}

rule escape b = parse
| '&'       { B.add_string b "&amp;";  escape b lexbuf } 
| '"'       { B.add_string b "&quot;"; escape b lexbuf } 
| '\''      { B.add_string b "&apos;"; escape b lexbuf }
| '>'       { B.add_string b "&gt;";   escape b lexbuf }
| '<'       { B.add_string b "&lt;";   escape b lexbuf }
| [^'&' '"' '\'' '>' '<']+ 
            { B.add_string b @@ get lexbuf
            ; escape b lexbuf
            }
| eof       { let x = B.contents b in B.clear b; x }
| _         { error lexbuf 
                "don't know how to quote: %s" (get lexbuf) }

{
let escape str = escape (B.create 100) (L.from_string str)

let main () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  args |> List.iter (fun str -> escape str |> print_endline)

let () = main () (* call main function on startup *)


* Interesting piece of knowledge

browser address interpretation.
// --> absolute.
/ --> relative to domain.
no slash --> relative to current page.

   
}