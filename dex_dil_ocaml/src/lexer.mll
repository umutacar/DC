let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let b = newline? white? "\\begin" white? "{"
let e = newline? white? "\\end" white? "{"
let rb = white? "}" newline?

let chap = "chapter"
let sect = "section"
let sub = "subsection"
let subsub = "subsubsection"
let grp = "group"
let grm = "gram"
let dfn = "definition"
let exmpl = "example"

rule read =
  parse
  | b chap rb     { Parser.B_CHAP }
  | e chap rb     { Parser.E_CHAP }
  | b sect rb     { Parser.B_SECT }
  | e sect rb     { Parser.E_SECT }
  | b sub rb      { Parser.B_SUB }
  | e sub rb      { Parser.E_SUB }
  | b subsub rb   { Parser.B_SUBSUB }
  | e subsub rb   { Parser.E_SUBSUB }
  | b grp rb      { Parser.B_GRP }
  | e grp rb      { Parser.E_GRP }
  | b grm rb      { Parser.B_GRM }
  | e grm rb      { Parser.E_GRM }
  | b dfn rb      { Parser.B_DFN }
  | e dfn rb      { Parser.E_DFN }
  | b exmpl rb    { Parser.B_EXMPL }
  | e exmpl rb    { Parser.E_EXMPL }
  | '\n'          { read lexbuf }
  | _             { let b = Buffer.create 80 in Buffer.add_string b (Lexing.lexeme lexbuf); read_text b lexbuf }
  | eof           { Parser.EOF }

and read_text buf =
  parse
  | '\n'            { Parser.TEXT (Buffer.contents buf) }
  | [^ '\n']*
                    { Buffer.add_string buf (Lexing.lexeme lexbuf);
                      read_text buf lexbuf }
  | eof             { Parser.TEXT (Buffer.contents buf) }
